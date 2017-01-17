package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import java.io.{PrintWriter, StringWriter}

import akka.actor.ActorSystem
import akka.dispatch.Envelope

import akka.stream._
import akka.stream.scaladsl._
import akka.util.Timeout
import com.github.tototoshi.csv.CSVReader
import com.google.inject.Inject
import models._
import play.api.cache.CacheApi
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsString, JsValue, Json}
import play.api.mvc._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.control.NonFatal


class CSVController @Inject() (val cache: CacheApi) extends Controller with Security with Logging {

  import scala.concurrent.ExecutionContext.Implicits.global
  import play.api.Play.current

  implicit val system = Akka.system


  val decider: Supervision.Decider = {
    case NonFatal(th) =>


      val sw = new StringWriter
      th.printStackTrace(new PrintWriter(sw))
      Console.println(sw.toString)
      //Supervision.Resume
      Supervision.Stop

    case _ => Supervision.Stop
  }

  implicit val materializer = ActorMaterializer()

  implicit val timeout = Timeout(5 seconds)
  val listCompute = new CSVlistCompute

  def upload = Action.async(parse.multipartFormData) { implicit request =>
    request.body.file("attachment").map { upload =>
      import java.io.File
      val filename = upload.filename
      val uploadedFile = upload.ref.moveTo(new File(s"/tmp/upload/$filename"))
      val reader = CSVReader.open(uploadedFile)
      Future(Ok("OK"))
      Source.single(CSVcompute(reader.all)).map(_.buildingJsonList).mapConcat { seq =>
        seq.to[scala.collection.immutable.Iterable]
      }.mapAsync(1) { json =>
        listCompute.getMetrics(json)
      }.runFold(Seq.empty[Option[String]]) { case (l, r) =>
        l :+ r
      }.map { r =>
        Ok(r.flatten.mkString(","))
      }.recover {
        case NonFatal(th) => Ok("Failed")
      }
    }.getOrElse {
      Future {
        Ok("File is missing")
      }
    }


  }
}

