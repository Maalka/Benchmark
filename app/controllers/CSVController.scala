package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import akka.dispatch.Envelope
import com.github.tototoshi.csv.CSVReader
import com.google.inject.Inject
import models._
import play.api.cache.CacheApi
import play.api.libs.json.{JsString, JsValue, Json}
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions


trait CSVActions {
  this: Controller =>

  def upload = Action.async(parse.multipartFormData) {implicit request =>

    val form:JsValue = {
      val tempForm = request.body.dataParts.flatMap {
        case (key,value) => value.headOption
      }
      Json.parse(tempForm.head)
    }

    request.body.file("attachment").map { upload =>
      Future {
        import java.io.File
        val filename = upload.filename
        val uploadedFile = upload.ref.moveTo(new File(s"/tmp/upload/$filename"))

        Ok("File uploaded") }
    }.getOrElse {
      Future{
        Ok("File is missing")
      }
    }




  }
}
class CSVController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with CSVActions