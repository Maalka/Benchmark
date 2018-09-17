/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import models._
import com.google.inject.Inject
import play.api.cache.AsyncCacheApi
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import _root_.util.Logging
import akka.actor.ActorSystem



class DegreeDaysController @Inject() (
                                       val cache: AsyncCacheApi,
                                       cc: ControllerComponents
                                     )(implicit val actorSystem: ActorSystem) extends AbstractController(cc) with Logging {
  this: AbstractController =>

  implicit def doubleToJSValue(d:Double):JsValue = Json.toJson(d)

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T):Either[String, JsValue] = {
    response match {
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: String => Right(Json.toJson(v))
      case None => Left("Could not recognize input type")
    }
  }

  def getDDMetrics() = Action.async(parse.json) { implicit request =>

    val DD: DegreeDays = DegreeDays(request.body)

    val futures = Future.sequence(Seq(

      DD.lookupWeatherStation.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      DD.lookupCDD.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      DD.lookupHDD.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}

    ))

    val fieldNames = Seq(
      "weatherStation",
      "CDD",
      "HDD"
    )

    futures.map(fieldNames.zip(_)).map { r =>
      val errors = r.collect {
        case (n, Left(s)) => Json.obj(n -> s)
      }
      val results = r.collect {
        case (n, Right(s)) => Json.obj(n -> s)
      }
      Ok(Json.obj(
        "values" -> results,
        "errors" -> errors
      ))
    }

  }
}

