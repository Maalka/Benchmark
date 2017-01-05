/**
 * Created by rimukas on 10/12/15.
 */


package controllers
import models._

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import squants.energy.{Energy}

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: Controller =>

  implicit def doubleToJSValue(d:Double):JsValue = Json.toJson(d)
  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)
  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map{
    case e:Energy => e.value
  })

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T):Either[String, JsValue] = {

    response match {
      case v: Energy => Right(v)
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: String => Right(Json.toJson(v))
      case None => Left("Could not recognize input type")
    }
  }




  def normalize() = Action.async(parse.json) { implicit request =>

    val normalize = new NormalizedWeather(request.body)

    Future{Ok("File has been uploaded")}


  }
}
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions