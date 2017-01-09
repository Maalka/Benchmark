
package controllers
import models._

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.mvc._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: Controller =>



  def normalize() = Action.async(parse.json) { implicit request =>


    Future{Ok("File has been uploaded")}


  }
}
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions