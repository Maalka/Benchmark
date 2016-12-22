package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import models._
import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.mvc._

import scala.concurrent.Future
import play.api.libs.concurrent.Promise
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

trait CSVActions {
  this: Controller =>

  def upload = Action.async(parse.multipartFormData) {implicit request =>

    //println(request.body.file("attachment"))
    //val csvOutput: CSVcompute = CSVcompute(request.body.file("attachment").get)


    Future{Ok("File has been uploaded2")}
  }
}
class CSVController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with CSVActions