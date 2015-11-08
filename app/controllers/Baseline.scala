/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future

import models.EUIMetrics

import scala.concurrent.ExecutionContext.Implicits.global

class Baseline(val cache: CacheApi) extends Controller with Security with Logging{

  def makeBaseline() = Action.async(parse.json) { implicit request =>

    val getBaseline:EUIMetrics = EUIMetrics(request.body)

    getBaseline.ExpectedEUI.map(println)
    getBaseline.sourceEUI.map(println)
    getBaseline.ES.map(println)
    getBaseline.targetEUI.map(println)



    Future(Ok("Ok"))

  }

}





