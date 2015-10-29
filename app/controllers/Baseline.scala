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

    getBaseline.predictedEUI match {
      case JsSuccess(a, _) => Console.println("Expected Predicted EUI is: " + a)
      case JsError(err) => Console.println(err)
    }
    getBaseline.sourceEUI match {
      case JsSuccess(a, _) => Console.println("Actual Source EUI is: " + a)
      case JsError(err) => Console.println(err)
    }

    Console.println("EUI Ratio is: " + getBaseline.euiRatio)

/*    getBaseline.ES.map(_ match {
      case Some(a) => Console.println("Calculated ES: " + a)
      case None => Console.println("Could not calculate ES")
    }
    )*/
    Future(Ok("Ok"))

  }

}





