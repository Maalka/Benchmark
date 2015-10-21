/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future

import models.Building

import scala.concurrent.ExecutionContext.Implicits.global

class Baseline(val cache: CacheApi) extends Controller with Security with Logging{

  def makeBaseline() = Action.async(parse.json) { implicit request =>

    Building.getExpectedEnergy(request.body) match {
      case JsSuccess(score,_) => {
        Console.println(score)
      }
      case JsError(err) => {
        Console.println("No building found - error:  " + err)
      }

    }

    Future(Ok("Ok"))
  }



}