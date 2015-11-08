/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import scala.math._

import models.EUIMetrics
import models.EUICalculator

import scala.concurrent.ExecutionContext.Implicits.global

class Baseline(val cache: CacheApi) extends Controller with Security with Logging{

  def makeBaseline() = Action.async(parse.json) { implicit request =>

    val getBaseline:EUIMetrics = EUIMetrics(request.body)
    val energyCalcs:EUICalculator = EUICalculator(request.body)

    getBaseline.ExpectedEUI.map{case a => Console.println("Expected EUI: " +  a)}
    getBaseline.sourceEUI.map{case a => Console.println("Actual Source EUI: " +  a)}
    getBaseline.ES.map{case a => Console.println("EnergyStar Score: " +  a)}
    getBaseline.targetEUI.map{case a => Console.println("TargetSourceEUI: " +  a)}

    energyCalcs.getSiteEnergy.map{case a => Console.println("Site Energy: " +  a)}
    energyCalcs.getSourceEnergy.map{case a => Console.println("Source Energy: " +  a)}
    energyCalcs.getTotalSourceEnergy.map{case a => Console.println("Total Source Energy: " +  a)}


    Future(Ok("Ok"))

  }

}





