/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import scala.math._
import squants.energy.{Gigajoules, KBtus, Energy, KilowattHours}
import squants.space._

import models.EUIMetrics
import models.EUICalculator

import scala.concurrent.ExecutionContext.Implicits.global

class Baseline(val cache: CacheApi) extends Controller with Security with Logging{

  def makeBaseline() = Action.async(parse.json) { implicit request =>

    val getBaseline:EUIMetrics = EUIMetrics(request.body)
    val energyCalcs:EUICalculator = EUICalculator(request.body)

    getBaseline.ExpectedEUI.map{case a => Console.println("Expected Building EUI: " +  a)}
    getBaseline.sourceEUI.map{case a => Console.println("Actual Building Source EUI: " +  a)}
    getBaseline.ES.map{case a => Console.println("Building EnergyStar Score: " +  a)}
    getBaseline.targetEUI.map{case a => Console.println("Building TargetSourceEUI: " +  a)}

    energyCalcs.getSiteEnergy.map{case a => Console.println("Site Energy: " +  a)}
    energyCalcs.getSourceEnergy.map{case a => Console.println("Source Energy: " +  a)}
    energyCalcs.getTotalSourceEnergy.map{case a => Console.println("Total Source Energy (with Pool/Parking): " +  a)}
    energyCalcs.getTotalSourceEnergyNoPoolNoParking.map{case a => Console.println("Total Source Energy for Building " +
      "(no pool/parking): " +  a)}
    energyCalcs.getPoolEnergy.map{case a => Console.println("Source Energy for Pool: " +  a)}
    energyCalcs.getParkingEnergy.map{case a => Console.println("Source Energy for Parking: " +  a)}


    Future(Ok("Ok"))

  }

}





