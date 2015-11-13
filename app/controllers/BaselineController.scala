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


trait BaselineActions {
  this: Controller =>

  def makeBaseline() = Action.async(parse.json) { implicit request =>

    val getBaseline:EUIMetrics = EUIMetrics(request.body)
    val energyCalcs:EUICalculator = EUICalculator(request.body)


    energyCalcs.getSiteEnergy.map{case a => Console.println("Site Energy: " +  a)}
    energyCalcs.getTotalSiteEnergy.map{case a => Console.println("Total Site Energy: " +  a)}

    energyCalcs.getSourceEnergy.map{case a => Console.println("Source Energy: " +  a)}
    energyCalcs.getTotalSourceEnergy.map{case a => Console.println("Total Source Energy (with Pool/Parking): " +  a)}
    energyCalcs.getPoolEnergy.map{case a => Console.println("Source Energy for Pool: " +  a)}
    energyCalcs.getParkingEnergy.map{case a => Console.println("Source Energy for Parking: " +  a)}
    energyCalcs.getTotalSourceEnergyNoPoolNoParking.map{case a => Console.println("Total Source Energy for Building " +
      "(no pool/parking): " +  a)}

    getBaseline.expectedSourceEUI.map{case a => Console.println("Expected Building Source EUI: " +  a)}
    getBaseline.sourceEUI.map{case a => Console.println("Actual Building Source EUI: " +  a)}
    getBaseline.ES.map{case a => Console.println("Building EnergyStar Score: " +  a)}

    getBaseline.targetSourceEUI.map{case a => Console.println("Target Source EUI: " +  a)}
    getBaseline.targetSiteEUI.map{case a => Console.println("Target Site EUI: " +  a)}
    getBaseline.targetSourceEnergy.map{case a => Console.println("Target Source Energy: " +  a)}
    getBaseline.targetSiteEnergy.map{case a => Console.println("Target Site Energy: " +  a)}


    getBaseline.medianSourceEUI.map{case a => Console.println("Median Source EUI: " +  a)}
    getBaseline.medianSiteEUI.map{case a => Console.println("Median Site EUI: " +  a)}
    getBaseline.medianSiteEnergy.map{case a => Console.println("Median Site Energy: " +  a)}
    getBaseline.medianSourceEnergy.map{case a => Console.println("Median Source Energy: " +  a)}

    getBaseline.percentBetterSourceEUI.map{case a => Console.println("Percent Better Target Source EUI: " +  a)}
    getBaseline.percentBetterSiteEUI.map{case a => Console.println("Percent Better Target Site EUI: " +  a)}
    getBaseline.percentBetterSourceEnergy.map{case a => Console.println("Percent Better Target Source Energy: " +  a)}
    getBaseline.percentBetterSiteEnergy.map{case a => Console.println("Percent Better Target Site Energy: " +  a)}
    getBaseline.percentBetterES.map{case a => Console.println("Percent Better Target ES: " +  a)}

    Future(Ok("Ok"))


  }
}

class BaselineController(val cache: CacheApi) extends Controller with Security with Logging with BaselineActions

