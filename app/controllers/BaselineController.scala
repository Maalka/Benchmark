/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import scala.math._
import squants.energy.{Gigajoules, KBtus, Energy, KilowattHours}
import squants.space._

import models.EUIMetrics
import models.EUICalculator
import scala.util.{ Success, Failure, Try}


import scala.concurrent.ExecutionContext.Implicits.global


trait BaselineActions {
  this: Controller =>

  private def tryToOption[A](objs: List[Try[A]]): List[Option[A]] = {
    objs.map {
      case Success(e) => Option(e)
      case Failure(th) => None
    }
  }


  def makeBaseline() = Action.async(parse.json) { implicit request =>
    implicit def energyToJSValue(b :Energy): JsValueWrapper = Json.toJsFieldJsValueWrapper(b.value)


    val getBaseline: EUIMetrics = EUIMetrics(request.body)

    val energyCalcs: EUICalculator = EUICalculator(request.body)

    val futures = for {
      siteEnergy <- energyCalcs.getSiteEnergy.map { m => tryToOption(m) }
      totalSiteEnergy <- energyCalcs.getTotalSiteEnergy

      sourceEnergy <- energyCalcs.getSourceEnergy.map { m => tryToOption(m) }
      totalSourceEnergy <- energyCalcs.getTotalSourceEnergy

      poolEnergy <- energyCalcs.getPoolEnergy
      parkingEnergy <- energyCalcs.getParkingEnergy
      totalSourceEnergyNoPoolNoParking <- energyCalcs.getTotalSourceEnergyNoPoolNoParking

      expectedSourceEUI <- getBaseline.expectedSourceEUI
      sourceEUI <- getBaseline.sourceEUI
      es <- getBaseline.ES

      targetSourceEUI <- getBaseline.targetSourceEUI
      targetSiteEUI <- getBaseline.targetSiteEUI
      targetSourceEnergy <- getBaseline.targetSourceEnergy
      targetSiteEnergy <- getBaseline.targetSiteEnergy

      medianSourceEUI <- getBaseline.medianSourceEUI
      medianSiteEUI <- getBaseline.medianSiteEUI
      medianSiteEnergy <- getBaseline.medianSiteEnergy
      medianSourceEnergy <- getBaseline.medianSourceEnergy

      percentBetterSourceEUI <- getBaseline.percentBetterSourceEUI
      percentBetterSiteEUI <- getBaseline.percentBetterSiteEUI
      percentBetterSourceEnergy <- getBaseline.percentBetterSourceEnergy
      percentBetterSiteEnergy <- getBaseline.percentBetterSiteEnergy
      percentBetterES <- getBaseline.percentBetterES
    } yield {
        Json.obj(
          "siteEnergy" -> siteEnergy.map(_.map(_.value)),
          "totalSiteEnergy" -> totalSiteEnergy,

          "sourceEnergy" -> sourceEnergy.map(_.map(_.value)),
          "totalSourceEnergy" -> totalSourceEnergy,

          "poolEnergy" -> poolEnergy,
          "parkingEnergy" -> parkingEnergy,
          "totalSourceEnergyNoPoolNoParking" -> totalSourceEnergyNoPoolNoParking,

          "expectedSourceEUI" -> expectedSourceEUI,
          "sourceEUI" -> sourceEUI,
          "es" -> es,

          "targetSourceEUI" -> targetSourceEUI,
          "targetSiteEUI" -> targetSiteEUI,
          "targetSourceEnergy" -> targetSourceEnergy,
          "targetSiteEnergy" -> targetSiteEnergy,

          "medianSourceEUI" -> medianSourceEUI,
          "medianSiteEUI" -> medianSiteEUI,
          "medianSiteEnergy" -> medianSiteEnergy,
          "medianSourceEnergy" -> medianSourceEnergy,

          "percentBetterSourceEUI" -> percentBetterSourceEUI,
          "percentBetterSiteEUI" -> percentBetterSiteEUI,
          "percentBetterSourceEnergy" -> percentBetterSourceEnergy,
          "percentBetterSiteEnergy" -> percentBetterSiteEnergy,
          "percentBetterES" -> percentBetterES
        )
      }
    futures.map { res =>
      Ok(res)
    }
  }
}

class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions