/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import squants.energy.{Gigajoules, KBtus, Energy, KilowattHours}

import models.EUIMetrics
import models.EUICalculator
import scala.util.control.NonFatal
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

  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)
  implicit def listTryEnergyToJSValue(v: List[Try[Energy]]): JsValue = Json.toJson(tryToOption[Energy](v).map(_.map(_.value)))

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api(response: Any):Either[String, JsValue] = {
    response match {
      case v: Energy => Right(v)
      case v: Double => Right(Json.toJson(v))
      case v: Int => Right(Json.toJson(v))
      case v: Energy => Right(v)
      case v: List[Try[Energy]] => Right(v)
      case None => Left("Could not recognize input type")
    }
  }

  def makeBaseline() = Action.async(parse.json) { implicit request =>
    val getBaseline: EUIMetrics = EUIMetrics(request.body)
    val energyCalcs: EUICalculator = EUICalculator(request.body)


    val fieldNames = Seq(
      "siteEnergy", "totalSiteEnergy",

      "sourceEnergy", "totalSourceEnergy",

      "poolEnergy", "parkingEnergy", "totalSourceEnergyNoPoolNoParking",

      "expectedSourceEUI", "sourceEUI", "ES",

      "targetSourceEUI", "targetSourceEnergy", "targetSiteEUI", "targetSiteEnergy",

      "medianSourceEUI", "medianSiteEUI", "medianSiteEnergy", "medianSourceEnergy",

      "percentBetterSourceEUI", "percentBetterSourceEnergy", "percentBetterSiteEUI", "percentBetterSiteEnergy", "percentBetterES")

    val futures = Future.sequence(Seq(

      energyCalcs.getSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      energyCalcs.getTotalSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      energyCalcs.getSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      energyCalcs.getTotalSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      energyCalcs.getPoolEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      energyCalcs.getParkingEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      energyCalcs.getTotalSourceEnergyNoPoolNoParking.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      getBaseline.expectedSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.sourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.ES.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      getBaseline.targetSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.targetSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.targetSiteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.targetSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      getBaseline.medianSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.medianSiteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.medianSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.medianSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      getBaseline.percentBetterSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.percentBetterSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.percentBetterSiteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.percentBetterSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      getBaseline.percentBetterES.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}
    ))

    futures.map(fieldNames.zip(_)).map { r =>
      val errors = r.collect {
        case (n, Left(s)) => Json.obj(n -> s)
      }
      val results = r.collect {
        case (n, Right(s)) => Json.obj(n -> s)
      }
      Ok(Json.obj(
        "values" -> results,
        "errors" -> errors
      ))
    }

  }
}
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions