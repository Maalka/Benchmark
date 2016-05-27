/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import squants.space.{SquareFeet, SquareMeters}
import scala.concurrent.Future
import squants.energy.{Gigajoules, KBtus, Energy, KilowattHours}

import models._
import scala.util.control.NonFatal
import scala.util.{ Success, Failure, Try}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: Controller =>

  implicit def roundDouble(d:Double):JsValue = Json.toJson(roundAt(4)(d))
  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(roundAt(4)(b.value))
  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map{
    case e:Energy => roundAt(4)(e.value)
  })


  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T, conversionFactor:Double=1.0):Either[String, JsValue] = {

    response match {
      case v: Energy => Right(v*conversionFactor)
      case v: Double => Right(v*conversionFactor)
      case v: Int => Right(Json.toJson(v*conversionFactor))
      case v: List[Any] => Right{
        Json.toJson(v.map{
          case a:Energy => energyToJSValue(a*conversionFactor)
          case a:EmissionsTuple => JsObject(Seq(a.eType -> Json.toJson(a.eValue)))
          case a:EnergyTuple => JsObject(Seq(a.energyType -> energyToJSValue(a.energyValue*conversionFactor)))
          case a:TableEntry => JsObject(Seq(
            "ES" -> JsNumber(a.ES),
            "CmPercent" -> JsNumber(a.CmPercent),
            "Ratio" -> JsNumber(a.Ratio)
            ))
          case a:PropParams => JsObject(Seq(
            "propType" -> JsString(a.propType),
            "propSize" -> JsNumber(a.propSize),
            "propPercent" -> JsNumber{
              a.propPercent match {
                case b => roundAt(2)(b*100)}
              },

            "areaUnits" -> JsString{
              a.areaUnits match {
                case "mSQ" => "sq.m"
                case "ftSQ" => "sq.ft"
              }
            }
          ))
        })
      }
      case v: String => Right(Json.toJson(v))
      case None => Left("Could not recognize input type")
    }
  }




  def getPredictedEnergy() = Action.async(parse.json) { implicit request =>

    val Baseline: EUIMetrics = EUIMetrics(request.body)

    val futures = Future.sequence(Seq(

      Baseline.getPropOutputList.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.esScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.zepiActual.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.zepiMedian.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.zepiPercentBetter.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.siteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.medianSiteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSiteEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.percentBetterSiteEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSourceEUI.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.getDirectEmissionList().map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getIndirectEmissionList().map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}

    ))

    val fieldNames = Seq(

      "propOutputList",

      "actualES",

      "actualZEPI",
      "medianZEPI",
      "percentBetterZEPI",

      "siteEUI",
      "sourceEUI",
      "totalSiteEnergy",
      "totalSourceEnergy",

      "medianSiteEUI",
      "medianSourceEUI",
      "medianSiteEnergy",
      "medianSourceEnergy",

      "percentBetterSiteEUI",
      "percentBetterSourceEUI",
      "percentBetterSiteEnergy",
      "percentBetterSourceEnergy",

      "directSiteEmissions",
      "indirectSiteEmissions",
      "totalEmissions",
      "medianEmissions",
      "percentBetterEmissions"
    )

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

  def getActualEnergy() = Action.async(parse.json) { implicit request =>
    val energyCalcs: EUICalculator = EUICalculator(request.body)

    val fieldNames = Seq(
      "sourceEnergy"
    )

    val futures = Future.sequence(Seq(
      // extra information, e.g. site/source conversion breakdowns
      energyCalcs.getSourceEnergy.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}
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