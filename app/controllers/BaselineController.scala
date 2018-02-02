/**
 * Created by rimukas on 10/12/15.
 */


package controllers
import models._
import com.google.inject.Inject
import play.api.cache.{AsyncCacheApi, SyncCacheApi}
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.Future
import squants.energy.Energy

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: BaseController =>

  implicit def doubleToJSValue(d: Double): JsValue = Json.toJson(d)

  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)

  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map {
    case e: Energy => e.value
  })

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p); (math round n * s) / s
  }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T): Either[String, JsValue] = {

    response match {
      case v: Energy => Right(v)
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: List[Any] => Right {
        Json.toJson(v.map {
          case a: Energy => energyToJSValue(a)
          case a: EmissionsTuple => JsObject(Seq(a.eType -> Json.toJson(a.eValue)))
          case a: EnergyTuple => JsObject(Seq(a.energyType -> energyToJSValue(a.energyValue)))
          case a: PropParams => JsObject(Seq(
            "propType" -> JsString(a.propType),
            "propSize" -> JsNumber(a.propSize),
            "propPercent" -> JsNumber {
              a.propPercent match {
                case b => roundAt(2)(b * 100)
              }
            },
            "areaUnits" -> JsString {
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


  def getZEPIMetrics() = Action.async(parse.json) { implicit request =>

    val Baseline: EUIMetrics = EUIMetrics(request.body)

    val futures = Future.sequence(Seq(

      Baseline.getPV.map(api(_)).recover { case NonFatal(th) => apiRecover(th) }
      /*    Baseline.getPropOutputList.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.siteEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEUIwOnSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEUIwOffSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEUIwOnandOffSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.sourceEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEUIwOnSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEUIwOffSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEUIwOnandOffSiteConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.getTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      //this uses default state energy mixes for emissions calcs rather than scaling by source energies per TargetFinder
      //to follow TargetFinder use Baseline.medianTotalEmissions not Baseline.defaultMedianTotalEmissions


      Baseline.onSiteRenewableTotal.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.offSitePurchasedTotal.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      //this is the total site energy without accounting for renewable generation and/or purchasing
      Baseline.siteEnergyALL.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}*/

    ))

    val fieldNames = Seq(
      "solar"
      /*    "propOutputList",

      "siteEUI",
      "siteEUIwOnSite",
      "siteEUIwOffSite",
      "siteEUIwOnAndOffSite",

      "sourceEUI",
      "sourceEUIwOnSite",
      "sourceEUIwOffSite",
      "sourceEUIwOnAndOffSite",

      "totalEmissions",

      "onSiteRenewableTotal",
      "offSitePurchasedTotal",
      "siteEnergyALL"*/
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
}
class BaselineController @Inject() (val cache: AsyncCacheApi, cc: ControllerComponents) extends AbstractController(cc) with Logging with BaselineActions