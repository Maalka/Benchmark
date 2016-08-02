/**
 * Created by rimukas on 10/12/15.
 */


package controllers
import models._

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import squants.energy.{Energy}

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: Controller =>

  implicit def doubleToJSValue(d:Double):JsValue = Json.toJson(d)
  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)
  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map{
    case e:Energy => e.value
  })

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T):Either[String, JsValue] = {

    response match {
      case v: Energy => Right(v)
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: List[Any] => Right{
        Json.toJson(v.map{
          case a:Energy => energyToJSValue(a)
          case a:EmissionsTuple => JsObject(Seq(a.eType -> Json.toJson(a.eValue)))
          case a:EnergyTuple => JsObject(Seq(a.energyType -> energyToJSValue(a.energyValue)))
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




  def getZEPIMetrics() = Action.async(parse.json) { implicit request =>

    val Baseline: EUIMetrics = EUIMetrics(request.body)

    val futures = Future.sequence(Seq(

      Baseline.getPropOutputList.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.getESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getTargetESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getMedianESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.percentBetterMedian.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterTarget.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterActual.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.actualGoalReduction.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.zepiActual.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.zepiMedian.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.zepiPercentBetter.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.siteEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.siteEnergyListConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.sourceEnergyListConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.medianSiteEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSiteEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.percentBetterSiteEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSourceEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSiteEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterSourceEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},

      Baseline.getDirectEmissionList().map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getIndirectEmissionList().map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.getTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.percentBetterTotalEmissions.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}

    ))

    val fieldNames = Seq(

      "propOutputList",

      "actualES",
      "targetES",
      "medianES",

      "percentBetterMedian",
      "percentBetterTarget",
      "percentBetterActual",
      "percentBetterActualtoGoal",

      "actualZEPI",
      "medianZEPI",
      "percentBetterZEPI",

      "siteEUI",
      "sourceEUI",
      "totalSiteEnergy",
      "siteEnergyList",
      "totalSourceEnergy",
      "sourceEnergyList",

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
}
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions