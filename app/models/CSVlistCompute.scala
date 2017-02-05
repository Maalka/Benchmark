package models

import play.api.libs.json._
import scala.concurrent.Future
import squants.energy.Energy

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


/**
  * Created by rimukas on 1/13/17.
  */
case class CSVlistCompute() {
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

  def getMetrics(csvJson:JsValue):Future[JsObject] = {

    val Baseline: EUIMetrics = EUIMetrics(csvJson)

    val futures = Future.sequence(Seq(

      Baseline.getBuildingName.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.zepiMedian.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSiteEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEUIConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSiteEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
      Baseline.medianSourceEnergyConverted.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)}

    ))

    val fieldNames = Seq(


      "buildingName",
      "medianZEPI",
      "medianSiteEUI",
      "medianSourceEUI",
      "medianSiteEnergy",
      "medianSourceEnergy"

    )


    futures.map(fieldNames.zip(_)).map { r =>
      val errors = r.collect {
        case (n, Left(s)) => Json.obj(n -> s)
      }
      val results = r.collect {
        case (n, Right(s)) => Json.obj(n -> s)
      }

      println( Json.obj(
        "values" -> results,
        "errors" -> errors
      ))

      Json.obj(
        "values" -> results,
        "errors" -> errors
      )
    }
  }

}
