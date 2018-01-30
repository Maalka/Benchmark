package models

import squants.energy.{Energy, Gigajoules, KBtus, TBtus}
import squants.space._

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads.min

case class SolarProperties(parameters: JsValue) {


  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def pvDefaults(resource:Int, pvArea:Double, wattsPerMeterSquared:Double, accessPerimeter:Double):JsValue = {

    resource match {
      case 0 => JsObject(Map (
        "pv_area" -> JsNumber(pvArea),
        "access_perimeter" -> JsNumber(accessPerimeter),
        "w_per_meter2" -> JsNumber(wattsPerMeterSquared),
        "system_capacity" -> JsNumber(roundAt(2)(pvArea * wattsPerMeterSquared)),
        "module_type" -> JsNumber(0),
        "losses" -> JsNumber(0.10),
        "array_type" -> JsNumber(0),
        "tilt" -> JsNumber(10),
        "azimuth" -> JsNumber(180),
        "inv_eff" -> JsNumber(0.96)
      ))
    }
  }



  def getBuilding: Future[BaseLine] = Future{

    val building: JsResult[BaseLine] = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(_) => parameters.validate[GenericBuilding]
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    building match {
      case JsSuccess(a: BaseLine, _) => a
      case JsError(err) => throw new Exception("Building Type parameters fail validation!")
    }
  }
}

case class PosInt(value: Int)
object PosInt {
  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))
}

case class PosDouble(value: Double)
object PosDouble {
  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))
}
case class BuildingArea(GFA:PosDouble)
object BuildingArea {
  implicit val buildingAreaReads: Reads[BuildingArea] = Json.reads[BuildingArea]
}


case class SolarMetrics(accessPerimeter:Option[Double],wattsPerMeterSquared:Option[Double],systemCapacity:Option[Double],
                        moduleType:Option[Int],losses:Option[Double],arrType:Option[Int],tilt:Option[Double],azimuth:Option[Double],invEff:Option[Double])
object SolarMetrics {
  implicit val energyReads: Reads[SolarMetrics] = (
    (JsPath \ "access_perimeter").read[Double] and
      (JsPath \ "w_per_meter2").read[Double] and
      (JsPath \ "system_capacity").read[Double] and
      (JsPath \ "module_type").read[Double](min(0.0)) and
      (JsPath \ "losses").read[Double](min(0.0)) and
      (JsPath \ "array_type").read[Double](min(0.0)) and
      (JsPath \ "tilt").read[Double](min(0.0)) and
      (JsPath \ "azimuth").read[Double](min(0.0)) and
      (JsPath \ "inv_eff").readNullable[Double]
    )(EnergyMetrics.apply _)
}

case class EnergyList(energies:List[EnergyMetrics])
object EnergyList {
  implicit val listReads:Reads[EnergyList] = Json.reads[EnergyList]
}