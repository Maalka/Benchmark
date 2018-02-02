package models

//import com.eclipsesource.schema._
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


//  def getArrayDefaults(solarList: SolarList, solarResources: ValidatedSolarResources): Future[SolarList] = Future {
//    solarList.pv_data.map(setArrayDefaults(_, solarResources))
//  }

  def setArrayDefaults(metrics: SolarMetrics, solarResources: ValidatedSolarResources):Future[ValidatedSolarMetrics] = Future{

    val default = solarResources.pv_resource

    //Default module_type is 0 - Standard, Others are 1-Premium, 2-Thin Film
    val module_type = metrics.module_type match {
      case Some(a: Int) if List(0,1,2).contains(a) => a
      case Some(_) => throw new Exception("Module Type Must be [0,1,2]! ")
      case _ if (default==1) => 0
      case _ => throw new Exception("Module Type Must be [0,1,2]! No Default Set. ")
    }
    //Default array_type is 0=Fixed - Open Rack, 1=Fixed - Roof Mounted, 2=1-Axis, 3=1-Axis Backtracking or 4=2-Axis
    val array_type = metrics.array_type match {
      case Some(a: Int) if List(0,1,2,3,4).contains(a) => a
      case Some(_) => throw new Exception("Array Type Must be [0,1,2,3,4]! ")
      case _ if (default==1) => 0
      case _ => throw new Exception("Array Type Must be [0,1,2,3,4]! No Default Set. ")
    }
    //Default losses
    val losses = metrics.losses match {
      case Some(a: Double) if a < 1.0 => a
      case Some(_) => throw new Exception("Losses are a percentage and must be less that 1! ")
      case _ if (default==1) => 0.10
      case _ => throw new Exception("Losses are a percentage and must be less that 1! No Default Set. ")
    }
    //Default tilt
    val tilt: Double = metrics.tilt match {
      case Some(a: Double) if a < 360.0 => a
      case Some(_) => throw new Exception("Tilt must be less than 360 Degrees! ")
      case _ if (default==1) => 10
      case _ => throw new Exception("Tilt must be less than 360 Degrees! No Default Set. ")
    }
    //Default azimuth
    val azimuth: Double = metrics.azimuth match {
      case Some(a: Double) if a < 360.0 => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default==1) => 180
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    //Default inveter efficiency
    val inv_eff = metrics.inv_eff match {
      case Some(a: Double) if a < 1.0 => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default==1) => 0.96
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    //Default access perimeter
    val access_perimeter = metrics.access_perimeter match {
      case Some(a: Double) if a > 0.0 => a
      case Some(_) => throw new Exception("Access Perimeter must be positive! ")
      case _ if (default==1) => 2.0
      case _ => throw new Exception("Access Perimeter must be positive! No Default Set. ")
    }
    val w_per_meter2 = metrics.w_per_meter2 match {
      case Some(a: Double) if a > 0.0 => a
      case Some(_) => throw new Exception("w_per_meter2 must be positive! ")
      case _ if (module_type==1) => 150
      case _ if (module_type==2) => 190
      case _ if (module_type==3) => 100
      case _ => throw new Exception("w_per_meter2 must be positive! No Module Type Set. ")
    }
    val pv_area:Double = solarResources.floor_area / solarResources.stories - 4*access_perimeter*(math.sqrt(solarResources.floor_area /
      solarResources.stories) - access_perimeter)

    //Default access perimeter
    val system_capacity = metrics.system_capacity match {
      case Some(a: Double) if a > 0.0 => a
      case Some(_) => throw new Exception("System Capacity must be positive! ")
        // Tadas: commenting out
        // case _ if (default==1) => pv_area*w_per_meter2
      case _ if (default==1) => pv_area
      case _ => throw new Exception("System Capacity must be positive! No Defaults Set. ")
    }

    ValidatedSolarMetrics(module_type,array_type,losses,tilt,azimuth,inv_eff, system_capacity)

  }

  def getSolarResources: Future[SolarResources] = Future {
    parameters.validate[SolarResources] match {
      case JsSuccess(a: SolarResources, _) => a
      case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
    }
  }

  //This sets defaults where needed for lookups and double checks for file references
  def initiateSolarResources(solarResources: SolarResources): Future[ValidatedSolarResources] = Future {

    val prescriptive_resource_1:List[String] =
      List("0A", "0B", "1A", "1B", "2A", "2B", "3A", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8")

    //default resources are always 1
    val pv_resource = solarResources.pv_resource match {
      case Some(a: Int) => a
      case _ => 1
    }
    val prescriptive_resource = solarResources.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 1
    }
    val solarID = solarResources.solar_file_id match {
      case Some(a: String) => a
      case _ => throw new Exception("No Solar File ID Found! ")
    }
    val climateZone = prescriptive_resource match {
      case 1 => {
        solarResources.climate_zone match {
          case Some(a) if prescriptive_resource_1.contains(a) => a
          case _ => throw new Exception("Not a valid Climate Zone for given Prescriptive Resource! ")
        }
      }
      case _ => throw new Exception("No Climate Zone ID Found! ")
    }

    val units = solarResources.floor_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case _ => throw new Exception("Floor Area Units must be either ftSQ or mSQ")
    }
    val floorArea = solarResources.floor_area match {
      case Some(a: Double) if a > 0.0 => {
        units match {
          case "mSQ" => (Area((a, units)).get to SquareMeters)
          case "ftSQ" => (Area((a, units)).get to SquareMeters)
          case _ => throw new Exception("Floor Area Units Must be mSQ or ftSQ! ")
        }
      }
      case Some(a: Double) if a < 0.0 => throw new Exception("Floor Area Must be Positive! ")
      case _ => throw new Exception("No Floor Area Found! ")
    }

    val stories = solarResources.stories match {
      case Some(a: Double) if a > 0.0 =>  a
      case Some(a: Double) if a < 0.0 =>  throw new Exception("Number of Stories Must be Positive! ")
      case _ => throw new Exception("No Number of Stories Found! ")
    }

    ValidatedSolarResources(pv_resource, solarID, climateZone, floorArea, stories)
  }


  def setPVDefaults =
    for {
      solarResources <- getSolarResources
      floorArea <- initiateSolarResources(solarResources)
      //stories <- getStories
    } yield {
      //println(solarList)
      println(floorArea)
      //println(stories)
      1.0
    }


  /*PVWatts API
    format: JSON
    api_key
    system_capacity - calculate
    module_type
    losses
    array_type
    tilt
    azimuth
    file_id
    inv_eff




    format: JSON
    api_key
    system_capacity - calculate
    file_id




    CALCULATE SYSTEM CAPACITY FOR EACH SOLAR ARRAY ENTRY


    */



  def getSolarList: Future[SolarList] = Future {
    parameters.asOpt[SolarList] match {
      case Some(a) => a
      case _ => throw new Exception("Could not retrieve list of PV Systems")
    }
  }

  case class SolarList(pv_data: List[SolarMetrics])

  object SolarList {
    implicit val listReads: Reads[SolarList] = Json.reads[SolarList]
  }

  case class SolarMetrics(access_perimeter: Option[Double], w_per_meter2: Option[Double], system_capacity: Option[Double],
                          module_type: Option[Int], losses: Option[Double], array_type: Option[Int], tilt: Option[Double],
                          azimuth: Option[Double], inv_eff: Option[Double])

  object SolarMetrics {
    implicit val solarMetricsReads: Reads[SolarMetrics] = Json.reads[SolarMetrics]
  }


  case class ValidatedSolarMetrics(module_type: Int, array_type: Int, losses: Double, tilt: Double,
                          azimuth: Double, inv_eff: Double, system_capacity: Double)

  object ValidatedSolarMetrics {
    implicit val validatedSolarMetricsReads: Reads[ValidatedSolarMetrics] = Json.reads[ValidatedSolarMetrics]
  }


  case class ValidatedSolarResources(pv_resource: Int, solar_file_id: String, climate_zone: String,
                                     floor_area: Double, stories: Double)


  case class SolarResources(pv_resource: Option[Int], prescriptive_resource: Option[Int], solar_file_id: Option[String], climate_zone: Option[String],
                            floor_area: Option[Double], floor_area_units: Option[String], stories: Option[Double])

  object SolarResources {
    implicit val solarResourcesReads: Reads[SolarResources] = Json.reads[SolarResources]
  }

}



/*    def roundAt(p: Int)(n: Double): Double = {
      val s = math pow(10, p); (math round n * s) / s
    }

    resource match {
      case 0 => JsObject(Map(
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
    }*/



