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

  def setArrayDefaults(metrics: SolarMetrics, solarResources: ValidatedSolarResources):Future[ValidatedSolarMetrics] = Future{

    val default = solarResources.pv_resource

    //Default module_type is 0 - Standard, Others are 1-Premium, 2-Thin Film
    val module_type = metrics.module_type match {
      case Some(a: Int) if List(0,1,2).contains(a) => a
      case Some(_) => throw new Exception("Module Type Must be [0,1,2]! ")
      case _ if (default==0) => 0
      case _ => throw new Exception("Module Type Must be [0,1,2]! No Default Set. ")
    }
    //Default array_type is 0=Fixed - Open Rack, 1=Fixed - Roof Mounted, 2=1-Axis, 3=1-Axis Backtracking or 4=2-Axis
    val array_type = metrics.array_type match {
      case Some(a: Int) if List(0,1,2,3,4).contains(a) => a
      case Some(_) => throw new Exception("Array Type Must be [0,1,2,3,4]! ")
      case _ if (default==0) => 0
      case _ => throw new Exception("Array Type Must be [0,1,2,3,4]! No Default Set. ")
    }
    //Default losses
    val losses = metrics.losses match {
      case Some(a: Double) if a < 1.0 => a
      case Some(_) => throw new Exception("Losses are a percentage and must be less that 1! ")
      case _ if (default==0) => 0.10
      case _ => throw new Exception("Losses are a percentage and must be less that 1! No Default Set. ")
    }
    //Default tilt
    val tilt: Double = metrics.tilt match {
      case Some(a: Double) if a < 360.0 => a
      case Some(_) => throw new Exception("Tilt must be less than 360 Degrees! ")
      case _ if (default==0) => 10
      case _ => throw new Exception("Tilt must be less than 360 Degrees! No Default Set. ")
    }
    //Default azimuth
    val azimuth: Double = metrics.azimuth match {
      case Some(a: Double) if a < 360.0 => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default==0) => 180
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    //Default inveter efficiency
    val inv_eff = metrics.inv_eff match {
      case Some(a: Double) if a < 1.0 => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default==0) => 0.96
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    //Default access perimeter
    val access_perimeter = metrics.access_perimeter match {
      case Some(a: Double) => a
      case _ if (default==0) => 2.0
      case _ => throw new Exception("Access Perimeter must be positive! No Default Set. ")
    }
    val w_per_meter2 = metrics.w_per_meter2 match {
      case Some(a: Double)  => a
      case _ if (module_type==0) => 150.0
      case _ if (module_type==1) => 190.0
      case _ if (module_type==2) => 100.0
      case _ => throw new Exception("No w_per_meter2 value for Module Type. ")
    }
    val area_units = metrics.pv_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case Some(_) => throw new Exception("PV Area Units Must be mSQ or ftSQ! ")
      case _ => "No PV Area Units"
    }
    val pv_area = metrics.estimated_area match {
      case Some(a: Double) => {
        area_units match {
          case "mSQ" => (Area((a, "mSQ")).get to SquareMeters)
          case "ftSQ" => (Area((a, "ftSQ")).get to SquareMeters)
          case _ => throw new Exception("If PV Area is supplied, pv_area_units must be mSQ or ftSQ! ")
        }
      }
      case _ => solarResources.floor_area / solarResources.stories - 4*access_perimeter*(math.sqrt(solarResources.floor_area /
        solarResources.stories) - access_perimeter)
    }

    //Default access perimeter
    val system_capacity = metrics.system_capacity match {
      case Some(a: Double) => a
      case _ if (default==0) => pv_area*w_per_meter2
      case _ => throw new Exception("System Capacity must be positive! No Defaults Set. ")
    }

    ValidatedSolarMetrics(module_type,array_type,losses,tilt,azimuth,inv_eff, system_capacity, solarResources.solar_file_id)

  }


  //This sets defaults where needed for lookups and double checks for file references
  def initiateSolarResources(solarResources: SolarResources): Future[ValidatedSolarResources] = Future {

    //default resources are always 1
    val pv_resource = solarResources.pv_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    val prescriptive_resource = solarResources.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    val solarID = solarResources.solar_file_id match {
      case Some(a: String) => a
      case _ => throw new Exception("No Solar File ID Found! ")
    }
    val climateZone = prescriptive_resource match {
      case 0 => {
        solarResources.climate_zone match {
          case Some(a) if List("0A", "0B", "1A", "1B", "2A", "2B", "3A", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8").contains(a) => a
          case _ => throw new Exception("Not a valid Climate Zone for given Prescriptive Resource! ")
        }
      }
      case _ => throw new Exception("Prescriptive Resouce not Supported! ")
    }

    val units = solarResources.floor_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case _ => throw new Exception("Floor Area Units must be either ftSQ or mSQ")
    }
    val floorArea = solarResources.floor_area match {
      case Some(a: Double) => {
        units match {
          case "mSQ" => (Area((a, "mSQ")).get to SquareMeters)
          case "ftSQ" => (Area((a, "ftSQ")).get to SquareMeters)
          case _ => throw new Exception("Floor Area Units Must be mSQ or ftSQ! ")
        }
      }
      case _ => throw new Exception("No Floor Area Found! ")
    }

    val stories = solarResources.stories match {
      case Some(a: Double) =>  a
      case _ => throw new Exception("No Number of Stories Found! ")
    }

    ValidatedSolarResources(pv_resource, solarID, climateZone, floorArea, stories)
  }


  def getSolarList: Future[SolarList] = Future {
    parameters.asOpt[SolarList] match {
      case Some(a) => a
      case _ => throw new Exception("Could not retrieve list of PV Systems")
    }
  }

  def getSolarResources: Future[SolarResources] = Future {
    parameters.validate[SolarResources] match {
      case JsSuccess(a: SolarResources, _) => a
      case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
    }
  }

  /*def getPVWattsData(arrayList:List[ValidatedSolarMetrics]):Future[ValidatedSolarMetrics] = Future {
    PVWatts API
    format: JSON
    api_key
    system_capacity - calculate
    module_type
    losses
    array_type
    tilt
    azimuth
    file_id
    inv_eff}*/


def setPVDefaults =
for {
  solarResources <- getSolarResources
  //set defaults for lookup tables and convert to default units
  solarResourcesDefaults <- initiateSolarResources(solarResources)
  arrayList <- getSolarList
  arrayListDefaults <- Future.sequence(arrayList.pv_data.map(setArrayDefaults(_,solarResourcesDefaults)))
} yield {
  arrayListDefaults
}




case class SolarList(pv_data: List[SolarMetrics])

object SolarList {
implicit val listReads: Reads[SolarList] = Json.reads[SolarList]
}

case class SolarMetrics(
                       estimated_area: Option[Double],
                       pv_area_units: Option[String],
                       access_perimeter: Option[Double],
                       w_per_meter2: Option[Double],
                       system_capacity: Option[Double],
                       module_type: Option[Int],
                       losses: Option[Double],
                       array_type: Option[Int],
                       tilt: Option[Double],
                       azimuth: Option[Double],
                       inv_eff: Option[Double])

object SolarMetrics {
implicit val solarMetricsReads: Reads[SolarMetrics] = Json.reads[SolarMetrics]
}




case class SolarResources(
                         pv_resource: Option[Int],
                         prescriptive_resource: Option[Int],
                         solar_file_id: Option[String],
                         climate_zone: Option[String],
                         floor_area: Option[Double],
                         floor_area_units: Option[String],
                         stories: Option[Double])

object SolarResources {
implicit val solarResourcesReads: Reads[SolarResources] = Json.reads[SolarResources]
}

}


// These classes represent data that have been populated with defaults
case class ValidatedSolarMetrics(
                              module_type: Int,
                              array_type: Int,
                              losses: Double,
                              tilt: Double,
                              azimuth: Double,
                              inv_eff: Double,
                              system_capacity: Double,
                              solar_file_id: String
                            )
case class ValidatedSolarResources(
                                pv_resource: Int,
                                solar_file_id: String,
                                climate_zone: String,
                                floor_area: Double,
                                stories: Double
                              )





