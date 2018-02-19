package models

import squants.energy._
import squants.space._

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads.min

case class SolarProperties(parameters: JsValue) {

  val prescriptiveEUI = PrescriptiveValues(parameters)

  def setArrayDefaults(metrics: SolarMetrics, solarResources: ValidatedSolarResources): Future[ValidatedSolarMetrics] = Future {


    val default = solarResources.pv_resource

    //Default module_type is 0 - Standard, Others are 1-Premium, 2-Thin Film
    val module_type = metrics.module_type match {
      case Some(a: Int) if List(0, 1, 2).contains(a) => a
      case Some(_) => throw new Exception("Module Type Must be [0,1,2]! ")
      case _ if (default == 0) => 0
      case _ => throw new Exception("Module Type Must be [0,1,2]! No Default Set. ")
    }
    //Default array_type is 0=Fixed - Open Rack, 1=Fixed - Roof Mounted, 2=1-Axis, 3=1-Axis Backtracking or 4=2-Axis
    val array_type = metrics.array_type match {
      case Some(a: Int) if List(0, 1, 2, 3, 4).contains(a) => a
      case Some(_) => throw new Exception("Array Type Must be [0,1,2,3,4]! ")
      case _ if (default == 0) => 0
      case _ => throw new Exception("Array Type Must be [0,1,2,3,4]! No Default Set. ")
    }
    //Default losses
    val losses = metrics.losses match {
      case Some(a: Double) if a < 99.0 => a
      case Some(_) => throw new Exception("Losses are a percentage and must be less that 99! ")
      case _ if (default == 0) => 10.0
      case _ => throw new Exception("Losses are a percentage and must be less that 1! No Default Set. ")
    }
    //Default tilt
    val tilt: Double = metrics.tilt match {
      case Some(a: Double) if (a > -5.0 && a < 90.0) => a
      case Some(_) => throw new Exception("Tilt must be less than 90 Degrees! ")
      case _ if (default == 0) => 10.0
      case _ => throw new Exception("Tilt must be less than 90 Degrees! No Default Set. ")
    }
    //Default azimuth
    val azimuth: Double = metrics.azimuth match {
      case Some(a: Double) if a < 360.0 => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default == 0) => 180
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    //Default inveter efficiency
    val inv_eff = metrics.inv_eff match {
      case Some(a: Double) if (a > 90.0 && a < 99.5) => a
      case Some(_) => throw new Exception("Azimuth must be less than 360 Degrees! ")
      case _ if (default == 0) => 96.0
      case _ => throw new Exception("Azimuth must be less than 360 Degrees! No Default Set. ")
    }
    val area_units = metrics.pv_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case Some(_) => throw new Exception("PV Area Units Must be mSQ or ftSQ! ")
      case _ => "No PV Area Units"
    }
    //Default access perimeter
    val access_perimeter = metrics.access_perimeter match {
      case Some(a: Double) => {
        area_units match {
          case "mSQ" => Meters(a) to Meters
          case "ftSQ" => Feet(a) to Meters
          case _ => throw new Exception("If access_perimeter is provided, pv_area_units must be mSQ or ftSQ! ")
        }
      }
      case _ if (default == 0) => 2.0 //Meters
      case _ => throw new Exception("If access perimeter is supplied, pv_area_units must be mSQ or ftSQ! ")
    }
    val w_per_meter2 = metrics.w_per_meter2 match {
      case Some(a: Double) => a
      case _ if (module_type == 0) => 150.0
      case _ if (module_type == 1) => 190.0
      case _ if (module_type == 2) => 100.0
      case _ => throw new Exception("No w_per_meter2 value for Module Type. ")
    }

    val pv_area = metrics.estimated_area match {
      case Some(a: Double) => {
        area_units match {
          case "mSQ" => (Area((a, "mSQ")).get to SquareMeters)
          case "ftSQ" => (Area((a, "ftSQ")).get to SquareMeters)
          case _ => throw new Exception("If PV Area is supplied, pv_area_units must be mSQ or ftSQ! ")
        }
      }
      case _ => (solarResources.floor_area / solarResources.stories) - 4 * access_perimeter * (math.sqrt(solarResources.floor_area /
        solarResources.stories) - access_perimeter)
    }

    //Default access perimeter
    val system_capacity = metrics.system_capacity match {
      case Some(a: Double) => a
      case _ if (default == 0) => pv_area * w_per_meter2 * (Watts(1) to Kilowatts)
      case _ => throw new Exception("System Capacity must be positive! No Defaults Set. ")
    }

    ValidatedSolarMetrics(module_type, array_type, losses, tilt, azimuth, inv_eff, system_capacity, solarResources.file_id)

  }


  //This sets defaults where needed for lookups and double checks for file references
  def initiateSolarResources(solarResources: SolarResources, building_size: Double): Future[ValidatedSolarResources] = Future {

    //building_size is always in Square Feet

    //default resources are always 1
    val pv_resource = solarResources.pv_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    val prescriptive_resource = solarResources.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    val solarID = solarResources.file_id match {
      case Some(a: String) => a
      case _ => throw new Exception("No Solar File ID Found! ")
    }
    val climateZone = prescriptive_resource match {
      case 0 => {
        solarResources.climate_zone match {
          case Some(a) if List("1A", "1B", "2A", "2B", "3A", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8").contains(a) => a
          case _ => throw new Exception("Not a valid Climate Zone for given Prescriptive Resource! ")
        }
      }
      case _ => throw new Exception("Prescriptive Resouce not Supported! ")
    }

    val floorArea = (Area((building_size, "ftSQ")).get to SquareMeters)


    val stories = solarResources.stories match {
      case Some(a: Double) => a
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

  def setPVDefaults: Future[List[ValidatedSolarMetrics]] = {
    for {
      validatedPropList <- prescriptiveEUI.getValidatedPropList
      building_size <- Future(validatedPropList.map(_.floor_area).sum)
      solarResources <- getSolarResources
      //set defaults for lookup tables and convert to default units
      solarResourcesDefaults <- initiateSolarResources(solarResources, building_size)
      arrayList <- getSolarList
      arrayListDefaults <- Future.sequence(arrayList.pv_data.map(setArrayDefaults(_, solarResourcesDefaults)))
    } yield arrayListDefaults
  }
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
                         file_id: Option[String],
                         climate_zone: Option[String],
                         floor_area: Option[Double],
                         floor_area_units: Option[String],
                         stories: Option[Double])

object SolarResources {
implicit val solarResourcesReads: Reads[SolarResources] = Json.reads[SolarResources]
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
                              file_id: String
                            )
case class ValidatedSolarResources(
                                pv_resource: Int,
                                file_id: String,
                                climate_zone: String,
                                floor_area: Double,
                                stories: Double
                              )





