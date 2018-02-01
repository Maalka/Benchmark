package models

import com.eclipsesource.schema._
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

  val schema = Json.fromJson[SchemaType](Json.parse(
    """{
      "properties": {
        "id":    { "type": "integer" },
        "title": { "type": "string" },
        "body":  { "type": "string" }
      }
    }""".stripMargin)).get

  val validator = new SchemaValidator()
  val result = validator.validate(schema, parameters)



  val validators: Option[SolarResources] = parameters.validate[SolarResources].fold(
    invalid = {
      fieldErrors =>
        fieldErrors.foreach(x => {
          println("field: " + x._1 + ", errors: " + x._2)
        })
        None
    },
    valid = {
      name => Some(name)
    }
  )


  def getSolarResources: Future[SolarResources] = Future {
    parameters.validate[SolarResources] match {
      case JsSuccess(a: SolarResources, _) => a
      case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
    }
  }


  def initiateSolarResources(solarResources: SolarResources): Future[ValidatedSolarResources] = Future {
    val pv_default_resource = solarResources.pv_defaults_resource match {
      case Some(a: Int) => a
      case _ => 1
    }
    val solarID = solarResources.solar_file_id match {
      case Some(a: String) => a
      case _ => throw new Exception("No Solar File ID Found! ")
    }
    val climateZone = solarResources.climate_zone match {
      case Some(a: String) => a
      case _ => throw new Exception("No Climate Zone ID Found! ")
    }
    val units = solarResources.floor_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case _ => throw new Exception("Floor Area Units must be either ftSQ or mSQ")
    }
    val gfa = solarResources.floor_area match {
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

    ValidatedSolarResources(pv_default_resource, solarID, climateZone, gfa, stories)
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

    where is w_per_meter2 input or calculated?
    when module_type = 0:150 W/m2,1:190, 2:100

    CALCULATE SYSTEM CAPACITY FOR EACH SOLAR ARRAY ENTRY

    [gfa/#floors] -  [sqrt(gfa/#floors)*4*access_perim - 4*acess_perim2 ]
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
                          module_type: Option[Double], losses: Option[Double], array_type: Option[Double], tilt: Option[Double],
                          azimuth: Option[Double], inv_eff: Option[Double])

  object SolarMetrics {
    implicit val solarMetricsReads: Reads[SolarMetrics] = (
      (JsPath \ "access_perimeter").readNullable[Double] and
        (JsPath \ "w_per_meter2").readNullable[Double] and
        (JsPath \ "system_capacity").readNullable[Double] and
        (JsPath \ "module_type").readNullable[Double](min(0.0)) and
        (JsPath \ "losses").readNullable[Double](min(0.0)) and
        (JsPath \ "array_type").readNullable[Double](min(0.0)) and
        (JsPath \ "tilt").readNullable[Double](min(0.0)) and
        (JsPath \ "azimuth").readNullable[Double](min(0.0)) and
        (JsPath \ "inv_eff").readNullable[Double]
      ) (SolarMetrics.apply _)
  }


  case class ValidatedSolarResources(pv_defaults_resource: Int, solar_file_id: String, climate_zone: String,
                                     floor_area: Double, stories: Double)


  case class SolarResources(pv_defaults_resource: Option[Int], solar_file_id: Option[String], climate_zone: Option[String],
                            floor_area: Option[Double], floor_area_units: Option[String], stories: Option[Double])

  object SolarResources {
    implicit val solarResourcesReads: Reads[SolarResources] = Json.reads[SolarResources]
  }

}
/*            (
            (JsPath \ "pv_defaults_resource").readNullable[Int] and
              (JsPath \ "solar_file_id").readNullable[String] and
              (JsPath \ "climate_zone").readNullable[String] and
              (JsPath \ "floor_area").readNullable[Double](min(0.0)) and
              (JsPath \ "floor_area_units").readNullable[String] and
              (JsPath \ "stories").readNullable[Double](min(0.0))
            )(SolarResources.apply _)
        }*/






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



