/**
 * Created by rimukas on 10/12/15.
 */


package controllers
import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.validation.VA
import models._
import com.google.inject.Inject
import play.api.Logger
import play.api.cache.{AsyncCacheApi, SyncCacheApi}
import play.api.libs.json.Reads.min
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{Await, Future}
import squants.energy.Energy

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.concurrent.duration._

class BaselineController @Inject() (val cache: AsyncCacheApi, cc: ControllerComponents, nrel_client: NREL_Client) extends AbstractController(cc) with Logging {

  implicit def doubleToJSValue(d: Double): JsValue = Json.toJson(d)

  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)

  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map {
    case e: Energy => e.value
  })

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p);
    (math round n * s) / s
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
          case a: ValidatedSolarMetrics => JsObject(Seq(
            "system_capacity" -> JsNumber(147418.77408607837), //JsNumber(a.system_capacity),
            "module_type" -> JsNumber(a.module_type),
            "losses" -> JsNumber(a.losses),
            "array_type" -> JsNumber(a.array_type),
            "tilt" -> JsNumber(a.tilt),
            "azimuth" -> JsNumber(a.azimuth),
            //"inv_eff" -> JsNumber(a.inv_eff),
            "file_id" -> JsString(a.file_id)
            //"lat" -> JsNumber(40),
            //"lon" -> JsNumber(-105)
          ))
          case a: ValidatedPropTypes => JsObject(Seq(
            "prop_types" -> JsString(a.building_type),
            "floor_area" -> JsNumber(a.floor_area),
            "floor_area_units" -> JsString(a.floor_area_units)
          ))
        })
      }
      case v: String => Right(Json.toJson(v))
      case a: ElectricityDistribution => Right(JsObject(Seq(
        "htg" -> JsNumber(a.elec_htg),
        "clg" -> JsNumber(a.elec_clg),
        "intLgt" -> JsNumber(a.elec_intLgt),
        "extLgt" -> JsNumber(a.elec_extLgt),
        "intEqp" -> JsNumber(a.elec_intEqp),
        "extEqp" -> JsNumber(a.elec_extEqp),
        "fans" -> JsNumber(a.elec_fans),
        "pumps" -> JsNumber(a.elec_pumps),
        "heatRej" -> JsNumber(a.elec_heatRej),
        "humid" -> JsNumber(a.elec_humid),
        "heatRec" -> JsNumber(a.elec_heatRec),
        "swh" -> JsNumber(a.elec_swh),
        "refrg" -> JsNumber(a.elec_refrg),
        "gentor" -> JsNumber(a.elec_gentor),
        "net" -> JsNumber(a.elec_net)
      )))
      case a: NaturalGasDistribution => Right(JsObject(Seq(
        "htg" -> JsNumber(a.ng_htg),
        "clg" -> JsNumber(a.ng_clg),
        "intLgt" -> JsNumber(a.ng_intLgt),
        "extLgt" -> JsNumber(a.ng_extLgt),
        "intEqp" -> JsNumber(a.ng_intEqp),
        "extEqp" -> JsNumber(a.ng_extEqp),
        "fans" -> JsNumber(a.ng_fans),
        "pumps" -> JsNumber(a.ng_pumps),
        "heatRej" -> JsNumber(a.ng_heatRej),
        "humid" -> JsNumber(a.ng_humid),
        "heatRec" -> JsNumber(a.ng_heatRec),
        "swh" -> JsNumber(a.ng_swh),
        "refrg" -> JsNumber(a.ng_refrg),
        "gentor" -> JsNumber(a.ng_gentor),
        "net" -> JsNumber(a.ng_net)
      )))
      case a: EndUseDistribution => Right(JsObject(Seq(
        "htg" -> JsNumber(a.htg),
        "clg" -> JsNumber(a.clg),
        "intLgt" -> JsNumber(a.intLgt),
        "extLgt" -> JsNumber(a.extLgt),
        "intEqp" -> JsNumber(a.intEqp),
        "extEqp" -> JsNumber(a.extEqp),
        "fans" -> JsNumber(a.fans),
        "pumps" -> JsNumber(a.pumps),
        "heatRej" -> JsNumber(a.heatRej),
        "humid" -> JsNumber(a.humid),
        "heatRec" -> JsNumber(a.heatRec),
        "swh" -> JsNumber(a.swh),
        "refrg" -> JsNumber(a.refrg),
        "gentor" -> JsNumber(a.gentor),
        "net" -> JsNumber(a.net)
      )))
      case a: ValidatedConversionDetails => Right(JsObject(Seq(
        "metric_type" -> JsString(a.metric_type),
        "conversion_resource" -> JsNumber(a.conversion_resource),
        "source_electricity" -> JsNumber(a.source_electricity),
        "source_natural_gas" -> JsNumber(a.source_natural_gas),
        "source_fuel_oil" -> JsNumber(a.source_fuel_oil),
        "source_propane" -> JsNumber(a.source_propane),
        "source_steam" -> JsNumber(a.source_steam),
        "source_hot_water" -> JsNumber(a.source_hot_water),
        "source_chilled_water" -> JsNumber(a.source_chilled_water),
        "source_coal" -> JsNumber(a.source_coal),
        "carbon_electricity" -> JsNumber(a.carbon_electricity),
        "carbon_natural_gas" -> JsNumber(a.carbon_natural_gas),
        "carbon_fuel_oil" -> JsNumber(a.carbon_fuel_oil),
        "carbon_propane" -> JsNumber(a.carbon_propane),
        "carbon_steam" -> JsNumber(a.carbon_steam),
        "carbon_hot_water" -> JsNumber(a.carbon_hot_water),
        "carbon_chilled_water" -> JsNumber(a.carbon_chilled_water),
        "carbon_coal" -> JsNumber(a.carbon_coal)
      )))
      case None => Left("Could not recognize input type")
    }
  }



  val validator = new SchemaValidator()

  val schema = Json.fromJson[SchemaType](Json.parse(
    """{
        "type": "array",
        "id": "http://znc.maalka.com/znc",
        "items": [
          {
          "id": "/items",
          "type": "object",
          "properties": {
            "prop_types": {
               "id": "/items/properties/prop_types",
               "type": "array",
               "required":true,
               "items": {
                   "type": "object",
                   "minItems": 1,
                   "properties": {
                     "building_type": {
                     "type": "string",
                     "enum": ["OfficeLarge", "OfficeMedium", "OfficeSmall", "RetailStandalone", "RetailStripmall",
                             "SchoolPrimary", "SchoolSecondary", "Hospital", "OutPatientHealthCare",
                             "RestaurantSitDown", "RestaurantFastFood", "HotelLarge", "HotelSmall",
                             "Warehouse", "ApartmentHighRise", "ApartmentMidRise", "Office", "Retail", "School",
                             "Healthcare", "Restaurant", "Hotel", "Apartment", "Warehouse", "AllOthers"]
                     },
                     "floor_area": {
                      "type": "number",
                      "minimum": 0
                     },
                     "floor_area_units": {
                       "type": "string",
                       "enum": ["mSQ", "ftSQ"]
                     }
                   },
                   "required": [
                     "building_type",
                     "floor_area",
                     "floor_area_units"
                   ]
               }
             },
            "stories": {
              "type": "number",
              "minimum": 0,
              "required":true
            },
            "prescriptive_resource": {
              "type": "integer"
            },
            "reporting_units": {
              "type": "string",
              "enum": ["imperial", "metric"]
            },
            "pv_resource": {
              "type": "integer"
            },
            "approach": {
              "type": "string",
              "enum": ["prescriptive", "performance"]
            },
            "metric": {
              "id": "/items/properties/metric",
              "type": "object",
              "properties": {
                "metric_type": {
                  "type": "string",
                  "enum": ["site", "source", "carbon"]
                },
                "conversion_resource": {
                  "type": "integer",
                  "minimum": 0
                },
                "carbon_electricity": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_natural_gas": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_fuel_oil": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_propane": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_steam": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_hot_water": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_chilled_water": {
                  "type": "number",
                  "minimum": 0
                },
                "carbon_coal": {
                  "type": "number",
                  "minimum": 0
                },
                "source_electricity": {
                  "type": "number",
                  "minimum": 0
                },
                "source_natural_gas": {
                  "type": "number",
                  "minimum": 0
                },
                "source_fuel_oil": {
                  "type": "number",
                  "minimum": 0
                },
                "source_propane": {
                  "type": "number",
                  "minimum": 0
                },
                "source_steam": {
                  "type": "number",
                  "minimum": 0
                },
                "source_hot_water": {
                  "type": "number",
                  "minimum": 0
                },
                "source_chilled_water": {
                  "type": "number",
                  "minimum": 0
                },
                "source_coal": {
                  "type": "number",
                  "minimum": 0
                }
              }
            },
            "climate_zone": {
              "type": "string",
              "enum": ["1A", "1B", "2A", "2B", "3A", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"],
              "required": true
            },
            "file_id": {
              "type": "string",
              "required": true
            },
            "pv_data": {
              "id": "/items/properties/pv_data",
              "type": "array",
              "items": {
                "type": "object",
                "additionalProperties": false,
                "properties": {
                  "estimated_area": {
                    "minimum": 0,
                    "type": "number"
                  },
                  "pv_area_units": {
                    "minimum": 0,
                    "type": "string",
                    "enum": ["mSQ", "ftSQ"]
                  },
                  "access_perimeter": {
                    "minimum": 0,
                    "type": "number"
                  },
                  "w_per_meter2": {
                    "minimum": 0,
                    "type": "number"
                  },
                  "system_capacity": {
                    "minimum": 0.05,
                    "maximum": 10000.0,
                    "type": "number"
                  },
                  "module_type": {
                    "type": "integer",
                    "enum": [0,1,2]
                  },
                  "losses": {
                    "type": "number",
                    "minimum": -5,
                    "maximum": 99
                  },
                  "array_type": {
                    "type": "integer",
                    "enum": [0,1,2,3,4]
                  },
                  "tilt": {
                    "type": "number",
                    "minimum": 0,
                    "maximum": 90
                  },
                  "azimuth": {
                    "type": "number",
                    "minimum": 0,
                    "maximum": 360
                  },
                  "inv_eff": {
                    "type": "number",
                    "minimum": 90,
                    "maximum": 99.5
                  }
                }
              }
            },
            "energies": {
              "id": "/items/properties/energies",
              "type": "array",
              "items": {
                  "type": "object",
                  "properties": {
                    "energyName": {
                      "type": "string"
                    },
                    "energyType": {
                      "type": "string",
                      "enum": ["electricity","natural_gas","fuel_oil","propane","steam","hot_water","chilled_water","coal"]
                    },
                    "energyUnits": {
                      "type": "string",
                       "enum": ["KBtu","MBtu","kWh","MWh","GJ","NGMcf","NGKcf","NGCcf","NGcf", "NGm3","Therms","No1UKG","No1USG",
                             "No1L","No2UKG","No2USG","No2L","No4UKG","No4USG","No4L","No6UKG","No6USG","No6L","DieselUKG","DieselUSG",
                             "DieselL","KeroseneUKG","KeroseneUSG","KeroseneL","PropaneUKG","PropaneUSG","PropaneCf","PropaneCCf",
                             "PropaneKCf","PropaneL","SteamLb","SteamKLb","SteamMLb","CHWTonH","CoalATon","CoalATonne","CoalALb",
                             "CoalAKLb","CoalAMLb","CoalBitTon","CoalBitTonne","CoalBitLb","CoalBitKLb","CoalBitMLb","CokeTon","CokeTonne",
                             "CokeLb","CokeKLb","CokeMLb","WoodTon","WoodTonne"]
                    },
                    "energyUse": {
                      "type": "number",
                      "minimum": 0
                    }
                  },
                  "required": [
                    "energyName",
                    "energyType",
                    "energyUnits",
                    "energyUse"
                  ]
              }
            }
          },
          "required": ["file_id"]
          }
        ]
      }""".stripMargin)).get

  def getZEPIMetrics() = Action.async(parse.json) { implicit request =>

    val Baseline: EUIMetrics = EUIMetrics(request.body, nrel_client)

    val json: JsValue = request.body
    val result = validator.validate(schema, json)

    result.fold(
      invalid = { errors =>
        Future {
          BadRequest(errors.toJson)
        }
      },
      valid = { post =>

        val f1: Future[Either[String, JsValue]] = Baseline.getPV.map(api(_)).recover { case NonFatal(th) => apiRecover(th) }




        val getPV_Parameters: Future[Seq[Map[String, String]]] = f1.map { e =>
          e match {
            case Right(jsonArray: JsArray) => jsonArray.as[Seq[Map[String, JsValue]]].map { x => x.map { y => (y._1, y._2.toString())}}
            case _ => Seq.empty[Map[String, String]]
          }
        }

        val multipleFutures: Future[Seq[JsValue]] = getPV_Parameters.flatMap { params =>

          val a: Seq[Future[JsValue]] = params.map { p =>
            nrel_client.makeWsRequest(p.toSeq)
          }
          Future.sequence(a)
        }


        val f2 = multipleFutures.map { r =>
    
          val stationInfo = (r.head \ "station_info").get
          val version = (r.head \ "version").get
          val ac_annual: Double = r.map{a => (a \ "outputs" \ "ac_annual").as[Double]}.sum
          val capacity_factor = r.map{a => (a \ "outputs" \ "capacity_factor").as[Double]}.sum / r.map{a => (a \ "outputs" \ "capacity_factor").as[Double]}.length
          val solrad_annual = r.map{a => (a \ "outputs" \ "solrad_annual").as[Double]}.sum

          val arr1 =  r.map{a => (a \ "outputs" \ "ac_monthly").as[Array[Double]]}

          val ac_monthly = arr1.tail.fold(arr1.head) { (z, i) =>
            (z, i).zipped.map(_ + _)
          }

          val arr2 =  r.map{a => (a \ "outputs" \ "dc_monthly").as[Array[Double]]}

          val dc_monthly = arr2.tail.fold(arr2.head) { (z, i) =>
            (z, i).zipped.map(_ + _)
          }

          val arr3 =  r.map{a => (a \ "outputs" \ "poa_monthly").as[Array[Double]]}

          val poa_monthly = arr3.tail.fold(arr3.head) { (z, i) =>
            (z, i).zipped.map(_ + _)
          }

          val arr4 =  r.map{a => (a \ "outputs" \ "solrad_monthly").as[Array[Double]]}

          val solrad_monthly = arr4.tail.fold(arr4.head) { (z, i) =>
            (z, i).zipped.map(_ + _)
          }

          val a: JsValue = Json.obj(
            "outputs" -> Json.obj(
              "ac_annual" -> ac_annual,
              "capacity_factor" -> capacity_factor,
              "solrad_annual" -> solrad_annual,
              "ac_monthly" -> ac_monthly,
              "dc_monthly" -> dc_monthly,
              "poa_monthly" -> poa_monthly,
              "solrad_monthly" -> solrad_monthly
            ),
            "stationInfo" -> stationInfo,
            "version" -> version
          )
          Right(a)

        }

        val merged: Future[Either[String, JsValue]] = for {
          e1 <- f1
          e2 <- f2
        } yield (e1, e2) match {
          case (Left(s: String), _) => Left(s)
//          case (_, Left(s: String)) => Left(s)
          case (Right(j1:JsValue), Right(j2:JsValue)) => {
            Right(Json.obj("PVDefaults" -> j1).deepMerge(j2.as[JsObject]))
          }
        }


        val futures = Future.sequence(Seq(

          Baseline.getPrescriptiveTotalSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveTotalSiteIntensity.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveTotalSource.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveTotalSourceIntensity.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveTotalCarbon.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveTotalCarbonIntensity.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.getPrescriptiveEndUses.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveElectricity.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getPrescriptiveNG.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.getPrescriptiveEndUsePercents.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          merged.recover { case NonFatal(th) => apiRecover(th) } ,

          Baseline.getBuildingData.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getMetrics.map(api(_)).recover { case NonFatal(th) => apiRecover(th) }

        ))

        val fieldNames = Seq(
          "prototype_total_site_energy",
          "prototype_total_site_EUI",
          "prototype_total_source",
          "prototype_total_source_intensity",
          "prototype_total_carbon",
          "prototype_total_carbon_intensity",

          "prototype_end_use_metric_data",
          "prototype_electricity_metric_data",
          "prototype_natural_gas_metric_data",

          "prototype_end_use_metric_percents",

          "pv_system_details",

          "prop_types",
          "metrics_conversion_details"

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
    )
  }
}