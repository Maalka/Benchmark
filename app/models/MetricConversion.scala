package models

import java.io.InputStream

import play.{Environment, api}
import play.api.{Environment, Play}
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import squants.Energy
import squants.energy.{Energy, KBtus, MegawattHours}
import squants.space.{Area, SquareFeet, SquareMeters}



case class MetricConversion(parameters:JsValue) {

  //all conversions are from kbtu of a source
  val emissionsScaleFactor:Double = KBtus(1) to MegawattHours

  val source_0: Map[String,Double] = Map(
    "electricity" -> 3.15,
    "natural_gas" -> 1.09,
    "fuel_oil" -> 1.19,
    "propane" -> 1.15,
    "srteam" -> 1.45,
    "hot_water" -> 1.35,
    "chilled_water" -> 1.04,
    "coal" -> 1.05,
    "other" -> 1.05
   )

  val carbon_0: Map[String,Double] = Map(
    "electricity" -> 0.61 * emissionsScaleFactor,
    "natural_gas" -> 0.23 * emissionsScaleFactor,
    "fuel_oil" -> 0.30 * emissionsScaleFactor,
    "propane" -> 0.27 * emissionsScaleFactor,
    "steam" -> 0.39 * emissionsScaleFactor,
    "hot_water" -> 0.37 * emissionsScaleFactor,
    "chilled_water" -> 0.15 * emissionsScaleFactor,
    "coal" -> 0.37 * emissionsScaleFactor,
    "other" -> 0.37 * emissionsScaleFactor
  )
    

  def getConversionMetrics(metricType:Option[String]): Future[ValidatedConversionDetails] = Future {
  //all conversions are from kbtu of a source

    val metric_type = metricType match {
      case Some(a) => a
      case None => parameters.asOpt[ConversionMetrics] match {
        case Some(a) => a.metric.metric_type.getOrElse("site")
        case _ => "site"
      }
    }
    val conversion_resource:Int = parameters.asOpt[ConversionMetrics] match {
        case Some(a) => a.metric.conversion_resource.getOrElse(0)
        case _ => 0
      }

    parameters.asOpt[ConversionMetrics] match {
      case Some(a) => {
        metric_type match {
          case "source" => {
            val source_electricity = (a.metric.source_electricity, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("electricity")
              case (_, _) => source_0("electricity") // source_0 is the default
            }
            val source_natural_gas = (a.metric.source_natural_gas, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("natural_gas")
              case (_, _) => source_0("natural_gas") // source_0 is the default
            }
          }
            val source_fuel_oil = (a.metric.source_fuel_oil, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("fuel_oil")
              case (_, _) => source_0("fuel_oil") // source_0 is the default
            }
            val source_propane = (a.metric.source_propane, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("propane")
              case (_, _) => source_0("propane") // source_0 is the default
            }
            val source_steam = (a.metric.source_steam, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("steam")
              case (_, _) => source_0("steam") // source_0 is the default
            }
            val source_hot_water = (a.metric.source_hot_water, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("hot_water")
              case (_, _) => source_0("hot_water") // source_0 is the default
            }
            val source_chilled_water = (a.metric.source_chilled_water, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("chilled_water")
              case (_, _) => source_0("chilled_water") // source_0 is the default
            }
            val source_coal = (a.metric.source_coal, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("coal")
              case (_, _) => source_0("coal") // source_0 is the default
            }
            val source_other = (a.metric.source_other, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => source_0("other")
              case (_, _) => source_0("other") // source_0 is the default
            }
          case "carbon" =>
            val carbon_electricity = (a.metric.carbon_electricity, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("electricity")
              case (_, _) => carbon_0("electricity") // source_0 is the default
            }

            val carbon_natural_gas = (a.metric.carbon_natural_gas, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("natural_gas")
              case (_, _) => carbon_0("natural_gas") // carbon_0 is the default
            }

            val carbon_fuel_oil = (a.metric.carbon_fuel_oil, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("fuel_oil")
              case (_, _) => carbon_0("fuel_oil") // carbon_0 is the default
            }

            val carbon_propane = (a.metric.carbon_propane, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("propane")
              case (_, _) => carbon_0("propane") // carbon_0 is the default

            val carbon_steam = (a.metric.carbon_steam, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("steam")
              case (_, _) => carbon_0("steam") // carbon_0 is the default
            }

            val carbon_hot_water = (a.metric.carbon_hot_water, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("hot_water")
              case (_, _) => carbon_0("hot_water") // carbon_0 is the default
            }

            val carbon_chilled_water = (a.metric.carbon_chilled_water, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("chilled_water")
              case (_, _) => carbon_0("chilled_water") // carbon_0 is the default
            }

            val carbon_coal = (a.metric.carbon_coal, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("coal")
              case (_, _) => carbon_0("coal") // carbon_0 is the default
            }

            val carbon_other = (a.metric.carbon_other, conversion_resource) match {
              case (Some(b), _) => b
              case (_, 0) => carbon_0("other")
              case (_, _) => carbon_0("other") // carbon_0 is the default
        }
                
            ValidatedConversionDetails(
              metric_type,
              conversion_resource,
              Option(carbon_electricity).getOrElse(1.0),
              Option(carbon_natural_gas).getOrElse(1.0),
              Option(carbon_fuel_oil).getOrElse(1.0),
              Option(carbon_propane).getOrElse(1.0),
              Option(carbon_steam).getOrElse(1.0),
              Option(carbon_hot_water).getOrElse(1.0),
              Option(carbon_chilled_water).getOrElse(1.0),
              Option(carbon_coal).getOrElse(1.0),
              Option(carbon_other).getOrElse(1.0),
              Option(source_electricity).getOrElse(1.0),
              Option(source_natural_gas).getOrElse(1.0),
              Option(source_fuel_oil).getOrElse(1.0),
              Option(source_propane).getOrElse(1.0),
              Option(source_steam).getOrElse(1.0),
              Option(source_hot_water).getOrElse(1.0),
              Option(source_chilled_water).getOrElse(1.0),
              Option(source_coal).getOrElse(1.0),
              Option(source_other).getOrElse(1.0)
            )
          }
        }
      }

      
    }
  }


  def convertMetrics[T](energyEntry:T, energyType:Option[String], conversionFactors:ValidatedConversionDetails, metricType:Option[String]):Future[T] = Future {
    // ALL ENERGY VALUES PASSED AS KBTU BASE INPUT

    val metric = metricType match {
      case Some(temp) => temp
      case None => conversionFactors.metric_type match {
        case a: String => a
        case _ => "site"
      }
    }


    val converted = (energyEntry, metric) match {
      case (b: ElectricityDistribution, "source") => {
        ElectricityDistribution(
          b.elec_htg * conversionFactors.source_electricity,
          b.elec_clg * conversionFactors.source_electricity,
          b.elec_intLgt * conversionFactors.source_electricity,
          b.elec_extLgt * conversionFactors.source_electricity,
          b.elec_intEqp * conversionFactors.source_electricity,
          b.elec_extEqp * conversionFactors.source_electricity,
          b.elec_fans * conversionFactors.source_electricity,
          b.elec_pumps * conversionFactors.source_electricity,
          b.elec_heatRej * conversionFactors.source_electricity,
          b.elec_humid * conversionFactors.source_electricity,
          b.elec_heatRec * conversionFactors.source_electricity,
          b.elec_swh * conversionFactors.source_electricity,
          b.elec_refrg * conversionFactors.source_electricity,
          b.elec_gentor * conversionFactors.source_electricity,
          b.elec_net * conversionFactors.source_electricity
        )
      }
      case (b: ElectricityDistribution, "carbon") => {
        ElectricityDistribution(
          b.elec_htg * conversionFactors.carbon_electricity,
          b.elec_clg * conversionFactors.carbon_electricity,
          b.elec_intLgt * conversionFactors.carbon_electricity,
          b.elec_extLgt * conversionFactors.carbon_electricity,
          b.elec_intEqp * conversionFactors.carbon_electricity,
          b.elec_extEqp * conversionFactors.carbon_electricity,
          b.elec_fans * conversionFactors.carbon_electricity,
          b.elec_pumps * conversionFactors.carbon_electricity,
          b.elec_heatRej * conversionFactors.carbon_electricity,
          b.elec_humid * conversionFactors.carbon_electricity,
          b.elec_heatRec * conversionFactors.carbon_electricity,
          b.elec_swh * conversionFactors.carbon_electricity,
          b.elec_refrg * conversionFactors.carbon_electricity,
          b.elec_gentor * conversionFactors.carbon_electricity,
          b.elec_net * conversionFactors.carbon_electricity
        )
      }
      case (b: NaturalGasDistribution, "source") => {
        NaturalGasDistribution(
          b.ng_htg * conversionFactors.source_natural_gas,
          b.ng_clg * conversionFactors.source_natural_gas,
          b.ng_intLgt * conversionFactors.source_natural_gas,
          b.ng_extLgt * conversionFactors.source_natural_gas,
          b.ng_intEqp * conversionFactors.source_natural_gas,
          b.ng_extEqp * conversionFactors.source_natural_gas,
          b.ng_fans * conversionFactors.source_natural_gas,
          b.ng_pumps * conversionFactors.source_natural_gas,
          b.ng_heatRej * conversionFactors.source_natural_gas,
          b.ng_humid * conversionFactors.source_natural_gas,
          b.ng_heatRec * conversionFactors.source_natural_gas,
          b.ng_swh * conversionFactors.source_natural_gas,
          b.ng_refrg * conversionFactors.source_natural_gas,
          b.ng_gentor * conversionFactors.source_natural_gas,
          b.ng_net * conversionFactors.source_natural_gas
        )
      }
      case (b: NaturalGasDistribution, "carbon") => {
        NaturalGasDistribution(
          b.ng_htg * conversionFactors.carbon_natural_gas,
          b.ng_clg * conversionFactors.carbon_natural_gas,
          b.ng_intLgt * conversionFactors.carbon_natural_gas,
          b.ng_extLgt * conversionFactors.carbon_natural_gas,
          b.ng_intEqp * conversionFactors.carbon_natural_gas,
          b.ng_extEqp * conversionFactors.carbon_natural_gas,
          b.ng_fans * conversionFactors.carbon_natural_gas,
          b.ng_pumps * conversionFactors.carbon_natural_gas,
          b.ng_heatRej * conversionFactors.carbon_natural_gas,
          b.ng_humid * conversionFactors.carbon_natural_gas,
          b.ng_heatRec * conversionFactors.carbon_natural_gas,
          b.ng_swh * conversionFactors.carbon_natural_gas,
          b.ng_refrg * conversionFactors.carbon_natural_gas,
          b.ng_gentor * conversionFactors.carbon_natural_gas,
          b.ng_net * conversionFactors.carbon_natural_gas
        )
      }

      case (b: ValidatedEnergy, "source") => {
        val energyValue = energyType match {
          case Some("electricity") => (b.energyValue ) * conversionFactors.source_electricity
          case Some("natural_gas") => (b.energyValue ) * conversionFactors.source_natural_gas
          case Some("fuel_oil") => (b.energyValue ) * conversionFactors.source_fuel_oil
          case Some("propane") => (b.energyValue ) * conversionFactors.source_propane
          case Some("steam") => (b.energyValue ) * conversionFactors.source_steam
          case Some("hot_water") => (b.energyValue ) * conversionFactors.source_hot_water
          case Some("chilled_water") => (b.energyValue ) * conversionFactors.source_chilled_water
          case Some("coal") => (b.energyValue ) * conversionFactors.source_coal
          case _ => throw new Exception("Cannot Convert Energy Value to Source Energy - Type Unkown")
        }
        ValidatedEnergy(b.energyType,b.energyName,energyValue)
      }
      case (b: ValidatedEnergy, "carbon") => {
        val energyValue = energyType match {
          case Some("electricity") => (b.energyValue ) * conversionFactors.carbon_electricity
          case Some("natural_gas") => (b.energyValue ) * conversionFactors.carbon_natural_gas
          case Some("fuel_oil") => (b.energyValue ) * conversionFactors.carbon_fuel_oil
          case Some("propane") => (b.energyValue ) * conversionFactors.carbon_propane
          case Some("steam") => (b.energyValue ) * conversionFactors.carbon_steam
          case Some("hot_water") => (b.energyValue ) * conversionFactors.carbon_hot_water
          case Some("chilled_water") => (b.energyValue ) * conversionFactors.carbon_chilled_water
          case Some("coal") => (b.energyValue ) * conversionFactors.carbon_coal
          case _ => throw new Exception("Cannot Convert Energy Value to Carbon - Type Unkown")
        }
        ValidatedEnergy(b.energyType,b.energyName,energyValue)
      }
      case (b: Double, "source") => {
        energyType match {
          case Some("electricity") => b * conversionFactors.source_electricity
          case Some("natural_gas") => b * conversionFactors.source_natural_gas
          case Some("fuel_oil") => b * conversionFactors.source_fuel_oil
          case Some("propane") => b * conversionFactors.source_propane
          case Some("steam") => b * conversionFactors.source_steam
          case Some("hot_water") => b * conversionFactors.source_hot_water
          case Some("chilled_water") => b * conversionFactors.source_chilled_water
          case Some("coal") => b * conversionFactors.source_coal
          case _ => throw new Exception("Cannot Convert Energy (Double) Value to Source - Type Unkown")
        }
      }
      case (b: Double, "carbon") => {
        energyType match {
          case Some("electricity") => b * conversionFactors.carbon_electricity
          case Some("natural_gas") => b * conversionFactors.carbon_natural_gas
          case Some("fuel_oil") => b * conversionFactors.carbon_fuel_oil
          case Some("propane") => b * conversionFactors.carbon_propane
          case Some("steam") => b * conversionFactors.carbon_steam
          case Some("hot_water") => b * conversionFactors.carbon_hot_water
          case Some("chilled_water") => b * conversionFactors.carbon_chilled_water
          case Some("coal") => b * conversionFactors.carbon_coal
          case _ => throw new Exception("Cannot Convert Energy (Double) Value to Carbon - Type Unkown")
        }
      }
      case (b: EndUseDistribution, _) => b
      case (b: Any, _) => b
      case (_, _) => throw new Exception("Cannot find correct conversion factors!")
    }
    converted.asInstanceOf[T]
  }


}


case class ConversionMetrics(metric: ConversionDetails)

object ConversionMetrics {
  implicit val ConversionMetricsReads: Reads[ConversionMetrics] = Json.reads[ConversionMetrics]
}
case class ConversionDetails(
                              metric_type: Option[String],
                              conversion_resource: Option[Int],
                              carbon_electricity: Option[Double],
                              carbon_natural_gas: Option[Double],
                              carbon_fuel_oil: Option[Double],
                              carbon_propane: Option[Double],
                              carbon_steam: Option[Double],
                              carbon_hot_water: Option[Double],
                              carbon_chilled_water: Option[Double],
                              carbon_coal: Option[Double],
                              carbon_other: Option[Double],
                              source_electricity: Option[Double],
                              source_natural_gas: Option[Double],
                              source_fuel_oil: Option[Double],
                              source_propane: Option[Double],
                              source_steam: Option[Double],
                              source_hot_water: Option[Double],
                              source_chilled_water: Option[Double],
                              source_coal: Option[Double],
                              source_other: Option[Double]
                            )

object ConversionDetails {
  implicit val ConversionDetailsReads: Reads[ConversionDetails] = Json.reads[ConversionDetails]
}


  case class ValidatedConversionDetails(
                                      metric_type: String,
                                      conversion_resource: Int,
                                      carbon_electricity: Double,
                                      carbon_natural_gas: Double,
                                      carbon_fuel_oil: Double,
                                      carbon_propane: Double,
                                      carbon_steam: Double,
                                      carbon_hot_water: Double,
                                      carbon_chilled_water: Double,
                                      carbon_coal: Double,
                                      carbon_other: Double,
                                      source_electricity: Double,
                                      source_natural_gas: Double,
                                      source_fuel_oil: Double,
                                      source_propane: Double,
                                      source_steam: Double,
                                      source_hot_water: Double,
                                      source_chilled_water: Double,
                                      source_coal: Double,
                                      source_other: Double
                                       )
object ValidatedConversionDetails {
  implicit val ValidatedConversionDetailsReads: Reads[ValidatedConversionDetails] = Json.reads[ValidatedConversionDetails]
}
