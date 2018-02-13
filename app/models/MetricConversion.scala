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


  def getConversionMetrics(metricType:Option[String]): Future[ValidatedConversionDetails] = Future {

    parameters.asOpt[ConversionMetrics] match {
      case Some(a) => {
        a.metric.conversion_resource match {
          /*default is conversion_resource 0, so add all other cases for different lookups here
           case <new conversion resource table integer> => {
            val conversion_resource = 0
            val metric_type = a.metric.metric_type match {
              case Some(met) => met
              case _ => "site"
          */
          case _ => {
            val conversion_resource = 0
            val metric_type = metricType match {
              case Some(temp) => temp
              case None => a.metric.metric_type match {
                case Some(met) => met
                case _ => "site"
              }
            }
            val source_electricity = (a.metric.source_electricity, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 3.15
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_electricity = (a.metric.carbon_electricity, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.61
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_natural_gas = (a.metric.source_natural_gas, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.09
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_natural_gas = (a.metric.carbon_natural_gas, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.23
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_fuel_oil = (a.metric.source_fuel_oil, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.19
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_fuel_oil = (a.metric.carbon_fuel_oil, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.30
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_propane = (a.metric.source_propane, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.15
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_propane = (a.metric.carbon_propane, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.27
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_steam = (a.metric.source_steam, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.45
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_steam = (a.metric.carbon_steam, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.39
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_hot_water = (a.metric.source_hot_water, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.35
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_hot_water = (a.metric.carbon_hot_water, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.37
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_chilled_water = (a.metric.source_chilled_water, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.04
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_chilled_water = (a.metric.carbon_chilled_water, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.15
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val source_coal = (a.metric.source_coal, metric_type) match {
              case (Some(b), "source") => b
              case (_, "source") => 1.05
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            val carbon_coal = (a.metric.carbon_coal, metric_type) match {
              case (Some(b), "carbon") => b
              case (_, "carbon") => 0.37
              case (_, "site") => 1.0
              case (_, _) => 1.0
            }
            ValidatedConversionDetails(
              metric_type,
              conversion_resource,
              carbon_electricity,
              carbon_natural_gas,
              carbon_fuel_oil,
              carbon_propane,
              carbon_steam,
              carbon_hot_water,
              carbon_chilled_water,
              carbon_coal,
              source_electricity,
              source_natural_gas,
              source_fuel_oil,
              source_propane,
              source_steam,
              source_hot_water,
              source_chilled_water,
              source_coal
            )
          }
        }
      }

      case _ => ValidatedConversionDetails("site", 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    }
  }


  def convertMetrics[T](energyEntry:T, energyType:Option[String], conversionFactors:ValidatedConversionDetails, metricType:Option[String]):Future[T] = Future {
    // If metric=carbon, the values should be tonnes of CO2e emissions per MWh of energy used.
    // ALL ENERGY VALUES PASSED AS KBTU ON INPUT

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
        println(conversionFactors)
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

      case (b: Energy, "source") => {
        energyType match {
          case Some("electricity") => KBtus((b to KBtus) * conversionFactors.source_electricity)
          case Some("natural_gas") => KBtus((b to KBtus) * conversionFactors.source_natural_gas)
          case Some("fuel_oil") => KBtus((b to KBtus) * conversionFactors.source_fuel_oil)
          case Some("propane") => KBtus((b to KBtus) * conversionFactors.source_propane)
          case Some("steam") => KBtus((b to KBtus) * conversionFactors.source_steam)
          case Some("hot_water") => KBtus((b to KBtus) * conversionFactors.source_hot_water)
          case Some("chilled_water") => KBtus((b to KBtus) * conversionFactors.source_chilled_water)
          case Some("coal") => KBtus((b to KBtus) * conversionFactors.source_coal)
          case _ => throw new Exception("Cannot Convert Energy Value - Type Unkown")
        }
      }
      case (b: Energy, "carbon") => {
        energyType match {
          case Some("electricity") => KBtus((b to KBtus) * conversionFactors.carbon_electricity)
          case Some("natural_gas") => KBtus((b to KBtus) * conversionFactors.carbon_natural_gas)
          case Some("fuel_oil") => KBtus((b to KBtus) * conversionFactors.carbon_fuel_oil)
          case Some("propane") => KBtus((b to KBtus) * conversionFactors.carbon_propane)
          case Some("steam") => KBtus((b to KBtus) * conversionFactors.carbon_steam)
          case Some("hot_water") => KBtus((b to KBtus) * conversionFactors.carbon_hot_water)
          case Some("chilled_water") => KBtus((b to KBtus) * conversionFactors.carbon_chilled_water)
          case Some("coal") => KBtus((b to KBtus) * conversionFactors.carbon_coal)
          case _ => throw new Exception("Cannot Convert Energy Value - Type Unkown")
        }
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
          case _ => throw new Exception("Cannot Convert Energy (Double) Value - Type Unkown")
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
          case _ => throw new Exception("Cannot Convert Energy (Double) Value - Type Unkown")
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
                              source_electricity: Option[Double],
                              source_natural_gas: Option[Double],
                              source_fuel_oil: Option[Double],
                              source_propane: Option[Double],
                              source_steam: Option[Double],
                              source_hot_water: Option[Double],
                              source_chilled_water: Option[Double],
                              source_coal: Option[Double]
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
                              source_electricity: Double,
                              source_natural_gas: Double,
                              source_fuel_oil: Double,
                              source_propane: Double,
                              source_steam: Double,
                              source_hot_water: Double,
                              source_chilled_water: Double,
                              source_coal: Double
                                       )
object ValidatedConversionDetails {
  implicit val ValidatedConversionDetailsReads: Reads[ValidatedConversionDetails] = Json.reads[ValidatedConversionDetails]
}
