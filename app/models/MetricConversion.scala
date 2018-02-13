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


  def getConversionMetrics: Future[List[ValidatedConversionDetails]] = Future.sequence {


      parameters.asOpt[ConversionMetrics] match {
        case Some(a) => {
          List(
            getConversionMetricsbyType(Some(a), "site"),
            getConversionMetricsbyType(Some(a), "source"),
            getConversionMetricsbyType(Some(a), "carbon")
          )
        }
        case None => {
          List(
            Future{ValidatedConversionDetails("site", 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)},
            Future{ValidatedConversionDetails("site", 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)},
            Future{ValidatedConversionDetails("site", 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)}
          )
        }
      }
  }


    def getConversionMetricsbyType(conversionMetrics: Option[ConversionMetrics],metric_type:String): Future[ValidatedConversionDetails] = Future {

    conversionMetrics match {
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
            //val metric_type = a.metric.metric_type match {
              //case Some(met) => met
              //case _ => "site"
            //}
            val metric_electricity = (a.metric.metric_electricity, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 3.15
              case (_, "carbon") => 0.61
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_natural_gas = (a.metric.metric_natural_gas, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.09
              case (_, "carbon") => 0.23
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_fuel_oil = (a.metric.metric_fuel_oil, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.19
              case (_, "carbon") => 0.30
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_propane = (a.metric.metric_propane, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.15
              case (_, "carbon") => 0.27
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_steam = (a.metric.metric_steam, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.45
              case (_, "carbon") => 0.39
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_hot_water = (a.metric.metric_hot_water, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.35
              case (_, "carbon") => 0.37
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_chilled_water = (a.metric.metric_chilled_water, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.04
              case (_, "carbon") => 0.15
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            val metric_coal = (a.metric.metric_coal, metric_type) match {
              case (Some(b), "source") => b
              case (Some(b), "carbon") => b
              case (_, "source") => 1.05
              case (_, "carbon") => 0.37
              case (_, "site") => 1.0
              case (_, _) => 1.0

            }
            ValidatedConversionDetails(
              metric_type, conversion_resource, metric_electricity, metric_natural_gas, metric_fuel_oil, metric_propane,
              metric_steam, metric_hot_water, metric_chilled_water, metric_coal)
          }
        }
      }

      case _ => ValidatedConversionDetails("site", 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    }
  }


  def convertMetrics[T](energyEntry:T, energyType:Option[String], conversionFactors:ValidatedConversionDetails):Future[T] = Future {
    // If metric=carbon, the values should be tonnes of CO2e emissions per MWh of energy used.
    // ALL ENERGY VALUES PASSED AS KBTU ON INPUT


    val converted = energyEntry match {
      case b: ElectricityDistribution => {
        ElectricityDistribution(
          b.elec_htg * conversionFactors.metric_electricity,
          b.elec_clg * conversionFactors.metric_electricity,
          b.elec_intLgt * conversionFactors.metric_electricity,
          b.elec_extLgt * conversionFactors.metric_electricity,
          b.elec_intEqp * conversionFactors.metric_electricity,
          b.elec_extEqp * conversionFactors.metric_electricity,
          b.elec_fans * conversionFactors.metric_electricity,
          b.elec_pumps * conversionFactors.metric_electricity,
          b.elec_heatRej * conversionFactors.metric_electricity,
          b.elec_humid * conversionFactors.metric_electricity,
          b.elec_heatRec * conversionFactors.metric_electricity,
          b.elec_swh * conversionFactors.metric_electricity,
          b.elec_refrg * conversionFactors.metric_electricity,
          b.elec_gentor * conversionFactors.metric_electricity,
          b.elec_net * conversionFactors.metric_electricity,
          b.site_EUI * conversionFactors.metric_electricity
        )
      }
      case b: NaturalGasDistribution => {
        NaturalGasDistribution(
          b.ng_htg * conversionFactors.metric_natural_gas,
          b.ng_clg * conversionFactors.metric_natural_gas,
          b.ng_intLgt * conversionFactors.metric_natural_gas,
          b.ng_extLgt * conversionFactors.metric_natural_gas,
          b.ng_intEqp * conversionFactors.metric_natural_gas,
          b.ng_extEqp * conversionFactors.metric_natural_gas,
          b.ng_fans * conversionFactors.metric_natural_gas,
          b.ng_pumps * conversionFactors.metric_natural_gas,
          b.ng_heatRej * conversionFactors.metric_natural_gas,
          b.ng_humid * conversionFactors.metric_natural_gas,
          b.ng_heatRec * conversionFactors.metric_natural_gas,
          b.ng_swh * conversionFactors.metric_natural_gas,
          b.ng_refrg * conversionFactors.metric_natural_gas,
          b.ng_gentor * conversionFactors.metric_natural_gas,
          b.ng_net * conversionFactors.metric_natural_gas,
          b.site_EUI * conversionFactors.metric_natural_gas
        )
      }
      case b: EndUseDistribution => b
      case b: Energy => {
        energyType match {
          case Some("electricity") => KBtus((b to KBtus) * conversionFactors.metric_electricity)
          case Some("natural_gas") => KBtus((b to KBtus) * conversionFactors.metric_natural_gas)
          case Some("fuel_oil") => KBtus((b to KBtus) * conversionFactors.metric_fuel_oil)
          case Some("propane") => KBtus((b to KBtus) * conversionFactors.metric_propane)
          case Some("steam") => KBtus((b to KBtus) * conversionFactors.metric_steam)
          case Some("hot_water") => KBtus((b to KBtus) * conversionFactors.metric_hot_water)
          case Some("chilled_water") => KBtus((b to KBtus) * conversionFactors.metric_chilled_water)
          case Some("coal") => KBtus((b to KBtus) * conversionFactors.metric_coal)
          case _ => throw new Exception("Cannot Convert Energy Value - Type Unkown")
        }
      }
      case b: Double => {
        energyType match {
          case Some("electricity") => b * conversionFactors.metric_electricity
          case Some("natural_gas") => b * conversionFactors.metric_natural_gas
          case Some("fuel_oil") => b * conversionFactors.metric_fuel_oil
          case Some("propane") => b * conversionFactors.metric_propane
          case Some("steam") => b * conversionFactors.metric_steam
          case Some("hot_water") => b * conversionFactors.metric_hot_water
          case Some("chilled_water") => b * conversionFactors.metric_chilled_water
          case Some("coal") => b * conversionFactors.metric_coal
          case _ => throw new Exception("Cannot Convert Energy (Double) Value - Type Unkown")
        }
      }
    }
    converted.asInstanceOf[T]
  }
}


case class ConversionMetrics(metric: ConversionDetails)

object ConversionMetrics {
  implicit val ConversionMetricsReads: Reads[ConversionMetrics] = Json.reads[ConversionMetrics]
}
case class ConversionDetails(
                              //metric_type: Option[String],
                              conversion_resource: Option[Int],
                              metric_electricity: Option[Double],
                              metric_natural_gas: Option[Double],
                              metric_fuel_oil: Option[Double],
                              metric_propane: Option[Double],
                              metric_steam: Option[Double],
                              metric_hot_water: Option[Double],
                              metric_chilled_water: Option[Double],
                              metric_coal: Option[Double]
                            )

object ConversionDetails {
  implicit val ConversionDetailsReads: Reads[ConversionDetails] = Json.reads[ConversionDetails]
}


  case class ValidatedConversionDetails(
                              metric_type: String,
                              conversion_resource: Int,
                              metric_electricity: Double,
                              metric_natural_gas: Double,
                              metric_fuel_oil: Double,
                              metric_propane: Double,
                              metric_steam: Double,
                              metric_hot_water: Double,
                              metric_chilled_water: Double,
                              metric_coal: Double)

object ValidatedConversionDetails {
  implicit val ValidatedConversionDetailsReads: Reads[ValidatedConversionDetails] = Json.reads[ValidatedConversionDetails]
}
