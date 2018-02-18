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
  val emissionsScaleFactor: Double = KBtus(1) to MegawattHours

  val source_0: Map[String, Double] = Map(
    "electricity" -> 3.15,
    "natural_gas" -> 1.09,
    "fuel_oil" -> 1.19,
    "propane" -> 1.15,
    "steam" -> 1.45,
    "hot_water" -> 1.35,
    "chilled_water" -> 1.04,
    "coal" -> 1.05,
    "other" -> 1.05
  )

  val carbon_0: Map[String, Double] = Map(
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


  def getConversionMetrics(metricType: Option[String]): Future[ValidatedConversionDetails] = Future {
    //all conversions are from kbtu of a source

    val metric_type = metricType match {
      case Some(a) => a
      case None => parameters.asOpt[ConversionMetrics] match {
        case Some(a) => a.metric.metric_type.getOrElse("site")
        case _ => "site"
      }
    }
    val conversion_resource: Int = parameters.asOpt[ConversionMetrics] match {
      case Some(a) => a.metric.conversion_resource.getOrElse(0)
      case _ => 0
    }

    parameters.asOpt[ConversionMetrics] match {
      case Some(a) => {
        /* ------------------------------------SOURCE ---------------------------------------- */
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
        /* ------------------------------------CARBON ---------------------------------------- */
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
        }

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

        val sourceMap: Map[String, Double] = Map(
          "electricity" -> Option(source_electricity).getOrElse(1.0),
          "natural_gas" -> Option(source_natural_gas).getOrElse(1.0),
          "fuel_oil" -> Option(source_fuel_oil).getOrElse(1.0),
          "propane" -> Option(source_propane).getOrElse(1.0),
          "steam" -> Option(source_steam).getOrElse(1.0),
          "hot_water" -> Option(source_hot_water).getOrElse(1.0),
          "chilled_water" -> Option(source_chilled_water).getOrElse(1.0),
          "coal" -> Option(source_coal).getOrElse(1.0),
          "other" -> Option(source_other).getOrElse(1.0)
        )

        val carbonMap: Map[String, Double] = Map(
          "electricity" -> Option(carbon_electricity).getOrElse(1.0),
          "natural_gas" -> Option(carbon_natural_gas).getOrElse(1.0),
          "fuel_oil" -> Option(carbon_fuel_oil).getOrElse(1.0),
          "propane" -> Option(carbon_propane).getOrElse(1.0),
          "steam" -> Option(carbon_steam).getOrElse(1.0),
          "hot_water" -> Option(carbon_hot_water).getOrElse(1.0),
          "chilled_water" -> Option(carbon_chilled_water).getOrElse(1.0),
          "coal" -> Option(carbon_coal).getOrElse(1.0),
          "other" -> Option(carbon_other).getOrElse(1.0)
        )


        ValidatedConversionDetails(
          metric_type,
          conversion_resource,
          sourceMap,
          carbonMap
        )

      }
      case _ => {

        val sourceMap: Map[String, Double] = Map(
          "electricity" -> source_0("electricity"),
          "natural_gas" -> source_0("natural_gas"),
          "fuel_oil" -> source_0("fuel_oil"),
          "propane" -> source_0("propane"),
          "steam" -> source_0("steam"),
          "hot_water" -> source_0("hot_water"),
          "chilled_water" -> source_0("chilled_water"),
          "coal" -> source_0("coal"),
          "other" -> source_0("other")
        )

        val carbonMap: Map[String, Double] = Map(
          "electricity" -> carbon_0("electricity"),
          "natural_gas" -> carbon_0("natural_gas"),
          "fuel_oil" -> carbon_0("fuel_oil"),
          "propane" -> carbon_0("propane"),
          "steam" -> carbon_0("steam"),
          "hot_water" -> carbon_0("hot_water"),
          "chilled_water" -> carbon_0("chilled_water"),
          "coal" -> carbon_0("coal"),
          "other" -> carbon_0("other")
        )

        ValidatedConversionDetails(
          metric_type,
          conversion_resource,
          sourceMap,
          carbonMap
        )

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
          b.elec_htg * conversionFactors.source("electricity"),
          b.elec_clg * conversionFactors.source("electricity"),
          b.elec_intLgt * conversionFactors.source("electricity"),
          b.elec_extLgt * conversionFactors.source("electricity"),
          b.elec_intEqp * conversionFactors.source("electricity"),
          b.elec_extEqp * conversionFactors.source("electricity"),
          b.elec_fans * conversionFactors.source("electricity"),
          b.elec_pumps * conversionFactors.source("electricity"),
          b.elec_heatRej * conversionFactors.source("electricity"),
          b.elec_humid * conversionFactors.source("electricity"),
          b.elec_heatRec * conversionFactors.source("electricity"),
          b.elec_swh * conversionFactors.source("electricity"),
          b.elec_refrg * conversionFactors.source("electricity"),
          b.elec_gentor * conversionFactors.source("electricity"),
          b.elec_net * conversionFactors.source("electricity")
        )
      }
      case (b: ElectricityDistribution, "carbon") => {
        ElectricityDistribution(
          b.elec_htg * conversionFactors.carbon("electricity"),
          b.elec_clg * conversionFactors.carbon("electricity"),
          b.elec_intLgt * conversionFactors.carbon("electricity"),
          b.elec_extLgt * conversionFactors.carbon("electricity"),
          b.elec_intEqp * conversionFactors.carbon("electricity"),
          b.elec_extEqp * conversionFactors.carbon("electricity"),
          b.elec_fans * conversionFactors.carbon("electricity"),
          b.elec_pumps * conversionFactors.carbon("electricity"),
          b.elec_heatRej * conversionFactors.carbon("electricity"),
          b.elec_humid * conversionFactors.carbon("electricity"),
          b.elec_heatRec * conversionFactors.carbon("electricity"),
          b.elec_swh * conversionFactors.carbon("electricity"),
          b.elec_refrg * conversionFactors.carbon("electricity"),
          b.elec_gentor * conversionFactors.carbon("electricity"),
          b.elec_net * conversionFactors.carbon("electricity")
        )
      }
      case (b: NaturalGasDistribution, "source") => {
        NaturalGasDistribution(
          b.ng_htg * conversionFactors.source("natural_gas"),
          b.ng_clg * conversionFactors.source("natural_gas"),
          b.ng_intLgt * conversionFactors.source("natural_gas"),
          b.ng_extLgt * conversionFactors.source("natural_gas"),
          b.ng_intEqp * conversionFactors.source("natural_gas"),
          b.ng_extEqp * conversionFactors.source("natural_gas"),
          b.ng_fans * conversionFactors.source("natural_gas"),
          b.ng_pumps * conversionFactors.source("natural_gas"),
          b.ng_heatRej * conversionFactors.source("natural_gas"),
          b.ng_humid * conversionFactors.source("natural_gas"),
          b.ng_heatRec * conversionFactors.source("natural_gas"),
          b.ng_swh * conversionFactors.source("natural_gas"),
          b.ng_refrg * conversionFactors.source("natural_gas"),
          b.ng_gentor * conversionFactors.source("natural_gas"),
          b.ng_net * conversionFactors.source("natural_gas")
        )
      }
      case (b: NaturalGasDistribution, "carbon") => {
        NaturalGasDistribution(
          b.ng_htg * conversionFactors.carbon("natural_gas"),
          b.ng_clg * conversionFactors.carbon("natural_gas"),
          b.ng_intLgt * conversionFactors.carbon("natural_gas"),
          b.ng_extLgt * conversionFactors.carbon("natural_gas"),
          b.ng_intEqp * conversionFactors.carbon("natural_gas"),
          b.ng_extEqp * conversionFactors.carbon("natural_gas"),
          b.ng_fans * conversionFactors.carbon("natural_gas"),
          b.ng_pumps * conversionFactors.carbon("natural_gas"),
          b.ng_heatRej * conversionFactors.carbon("natural_gas"),
          b.ng_humid * conversionFactors.carbon("natural_gas"),
          b.ng_heatRec * conversionFactors.carbon("natural_gas"),
          b.ng_swh * conversionFactors.carbon("natural_gas"),
          b.ng_refrg * conversionFactors.carbon("natural_gas"),
          b.ng_gentor * conversionFactors.carbon("natural_gas"),
          b.ng_net * conversionFactors.carbon("natural_gas")
        )
      }

      case (b: ValidatedEnergy, "source") => {
        val energyValue = energyType match {
          case Some("electricity") => (b.energyValue ) * conversionFactors.source("electricity")
          case Some("natural_gas") => (b.energyValue ) * conversionFactors.source("natural_gas")
          case Some("fuel_oil") => (b.energyValue ) * conversionFactors.source("fuel_oil")
          case Some("propane") => (b.energyValue ) * conversionFactors.source("propane")
          case Some("steam") => (b.energyValue ) * conversionFactors.source("steam")
          case Some("hot_water") => (b.energyValue ) * conversionFactors.source("hot_water")
          case Some("chilled_water") => (b.energyValue ) * conversionFactors.source("chilled_water")
          case Some("coal") => (b.energyValue ) * conversionFactors.source("coal")
          case Some(_) => (b.energyValue ) * conversionFactors.source("other")
          case _ => throw new Exception("Cannot Convert Energy Value to Source Energy - Type Unkown")
        }
        ValidatedEnergy(b.energyType,b.energyName,energyValue)
      }
      case (b: ValidatedEnergy, "carbon") => {
        val energyValue = energyType match {
          case Some("electricity") => (b.energyValue ) * conversionFactors.carbon("electricity")
          case Some("natural_gas") => (b.energyValue ) * conversionFactors.carbon("natural_gas")
          case Some("fuel_oil") => (b.energyValue ) * conversionFactors.carbon("fuel_oil")
          case Some("propane") => (b.energyValue ) * conversionFactors.carbon("propane")
          case Some("steam") => (b.energyValue ) * conversionFactors.carbon("steam")
          case Some("hot_water") => (b.energyValue ) * conversionFactors.carbon("hot_water")
          case Some("chilled_water") => (b.energyValue ) * conversionFactors.carbon("chilled_water")
          case Some("coal") => (b.energyValue ) * conversionFactors.carbon("coal")
          case Some(_) => (b.energyValue ) * conversionFactors.carbon("other")
          case _ => throw new Exception("Cannot Convert Energy Value to Carbon - Type Unkown")
        }
        ValidatedEnergy(b.energyType,b.energyName,energyValue)
      }
      case (b: Double, "source") => {
        energyType match {
          case Some("electricity") => b * conversionFactors.source("electricity")
          case Some("natural_gas") => b * conversionFactors.source("natural_gas")
          case Some("fuel_oil") => b * conversionFactors.source("fuel_oil")
          case Some("propane") => b * conversionFactors.source("propane")
          case Some("steam") => b * conversionFactors.source("steam")
          case Some("hot_water") => b * conversionFactors.source("hot_water")
          case Some("chilled_water") => b * conversionFactors.source("chilled_water")
          case Some("coal") => b * conversionFactors.source("coal")
          case Some(_) => b * conversionFactors.source("other")
          case _ => throw new Exception("Cannot Convert Energy (Double) Value to Source - Type Unkown")
        }
      }
      case (b: Double, "carbon") => {
        energyType match {
          case Some("electricity") => b * conversionFactors.carbon("electricity")
          case Some("natural_gas") => b * conversionFactors.carbon("natural_gas")
          case Some("fuel_oil") => b * conversionFactors.carbon("fuel_oil")
          case Some("propane") => b * conversionFactors.carbon("propane")
          case Some("steam") => b * conversionFactors.carbon("steam")
          case Some("hot_water") => b * conversionFactors.carbon("hot_water")
          case Some("chilled_water") => b * conversionFactors.carbon("chilled_water")
          case Some("coal") => b * conversionFactors.carbon("coal")
          case Some(_) => b * conversionFactors.carbon("other")
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
                                      source: Map[String,Double],
                                      carbon: Map[String,Double]
                                       )
object ValidatedConversionDetails {
  implicit val ValidatedConversionDetailsReads: Reads[ValidatedConversionDetails] = Json.reads[ValidatedConversionDetails]
}
