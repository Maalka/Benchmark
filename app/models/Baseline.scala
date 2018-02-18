
package models


import squants.energy._
import squants.space._

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.InputStream

import play.api.libs.ws.WSResponse

import scala.util.control.NonFatal


case class EUIMetrics(parameters: JsValue, nrel_client: NREL_Client) {


  val result = parameters.as[List[JsValue]]

  val pvSystems: SolarProperties = SolarProperties(result.head)
  val metricConversion: MetricConversion = MetricConversion(result.head)
  val submittedEnergy: EUICalculator = EUICalculator(result.head)
  val prescriptiveEUI = PrescriptiveValues(result.head)


  def getPV = pvSystems.setPVDefaults
  def pVWattsResponse: Future[JsValue] = nrel_client.makeWsRequest(Seq.empty[(String, String)])

  def getBuildingEnergyList: Future[EnergyList] = submittedEnergy.getSiteEnergyList
  def getBuildingData: Future[List[ValidatedPropTypes]] = prescriptiveEUI.getValidatedPropList
  def getMetrics: Future[ValidatedConversionDetails] = metricConversion.getConversionMetrics(None)

  def getSiteMetrics:Future[Seq[Map[String,Any]]] = {
    //Site EUI and Energy have to be converted to reportingUnits for output as last step, carbon and source are already converted
      for {
      buildingSize <- prescriptiveEUI.getBuildingSize
      convertedBuildingSize <- convertSize(buildingSize)

      totalSite <- submittedEnergy.getTotalSiteEnergy
      totalCarbon <- getTotalActualCarbon
      totalSource <- getTotalActualSource

      convertedTotalSite <- convertEnergy(totalSite)
      convertedTotalSource <- convertEnergy(totalSource)

      energyList <- submittedEnergy.getSiteEnergyList


      } yield {
        Seq(

          Map("site_energy"->convertedTotalSite),
          Map("source_energy"->convertedTotalSource),
          Map("carbon_tonnes"->totalCarbon),

          Map("site_eui"->convertedTotalSite / convertedBuildingSize),
          Map("source_eui"->totalSource / convertedBuildingSize),
          Map("carbon_intensity"->totalCarbon / convertedBuildingSize),

          Map("building_energy_list"-> energyList.energies)
        )
      }
  }

  def getPrescriptiveMetrics:Future[Seq[Map[String,Any]]] = {

      for {
      buildingSize <- prescriptiveEUI.getBuildingSize
      convertedBuildingSize <- convertSize(buildingSize)

      totalCarbon <- getPrescriptiveTotalCarbon
      totalSource <- getPrescriptiveTotalSource
      totalSite <- getPrescriptiveTotalSite

      convertedTotalSource <- convertEnergy(totalSource)
      convertedTotalSite <- convertEnergy(totalSite)


      prescriptiveEndUses <- getPrescriptiveEndUses
      prescriptiveElectricity <- getPrescriptiveElectricity
      prescriptiveNG <- getPrescriptiveNG
      prescriptiveEndUsePercents <- getPrescriptiveEndUsePercents

      } yield {
        Seq(
          Map("site_energy"->convertedTotalSite),
          Map("source_energy"->convertedTotalSource),
          Map("carbon_tonnes"->totalCarbon),

          Map("site_eui"->convertedTotalSite / convertedBuildingSize),
          Map("source_eui"->convertedTotalSource / convertedBuildingSize),
          Map("carbon_intensity"->totalCarbon / convertedBuildingSize),

          Map("prescriptive_end_use_metric_data"->prescriptiveEndUses),
          Map("prescriptive_electricity_metric_data"->prescriptiveElectricity),
          Map("prescriptive_natural_gas_metric_data"->prescriptiveNG),
          Map("prescriptive_end_use_metric_percents"->prescriptiveEndUsePercents)
        )
      }
  }

  def getTotalSiteEnergy: Future[Energy] = {
    for {
      totalSite <- submittedEnergy.getTotalSiteEnergy
      convertedTotalSite <- convertEnergy(totalSite)
    } yield convertedTotalSite
  }


  def getTotalActualCarbon: Future[Double] = getTotalActualMetric(Some("carbon"))
  def getTotalActualSource: Future[Energy] = {
    for {
      totalSource <- getTotalActualMetric(Some("source"))
      converted <- convertEnergy(KBtus(totalSource))
    } yield converted
  }





  def getPrescriptiveEndUsePercents: Future[EndUseDistribution] = {
    for {
      prescriptiveEndUSePercents <- prescriptiveEUI.lookupPrescriptiveEndUsePercents(None)
    } yield prescriptiveEndUSePercents
  }

  def getPrescriptiveEndUses: Future[EndUseDistribution] = {
    for {
      prescriptiveEndUses <- prescriptiveEUI.lookupPrescriptiveEndUses(None)
      converted <- convertPrescriptive(prescriptiveEndUses)
    } yield converted
  }

  def getPrescriptiveElectricity: Future[ElectricityDistribution] = {
    for {
      prescriptiveElectricityWeighted <- prescriptiveEUI.lookupPrescriptiveElectricityWeighted(None)
      converted <- convertPrescriptive(prescriptiveElectricityWeighted)
    } yield converted
  }

  def getPrescriptiveNG: Future[NaturalGasDistribution] = {
    for {
      prescriptiveNGWeighted <- prescriptiveEUI.lookupPrescriptiveNGWeighted(None)
      converted <- convertPrescriptive(prescriptiveNGWeighted)
    } yield converted
  }

  def getPrescriptiveTotalCarbonIntensity: Future[Energy] = {
    for {
      prescriptiveTotalEUI <- prescriptiveEUI.lookupPrescriptiveTotalMetricIntensity(Some("carbon"))
    } yield prescriptiveTotalEUI
  }

  def getPrescriptiveTotalCarbon: Future[Energy] = {
    for {
      prescriptiveTotalEnergy <- getPrescriptiveTotalCarbonIntensity
      building_size <- prescriptiveEUI.getBuildingSize
    } yield prescriptiveTotalEnergy * building_size
  }

  def getPrescriptiveTotalSourceIntensity: Future[Energy] = {
    for {
      prescriptiveTotalEUI <- prescriptiveEUI.lookupPrescriptiveTotalMetricIntensity(Some("source"))
    } yield prescriptiveTotalEUI
  }

  def getPrescriptiveTotalSource: Future[Energy] = {
    for {
      prescriptiveTotalEnergy <- getPrescriptiveTotalSourceIntensity
      building_size <- prescriptiveEUI.getBuildingSize
    } yield prescriptiveTotalEnergy*building_size
  }

  def getPrescriptiveTotalSiteIntensity: Future[Energy] = {
    for {
      prescriptiveTotalEUI <- prescriptiveEUI.lookupPrescriptiveTotalMetricIntensity(Some("site"))
    } yield prescriptiveTotalEUI
  }

  def getPrescriptiveTotalSite: Future[Energy] = {
    for {
      prescriptiveTotalEnergy <- getPrescriptiveTotalSiteIntensity
      building_size <- prescriptiveEUI.getBuildingSize
    } yield prescriptiveTotalEnergy*building_size
  }


  def getTotalActualMetric(metric_type: Option[String]): Future[Double] = {
    //this returns in the output reporting metric
    for {
      energyList <- submittedEnergy.getValidateSiteEnergyList
      conversionMetrics <- metricConversion.getConversionMetrics(metric_type)
      metricList:List[ValidatedEnergy] <- Future.sequence(energyList.map {
        case a: ValidatedEnergy => metricConversion.convertMetrics(a, Some(a.energyType), conversionMetrics, metric_type)
      })
      sum <- Future(metricList.map(_.energyValue.value).sum )
    } yield sum
  }


  //default reporting units are IMPERIAL (kbtu, square feet, ...)
  def reportingUnits: String = {
    result.head.asOpt[ReportingUnits] match {
      case Some(a) => a.reporting_units
      case _ => "imperial"
    }
  }


case class ReportingUnits(reporting_units:String)
  object ReportingUnits {
    implicit val ReportingUnitsReads: Reads[ReportingUnits] = Json.reads[ReportingUnits]
  }


  def convertPrescriptive[T](distribution: T):Future[T]  = Future {
    reportingUnits match {
      case ("metric") => {
        val c = energyMetricConstant / areaMetricConstant
        distribution match {
          case b:ElectricityDistribution => {
            ElectricityDistribution(
              b.elec_htg * c,
              b.elec_clg * c,
              b.elec_intLgt * c,
              b.elec_extLgt * c,
              b.elec_intEqp * c,
              b.elec_extEqp * c,
              b.elec_fans * c,
              b.elec_pumps * c,
              b.elec_heatRej * c,
              b.elec_humid * c,
              b.elec_heatRec * c,
              b.elec_swh * c,
              b.elec_refrg * c,
              b.elec_gentor * c,
              b.elec_net * c
            )
          }
          case b:NaturalGasDistribution => {
            NaturalGasDistribution(
              b.ng_htg * c,
              b.ng_clg * c,
              b.ng_intLgt * c,
              b.ng_extLgt * c,
              b.ng_intEqp * c,
              b.ng_extEqp * c,
              b.ng_fans * c,
              b.ng_pumps * c,
              b.ng_heatRej * c,
              b.ng_humid * c,
              b.ng_heatRec * c,
              b.ng_swh * c,
              b.ng_refrg * c,
              b.ng_gentor * c,
              b.ng_net * c
            )
          }
          case b:EndUseDistribution => {
            EndUseDistribution(
              b.htg * c,
              b.clg * c,
              b.intLgt * c,
              b.extLgt * c,
              b.intEqp * c,
              b.extEqp * c,
              b.fans * c,
              b.pumps * c,
              b.heatRej * c,
              b.humid * c,
              b.heatRec * c,
              b.swh * c,
              b.refrg * c,
              b.gentor * c,
              b.net * c
            )
          }
        }
      }.asInstanceOf[T]
      case _ => distribution.asInstanceOf[T]
    }
  }


// final output conversions

  def energyMetricUnit(energyEntry:Energy):Energy = energyEntry in KilowattHours
  def energyMetricConstant:Double = KBtus(1) to KilowattHours //interpret as kwh per kbtu

  def areaMetricUnit(areaEntry:Double):Double = SquareFeet(areaEntry) to SquareMeters
  def areaMetricConstant:Double = SquareFeet(1) to SquareMeters//interpret as sq meters per sq ft

  def solarConversionEnergy: Future[Double] = Future{
    result.head.asOpt[ReportingUnits] match {
      case Some(a) => a.reporting_units match {
        case "metric" => 1.0
        case _ => 1.0 / energyMetricConstant
      }
      case _ => 1.0 / energyMetricConstant
    }
  }
  def solarConversionIntensity: Future[Double] = Future{
    result.head.asOpt[ReportingUnits] match {
      case Some(a) => a.reporting_units match {
        case "metric" => 1.0
        case _ => 1.0 / energyMetricConstant * areaMetricConstant
      }
      case _ => 1.0 / energyMetricConstant * areaMetricConstant
    }
  }

  def convertEnergy(energyEntry:Energy):Future[Energy] = Future{
    //input should always be KBtus
    reportingUnits match {
      case "imperial" => energyEntry
      case "metric" => energyMetricUnit(energyEntry)
      case _ =>  throw new Exception ("Reporting Units not Identified!")
    }
  }

  def convertEUI(energyEntry:Energy,areaEntry:Double):Future[Energy] = Future{
    reportingUnits match {
      case "imperial" => energyEntry / areaEntry
      case "metric" => energyMetricUnit(energyEntry) / areaMetricUnit(areaEntry)
    }
  }

  def convertSize(areaEntry:Double):Future[Double] = Future{
    reportingUnits match {
      case "imperial" => areaEntry
      case "metric" => areaMetricUnit(areaEntry)
    }
  }


}

// These classes represent data that have been populated with defaults
case class BuildingData(
                           building_type: Option[String],
                           solar_file_id: Option[String],
                           climate_zone: Option[String],
                           floor_area: Option[Double],
                           floor_area_units: Option[String],
                           stories: Option[Double])

object BuildingData {
  implicit val BuildingDataReads: Reads[BuildingData] = Json.reads[BuildingData]
}

case class ValidatedBuildingData(
                           building_type: String,
                           solar_file_id: String,
                           climate_zone: String,
                           floor_area: Double,
                           floor_area_units: String,
                           stories: Double)






