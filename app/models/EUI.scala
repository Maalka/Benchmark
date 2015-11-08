package models

/**
 * Created by rimukas on 10/20/15.
 */


import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, KBtus}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import EnergyConversions.EnergyNumeric

import scala.util._




case class EUICalculator(parameters: JsValue) {

  def getSiteEnergy: Future[List[Try[Energy]]] = {
    for {
      entries <- Future{parameters.validate[EnergyList]}
      siteEnergyList <- computeSiteEnergy(entries)
    } yield siteEnergyList
  }

  def computeSiteEnergy[T](entries: T): Future[List[Try[Energy]]] = {
    val f = entries match {
      case JsSuccess(a:EnergyList, _) => a.energies.map{
        case a:EnergyMetrics => Energy((a.energyUse, a.energyUnits))
      }
      case JsError => throw new Exception("Could not validate energy entries to calculate EUI")
    }
    Future(f)
  }

  def getSourceEnergy:Future[List[Try[Energy]]] = {
    for {
      entries <- Future {
        parameters.validate[EnergyList]
      }
      conversionInfo <- Future {parameters.validate[ConversionInfo].
        getOrElse(throw new Exception("Cannot find Country Type"))}
      sourceEnergyList <- computeSourceEnergy(entries, conversionInfo)
      sourceEnergyListConverted <- convertOutput(sourceEnergyList, conversionInfo)

    } yield {
      sourceEnergyListConverted
    }
  }

  def getTotalSourceEnergy:Future[Energy] = {
    for {
      entries <- Future {
        parameters.validate[EnergyList]
      }
      conversionInfo <- Future {parameters.validate[ConversionInfo].
        getOrElse(throw new Exception("Cannot find Country Type"))}
      sourceEnergyList <- computeSourceEnergy(entries, conversionInfo)
      sourceEnergyListConverted <- convertOutputSum(sourceEnergyList, conversionInfo)

    } yield {
      sourceEnergyListConverted
    }
  }

  def computeSourceEnergy[T](entries: T, convert: ConversionInfo): Future[List[Try[Energy]]] = {
    val f = entries match {
      case JsSuccess(a:EnergyList, _) => a.energies.map{
        case a:EnergyMetrics => {
          sourceConvert(a.energyType, convert.country, Energy((a.energyUse, a.energyUnits)).getOrElse
            (throw new Exception("Error creating energy list for site to source conversion")))
        }
      }
      case JsError => throw new Exception("Could not complete site to source energy conversion")
    }
    Future(f)
  }


  def convertOutput(energies: List[Try[Energy]], convert: ConversionInfo): Future[List[Try[Energy]]] = {

    val f = convert.reportingUnits match {
      case "us" => energies.map{
        case a:Success[Energy] => a.map(_ in KBtus)
        case a:Failure[Energy] => throw new Exception("Could not recognize reporting unit conversion")
      }
      case "metric" => energies.map{
        case a:Success[Energy] => a.map(_ in Gigajoules)
        case a:Failure[Energy] => throw new Exception("Could not recognize reporting unit conversion")
      }
      case _ => throw new Exception("Could not recognize reporting unit conversion")
    }
    Future(f)
  }

  def convertOutputSum(energies: List[Try[Energy]], convert: ConversionInfo): Future[Energy] = {

    val f = convert.reportingUnits match {
      case "us" => energies.map{
        case a:Success[Energy] => a.map(_ in KBtus)
        case a:Failure[Energy] => throw new Exception("Could not recognize reporting unit conversion")
      }.map(_.get).sum in KBtus
      case "metric" => energies.map{
        case a:Success[Energy] => a.map(_ in Gigajoules)
        case a:Failure[Energy] => throw new Exception("Could not recognize reporting unit conversion")
      }.map(_.get).sum in Gigajoules
      case _ => throw new Exception("Could not recognize reporting unit conversion")
    }
    Future(f)
  }


  def sourceConvert(energyType:String,country:String, siteEnergy:Energy):Try[Energy] = {
    (energyType, country) match {
      case ("grid", "USA") => Success(siteEnergy * siteToSourceConversions.gridUS)
      case ("grid", "Canada") => Success(siteEnergy * siteToSourceConversions.gridCanada)
      case ("naturalGas", "USA") => Success(siteEnergy * siteToSourceConversions.ngUS)
      case ("naturalGas", "Canada") => Success(siteEnergy * siteToSourceConversions.ngCanada)
      case ("onSiteElectricity", _) => Success(siteEnergy * siteToSourceConversions.onSiteElectricity)
      case ("fuelOil", _) => Success(siteEnergy * siteToSourceConversions.fuelOil)
      case ("propane", "USA") => Success(siteEnergy * siteToSourceConversions.propaneUS)
      case ("propane", "Canada") => Success(siteEnergy * siteToSourceConversions.propaneCanada)
      case ("steam", _) => Success(siteEnergy * siteToSourceConversions.steam)
      case ("hotWater", _) => Success(siteEnergy * siteToSourceConversions.hotWater)
      case ("chilledWater", "USA") => Success(siteEnergy * siteToSourceConversions.chilledWaterUS)
      case ("chilledWater", "Canada") => Success(siteEnergy * siteToSourceConversions.chilledWaterCanada)
      case ("wood", _) => Success(siteEnergy * siteToSourceConversions.wood)
      case ("coke", _) => Success(siteEnergy * siteToSourceConversions.coke)
      case ("coal", _) => Success(siteEnergy * siteToSourceConversions.coal)
      case ("other", _) => Success(siteEnergy * siteToSourceConversions.other)
      case (_, _) => throw new Exception("Could Not Convert to Source Energy")
    }
  }
}

case class ConversionInfo(country:String, reportingUnits: String)
object ConversionInfo {
  implicit val conversionInfoReads: Reads[ConversionInfo] = Json.reads[ConversionInfo]
}

case class EnergyMetrics(energyType:String,energyUnits:String,energyUse:Double,energyRate:Option[Double])
object EnergyMetrics {
  implicit val energyReads: Reads[EnergyMetrics] = (
    (JsPath \ "energyType").read[String] and
    (JsPath \ "energyUnits").read[String] and
    (JsPath \ "energyUse").read[Double](min(0.0)) and
    (JsPath \ "energyRate").readNullable[Double]
    )(EnergyMetrics.apply _)
}

case class EnergyList(energies:List[EnergyMetrics])
object EnergyList {
  implicit val listReads:Reads[EnergyList] = Json.reads[EnergyList]
}

object siteToSourceConversions {
  val gridUS:Double = 3.14
  val gridCanada:Double = 2.05
  val onSiteElectricity:Double = 1.00
  val ngUS:Double = 1.05
  val ngCanada:Double = 1.02
  val fuelOil:Double = 1.01 // 1,2,3,4,5,6 Diesel, Kerosene
  val propaneUS:Double = 1.01
  val propaneCanada:Double = 1.03
  val steam:Double = 1.2
  val hotWater:Double = 1.2
  val chilledWaterUS:Double = 1.0
  val chilledWaterCanada:Double = 0.71
  val wood:Double = 1.0
  val coal:Double = 1.0
  val coke:Double = 1.0
  val other:Double = 1.0

}
