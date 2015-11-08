package models

/**
 * Created by rimukas on 10/20/15.
 */


import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, MBtus}
import squants.space.SquareFeet
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import squants.space._

import scala.util._




object EnergyCalcs {


  def getE(parameters: JsValue):Future[Double] = {
    val f = parameters.validate[EnergyList] match {
      case JsSuccess(a:EnergyList,_) => getTotalSourceEnergy(a)
      case JsError(err) => throw new Exception("EUI (for lookup) could not be predicted")
    }
    Future(f)
   // r.map{ case (a:EnergyList) => getTotalSourceEnergy(a) }
  }


  def getSiteEnergy(energyUse:Double,energyUnits:String):Try[Energy] = Energy((energyUse, energyUnits))

  def getTotalSourceEnergy(entries:EnergyList): Double =  {

    val totalE:Double = entries.reportingUnits match {
      case "us" =>  {
        val totalEnergy:Double = entries.energies.map{
        case a: EnergyMetrics => (getSourceEnergy(getSiteEnergy(a.energyUse,a.energyUnits),a.energyType,entries.country).get
          to KBtus)}.sum
        totalEnergy
      }
      case "metric" =>  {
        val totalEnergy:Double = entries.energies.map{
          case a: EnergyMetrics => (getSourceEnergy(getSiteEnergy(a.energyUse,a.energyUnits),a.energyType,entries.country).get
            to Gigajoules)}.sum
        totalEnergy
      }
    }
    totalE
  }

  /**
   *
   * @param siteEnergy
   * @param energyType
   * @param country
   * @return
   */
  def getSourceEnergy(siteEnergy:Try[Energy],energyType:String,country:String):Try[Energy] = {

    val sourceEnergy:Try[Energy] = siteEnergy match {
      case Success(siteUse) => {
        Console.println("Actual " + energyType + " Site Energy: " + siteEnergy.get +" and in kBTU: " + (siteEnergy.get to KBtus))
        sourceConvert(energyType, country, siteUse)
      }
      case Failure(error) => {
        Console.println("Actual " + energyType + " Site Energy not Found")
        Failure(error)
      }
    }

    Console.println("Actual " + energyType + " Source Energy (kWh): " + sourceEnergy.get + " and in kBTU: " +
      (sourceEnergy.get to KBtus))
    sourceEnergy

    }

  def sourceConvert(energyType:String,country:String, siteEnergy:Energy):Try[Energy] = {

    val convertedEnergy: Try[Energy] = (energyType,country) match {
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
      case (_,_) => throw new Exception("Could Not Convert to Source Energy")
    }
    convertedEnergy
  }

}

case class EnergyMetrics(energyType:String,energyUnits:String,energyUse:Double,energyRate:Double)
object EnergyMetrics {
  implicit val energyReads: Reads[EnergyMetrics] = (
    (JsPath \ "energyType").read[String] and
    (JsPath \ "energyUnits").read[String] and
    (JsPath \ "energyUse").read[Double](min(0.0)) and
    (JsPath \ "energyRate").read[Double]
    )(EnergyMetrics.apply _)
}

case class EnergyList(energies:List[EnergyMetrics],reportingUnits:String, country:String, areaUnits:String)
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
