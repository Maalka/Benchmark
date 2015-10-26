package models

/**
 * Created by rimukas on 10/20/15.
 */


import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, MBtus}
import squants.space.SquareFeet
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import squants.space._

import scala.util._

object EnergyCalcs {


  def getEUI(parameters: JsValue):JsResult[Double] = {

    val r = parameters.validate[EnergyList]
    r.map{ case (a:EnergyList) => getTotalEUI(a) }
  }


  def getTotalEUI(entries:EnergyList): Double =  {


    val floorArea:Try[Area] = Area((entries.GFA, entries.areaUnits))

    val EUI:Double = entries.reportingUnits match {
      case "us" =>  {
        val totalEnergy:Double = entries.energies.map(getSourceEnergy(_,entries.country).get to KBtus).sum
        val buildingArea:Double = floorArea.get to SquareFeet
        totalEnergy / buildingArea
      }
      case "metric" =>  {
        val totalEnergy:Double = entries.energies.map(getSourceEnergy(_,entries.country).get to Gigajoules).sum
        val buildingArea:Double = floorArea.get to SquareMeters
        totalEnergy / buildingArea
      }
    }
    EUI
  }

  /**
   * getResourceEnergy returns the Total Site EUI in kBTU -- at the last step this is converted to whatever reporting
   * units defined by the user
   * @param entry
   * @return
   */
  def getSourceEnergy(entry:EnergyMetrics,country:String):Try[Energy] = {

    val siteEnergy:Try[Energy] = Energy((entry.energyUse, entry.energyUnits))
    Console.println("Site: " + siteEnergy)


    val sourceEnergy:Try[Energy] = siteEnergy match {
      case Success(siteUse) => Success(sourceConvert(entry.energyType, country, siteUse))
      case Failure(error) => Failure(error)
    }
    Console.println("Source: " + (sourceEnergy.get to Gigajoules))
    sourceEnergy

    }

  def sourceConvert(energyType:String,country:String, siteEnergy:Energy):Energy = {

    val convertedEnergy: Energy = (energyType,country) match {
      case ("grid", "USA") => siteEnergy * siteToSourceConversions.gridUS
      case ("grid", "Canada") => siteEnergy * siteToSourceConversions.gridCanada
      case ("naturalGas", "USA") => siteEnergy * siteToSourceConversions.ngUS
      case ("naturalGas", "Canada") => siteEnergy * siteToSourceConversions.ngCanada
      case ("onSiteElectricity", _) => siteEnergy * siteToSourceConversions.onSiteElectricity
      case ("fuelOil", _) => siteEnergy * siteToSourceConversions.fuelOil
      case ("propane", "USA") => siteEnergy * siteToSourceConversions.propaneUS
      case ("propane", "Canada") => siteEnergy * siteToSourceConversions.propaneCanada
      case ("steam", _) => siteEnergy * siteToSourceConversions.steam
      case ("hotWater", _) => siteEnergy * siteToSourceConversions.hotWater
      case ("chilledWater", "USA") => siteEnergy * siteToSourceConversions.chilledWaterUS
      case ("chilledWater", "Canada") => siteEnergy * siteToSourceConversions.chilledWaterCanada
      case ("wood", _) => siteEnergy * siteToSourceConversions.wood
      case ("coke", _) => siteEnergy * siteToSourceConversions.coke
      case ("coal", _) => siteEnergy * siteToSourceConversions.coal
      case ("other", _) => siteEnergy * siteToSourceConversions.other
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

case class EnergyList(energies:List[EnergyMetrics],GFA:Double,reportingUnits:String, country:String, areaUnits:String)
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
