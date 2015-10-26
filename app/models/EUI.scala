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



object EnergyCalcs {


  def getEUI(parameters: JsValue):JsResult[Double] = {

    val r = parameters.validate[EnergyList]
    r.map{ case (a:EnergyList) => getTotalEUI(a) }
  }


  def getTotalEUI(entries:EnergyList): Double =  {


    val EUI:Double = entries.reportingUnits match {
      case "us" =>  {
        val totalEnergy:Double = entries.energies.map(getSourceEnergy(_,entries.country) to KBtus).sum
        val floorArea:Area = SquareFeet(entries.GFA)
        totalEnergy / floorArea.value
      }
      case "metric" =>  {
        val totalEnergy:Double = entries.energies.map(getSourceEnergy(_,entries.country) to Gigajoules).sum
        val floorArea:Area = SquareMeters(entries.GFA)
        totalEnergy / floorArea.value
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
  def getSourceEnergy(entry:EnergyMetrics,country:String):Energy = {

    val siteEnergy:Energy = getSiteEnergy(entry)
    val sourceEnergy:Energy = (entry.energyType, country) match {
      case ("grid","USA") => siteEnergy*siteToSourceConversions.gridUS
      case ("grid","Canada") => siteEnergy*siteToSourceConversions.gridCanada
      case ("naturalGas","USA") => siteEnergy*siteToSourceConversions.ngUS
      case ("naturalGas","Canada") => siteEnergy*siteToSourceConversions.ngCanada
      case ("onSiteElectricity",_) => siteEnergy*siteToSourceConversions.onSiteElectricity
      case ("fuelOil",_) => siteEnergy*siteToSourceConversions.fuelOil
      case ("propane","USA") => siteEnergy*siteToSourceConversions.propaneUS
      case ("propane","USA") => siteEnergy*siteToSourceConversions.propaneCanada
      case ("steam",_) => siteEnergy*siteToSourceConversions.steam
      case ("hotWater",_) => siteEnergy*siteToSourceConversions.hotWater
      case ("chilledWater","USA") => siteEnergy*siteToSourceConversions.chilledWaterUS
      case ("chilledWater","Canada") => siteEnergy*siteToSourceConversions.chilledWaterCanada
      case ("wood",_) => siteEnergy*siteToSourceConversions.wood
      case ("coke",_) => siteEnergy*siteToSourceConversions.coke
      case ("coal",_) => siteEnergy*siteToSourceConversions.coal
      case ("other",_) => siteEnergy*siteToSourceConversions.other


    }
    Console.println(sourceEnergy to KBtus)
    sourceEnergy
  }


  def getSiteEnergy(entry:EnergyMetrics):Energy = {

    val energyUSe:Energy = entry match {
      case EnergyMetrics("grid",energyUnits,energyUse,energyRate) => gridKwh(energyUnits,energyUse)
      case EnergyMetrics("naturalGas",energyUnits,energyUse,energyRate) => naturalGasKwh(energyUnits,energyUse)

    }
    Console.println(energyUSe to KBtus)
    energyUSe
  }

  def gridKwh(unitType:String,gridUse:Double):Energy = {
    val energyUse:Energy = unitType match {

      case "GJ" => Gigajoules(gridUse)
      case "kBtu" => KBtus(gridUse)
      case "MBtu" => MBtus(gridUse)
      case "kWh" => KilowattHours(gridUse)
      case "MWh" => MegawattHours(gridUse)

    }
    energyUse
  }

  def naturalGasKwh(unitType:String,ngUse:Double):Energy = {
    val energyUse:Energy = unitType match {

      case "therms" => Therms(ngUse)
      case "kBtu" => KBtus(ngUse)
      case "MBtu" => MBtus(ngUse)
      case "cf" => KilowattHours(ngUse)
      case "ccf" => MegawattHours(ngUse)
      case "kcf" => MBtus(ngUse)
      case "m3" => KilowattHours(ngUse)
      case "GJ" => MegawattHours(ngUse)

    }
    energyUse
  }
}

case class EnergyMetrics(energyType:String,energyUnits:String,energyUse:Double,energyRate:Double)
object EnergyMetrics {
  implicit val energyReads: Reads[EnergyMetrics] = (
    (JsPath \ "energyType").read[String] and
    (JsPath \ "energyUnits").read[String] and
    (JsPath \ "energyUse").read[Double] and
    (JsPath \ "energyRate").read[Double]
    )(EnergyMetrics.apply _)
}

case class EnergyList(energies:List[EnergyMetrics],GFA:Double,reportingUnits:String, country:String)
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
