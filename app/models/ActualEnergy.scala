package models

/**
 * Created by rimukas on 10/20/15.
 */

import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, KBtus}
import squants.space.{Area, SquareFeet, SquareMeters}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import EnergyConversions.EnergyNumeric
import squants.energy
import squants.motion.CubicFeetPerHour

import scala.util._
import scala.util.control.NonFatal

case class EUICalculator(parameters: JsValue) {

  implicit def boolOptToInt(b: Option[Boolean]): Int = if (b.getOrElse(false)) 1 else 0

  def getTotalSiteEnergy: Future[Energy] = {
    for {
      validatedList <- getValidateSiteEnergyList
      sum <- getSiteEnergySum(validatedList)
    } yield sum
  }



  def getSiteEnergySum(energyList: List[ValidatedEnergy]):Future[Energy] = Future {
    energyList.map(_.energyValue).sum in KBtus
  }

  //always read energy input values as KBtus
  def getValidateSiteEnergyList: Future[List[ValidatedEnergy]] = {
    for {
      energyList <- getSiteEnergyList
      validatedEnergyList <- Future {
        energyList.energies.map {
          case a: EnergyMetrics => ValidatedEnergy(a.energyType, a.energyName, mapEnergy(a.energyUnits, a.energyUse) in KBtus)
        }
      }
    } yield validatedEnergyList
  }

  def getSiteEnergyList: Future[EnergyList] = Future {
    parameters.asOpt[EnergyList] match {
      case Some(a) => a
      case _ => throw new Exception("Unidentified energy entry in array!")
    }
  }
  
  def mapEnergy(units:String,value:Double):Energy = {
    units match {
      case "KBtu" => KBtus(value)
      case "MBtu" => MBtus(value)
      case "kWh" => KilowattHours(value)
      case "MWh" => MegawattHours(value)
      case "GJ" => Gigajoules(value)
      case "NGMcf" => NGMCfs(value)
      case "NGKcf" => NGKCfs(value)
      case "NGCcf" => NGCCfs(value)
      case "NGcf" => NGCfs(value)
      case "NGm3" => NGm3s(value)
      case "Therms" => Therms(value)
      case "No1UKG" => OilNo1UKGs(value)
      case "No1USG" => OilNo1USGs(value)
      case "No1L" => OilNo1Ls(value)
      case "No2UKG" => OilNo2UKGs(value)
      case "No2USG" => OilNo2USGs(value)
      case "No2L" => OilNo2Ls(value)
      case "No4UKG" => OilNo4UKGs(value)
      case "No4USG" => OilNo4USGs(value)
      case "No4L" => OilNo4Ls(value)
      case "No6UKG" => OilNo6UKGs(value)
      case "No6USG" => OilNo6USGs(value)
      case "No6L" => OilNo6Ls(value)
      case "PropaneUKG" => PropaneUKGs(value)
      case "PropaneUSG" => PropaneUSGs(value)
      case "PropaneCf" => PropaneCfs(value)
      case "PropaneCCf" => PropaneCCfs(value)
      case "PropaneKCf" => PropaneKCfs(value)
      case "PropaneL" => PropaneLs(value)
      case "SteamLb" => SteamLbs(value)
      case "SteamKLb" => SteamKLbs(value)
      case "SteamMLb" => SteamMLbs(value)
      case "CHWTonH" => CHWTonHs(value)
      case "CoalATon" => CoalATons(value)
      case "CoalATonne" => CoalATonnes(value)
      case "CoalALb" => CoalALbs(value)
      case "CoalBitTon" => CoalBitTons(value)
      case "CoalBitTonne" => CoalBitTonnes(value)
      case "CoalBitLb" => CoalBitLbs(value)
      case "CokeTon" => CokeTons(value)
      case "CokeTonne" => CokeTonnes(value)
      case "CokeLb" => CokeLbs(value)
      case "WoodTon" => WoodTons(value)
      case "WoodTonne" => WoodTonnes(value)
    }
  }

}


case class EnergyMetrics(energyType:String,energyName:String,energyUnits:String,energyUse:Double)
object EnergyMetrics {
  implicit val energyReads: Reads[EnergyMetrics] = (
    (JsPath \ "energyType").read[String] and
    (JsPath \ "energyName").read[String] and
    (JsPath \ "energyUnits").read[String] and
    (JsPath \ "energyUse").read[Double](min(0.0))
    )(EnergyMetrics.apply _)
}

case class EnergyList(energies:List[EnergyMetrics])
object EnergyList {
  implicit val listReads:Reads[EnergyList] = Json.reads[EnergyList]
}

case class RenewableEnergyList(renewableEnergies:List[EnergyMetrics])
object RenewableEnergyList {
  implicit val listReads:Reads[RenewableEnergyList] = Json.reads[RenewableEnergyList]
}




case class ValidatedEnergy(energyType: String, energyName: String, energyValue: Energy)

