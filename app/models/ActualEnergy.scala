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

import scala.util._
import scala.util.control.NonFatal

case class EUICalculator(parameters: JsValue) {

  implicit def boolOptToInt(b: Option[Boolean]): Int = if (b.getOrElse(false)) 1 else 0

  def country:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.country match {
          case Some(country:String) => country
          case _ => throw new Exception("Could not retrieve Country")
        }
      }

      case _ => throw new Exception("Could not retrieve Country")
    }
  }

  def reportingUnits:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.reportingUnits match {
          case Some(reportingUnits:String) => reportingUnits
          case None => "us"
        }
      }

      case _ => throw new Exception("Could not retrieve Reporting Units")
    }
  }

  def isNetMetered:Future[Boolean] = Future{
    parameters.asOpt[NetMetered] match {
      case Some(a) => a.netMetered
      case _ => false //default to false (subtract renewables from total)
      //throw new Exception("Could not tell if Net Metered")
    }
  }

  def sourceEnergynoPoolnoParking:Future[Energy] = {
    for {
      poolEnergy <- getPoolEnergy
      parkingEnergy <- getParkingEnergy
      sourceTotalEnergy <- getTotalSourceEnergy
      sourceEnergy <- Future(sourceTotalEnergy - poolEnergy - parkingEnergy)
    } yield sourceEnergy
  }

  def getSiteEnergyList: Future[List[EnergyTuple]] = {
    for {
      entries <- getEnergyList
      siteEnergyList <- computeSiteEnergy(entries)
    } yield siteEnergyList
  }


  def getSiteRenewableEnergyList: Future[List[EnergyTuple]] = {
    for {
      entries <- getRenewableEnergyList
      siteRenewableEnergyList <- computeSiteEnergy(entries)
    } yield siteRenewableEnergyList
  }

  def getRenewableEnergyTotalbyType(entryList:List[EnergyTuple],entryType:String): Future[Energy] = Future {
    entryType match {
      case "onSite" => entryList.filterNot(_.energyName == "Off-Site Purchased").map(_.energyValue).sum
      case "purchased" => entryList.filter(_.energyName == "Off-Site Purchased").map(_.energyValue).sum
      case _ => entryList.map(_.energyValue).sum
    }
  }

  //this function will output in double in expected units, while the function above will output in watt-hours
  //in order to be used in downstreams arithmetic operations
  def getRenewableEnergyTotalbyTypeOutput(entryList:List[EnergyTuple],entryType:String): Future[Double] = Future {
    entryType match {
      case "onSite" => entryList.filterNot(_.energyName == "Off-Site Purchased").map(_.energyValue.value).sum
      case "purchased" => entryList.filter(_.energyName == "Off-Site Purchased").map(_.energyValue.value).sum
      case _ => 0.0
    }
  }

  def getSiteEnergyTotalbyType(entryList:List[EnergyTuple]): Future[Double] = Future {
    entryList.map(_.energyValue.value).sum
  }

  def getTotalSiteEnergy: Future[Energy] = {
    for {
      siteEnergyList <- getSiteEnergyList
      siteEnergySum <- getSiteEnergySum(siteEnergyList)
      siteRenewableSum <- getTotalSiteRenewableEnergy
    } yield siteEnergySum - siteRenewableSum
  }


  def getTotalSiteRenewableEnergy: Future[Energy] = {
    val local = for {
      netMetered <- isNetMetered
      entries <- getRenewableEnergyList
      siteRenewableEnergyList <- computeSiteEnergy(entries)
      siteRenewableEnergySum <- getSiteEnergySum(siteRenewableEnergyList)
    } yield {
        netMetered match {
          case true => countryZero
          case false => siteRenewableEnergySum
        }
      }
    local.recoverWith{
      case NonFatal(th) =>  Future{countryZero}
    }
  }

  def getSiteEnergySum(energies: List[EnergyTuple]): Future[Energy] = {
    val f = country match {
      case "USA" => energies.map { case a: EnergyTuple => a.energyValue in KBtus }.sum in KBtus
      case _ => energies.map { case a: EnergyTuple => a.energyValue in Gigajoules }.sum in Gigajoules
    }
    Future(f)
  }

  def computeSiteEnergy[T](entries: T): Future[List[EnergyTuple]] = Future {
    entries match {
      case a: EnergyList => a.energies.map {
        case a: EnergyMetrics => {
          Energy((a.energyUse, a.energyUnits)) match {
            case b:Success[Energy] => EnergyTuple(a.energyType,a.energyName,b.get)
            case b: Failure[Energy] => throw new Exception("Could not determine energy unit for entry")
          }
        }
      }
      case a: RenewableEnergyList => a.renewableEnergies.map {
        case a: EnergyMetrics => {
          Energy((a.energyUse, a.energyUnits)) match {
            case b:Success[Energy] => EnergyTuple(a.energyType,a.energyName,b.get)
            case b: Failure[Energy] => throw new Exception("Could not determine energy unit for entry")
          }
        }
      }
      case _ => throw new Exception("Could not validate energy entries to calculate EUI")
    }
  }

  def getSourceEnergy: Future[List[EnergyTuple]] = {
    for {
      entries <- getEnergyList
      sourceEnergyList <- computeSourceEnergy(entries)
      sourceEnergyListConverted <- convertOutput(sourceEnergyList)
    } yield {
      sourceEnergyListConverted
    }
  }

  def getTotalSourceEnergy: Future[Energy] = {
    for {
      netMetered <- isNetMetered
      entries <- getEnergyList
      sourceEnergyList <- computeSourceEnergy(entries)
      sourceEnergyListConverted <- convertOutputSum(sourceEnergyList)
      totalSourceRenewableEnergy <- getTotalSourceRenewableEnergy

    } yield sourceEnergyListConverted - totalSourceRenewableEnergy
  }

  def getTotalSourceRenewableEnergy: Future[Energy] = {
    val local = for {
      netMetered <- isNetMetered
      entries <- getRenewableEnergyList
      sourceRenewableEnergyList <- computeSourceEnergy(entries)
      sourceRenewableEnergySum <- convertOutputSum(sourceRenewableEnergyList)
    } yield {
        netMetered match {
          case true => countryZero
          case false => sourceRenewableEnergySum
        }
      }
    local.recoverWith{
      case NonFatal(th) =>  Future{countryZero}
    }
  }


  def getTotalSourceEnergyNoPoolNoParking: Future[Energy] = {
    for {
      sourceEnergyListConverted <- getTotalSourceEnergy
      poolEnergy <- getPoolEnergy
      parkingEnergy <- getParkingEnergy

    } yield sourceEnergyListConverted - poolEnergy - parkingEnergy
  }

  def computeSourceEnergy[T](entries: T): Future[List[EnergyTuple]] = Future{
    entries match {
      case a: EnergyList => a.energies.map {
        case a: EnergyMetrics => sourceConvert(a.energyType,a.energyName, Energy((a.energyUse, a.energyUnits)))
      }
      case a: RenewableEnergyList => a.renewableEnergies.map {
        case a: EnergyMetrics => sourceConvert(a.energyType,a.energyName, Energy((a.energyUse, a.energyUnits)))
      }
      case _ => throw new Exception("Could not compute Source Energy from Site Energy")
    }
  }

  def getEnergyList: Future[EnergyList] = {
    for {
      entries <- Future {
        parameters.validate[EnergyList]
      }
      siteEnergyList <- {
        entries match {
          case JsSuccess(a, _) => Future(a)
          case JsError(err) => throw new Exception("Could not read provided energy list")
        }
      }
    } yield siteEnergyList
  }

  def getRenewableEnergyList: Future[RenewableEnergyList] = {
    for {
      entries <- Future {
        parameters.validate[RenewableEnergyList]
      }
      siteRenewableEnergyList <- {
        entries match {
          case JsSuccess(a, _) => Future(a)
          case JsError(err) => throw new Exception("Could not read provided energy list")
        }
      }
    } yield siteRenewableEnergyList
  }

  def convertOutput(energies: List[EnergyTuple]): Future[List[EnergyTuple]] = Future {
    country match {
      case "USA" => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName,a.energyValue in KBtus)}
      case _ => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName, a.energyValue in Gigajoules)}
    }
  }

  def convertOutputSum(energies: List[EnergyTuple]): Future[Energy] = Future {
    country match {
      case "USA" => energies.map {case a:EnergyTuple => a.energyValue in KBtus}.sum in KBtus
      case _ => energies.map {case a:EnergyTuple => a.energyValue in Gigajoules}.sum in Gigajoules
    }
  }

  def sourceConvert(energyType: String, energyName:String, siteEnergy: Try[Energy]): EnergyTuple = {
    val sourceEnergyValue = siteEnergy match {
        case a: Success[Energy] => {
        (energyType, country) match {
        case ("grid", "USA") => a.get * siteToSourceConversions.gridUS
        case ("grid", "Canada") => a.get * siteToSourceConversions.gridCanada
        case ("naturalGas", "USA") => a.get * siteToSourceConversions.ngUS
        case ("naturalGas", "Canada") => a.get * siteToSourceConversions.ngCanada
        case ("onSiteElectricity", _) => a.get * siteToSourceConversions.onSiteElectricity
        case ("fuelOil1", _) => a.get * siteToSourceConversions.fuelOil
        case ("fuelOil2", _) => a.get * siteToSourceConversions.fuelOil
        case ("fuelOil4", _) => a.get * siteToSourceConversions.fuelOil
        case ("fuelOil6", _) => a.get * siteToSourceConversions.fuelOil
        case ("kerosene", _) => a.get * siteToSourceConversions.fuelOil
        case ("diesel", _) => a.get * siteToSourceConversions.fuelOil
        case ("propane", "USA") => a.get * siteToSourceConversions.propaneUS
        case ("propane", "Canada") => a.get * siteToSourceConversions.propaneCanada
        case ("steam", _) => a.get * siteToSourceConversions.steam
        case ("hotWater", _) => a.get * siteToSourceConversions.hotWater
        case ("chilledWater", "USA") => a.get * siteToSourceConversions.chilledWaterUS
        case ("chilledWater", "Canada") => a.get * siteToSourceConversions.chilledWaterCanada
        case ("wood", _) => a.get * siteToSourceConversions.wood
        case ("coke", _) => a.get * siteToSourceConversions.coke
        case ("coalA", _) => a.get * siteToSourceConversions.coal
        case ("coalB", _) => a.get * siteToSourceConversions.coal
        case ("other", _) => a.get * siteToSourceConversions.other
        case ("onSiteSolar", _) => a.get * siteToSourceConversions.gridUS
        case ("onSiteWind", _) => a.get * siteToSourceConversions.gridUS
        case ("onSiteOther", _) => a.get * siteToSourceConversions.gridUS
        case ("offSite", _) => a.get * siteToSourceConversions.gridUS
        case (_, _) => throw new Exception("Could Not Convert to Source Energy")
      }}
      case a: Failure[Energy] => throw new Exception("Could not read site energy entry")
    }
    return EnergyTuple(energyType,energyName,sourceEnergyValue)
  }

  def getParkingEnergy: Future[Energy] = Future {
    parameters.asOpt[Parking] match {
      case Some(Parking(a, b, c, d, e, units, "USA")) => {

        val openArea: Double = Area((a.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet
        val partiallyEnclosedParkingArea: Double = Area((b.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet
        val fullyEnclosedParkingArea: Double = Area((c.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet

        KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
          (0.009822 * (d.getOrElse(0.0) * e)))
      }
      case Some(Parking(a, b, c, d, e, units, "Canada")) => {

        val openArea: Double = Area((a.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet
        val partiallyEnclosedParkingArea: Double = Area((b.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet
        val fullyEnclosedParkingArea: Double = Area((c.getOrElse(0.0), units.getOrElse("ftSQ"))).get to SquareFeet

        KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
          (0.009822 * (d.getOrElse(0.0) * e))) in Gigajoules
      }
      case Some(_) => KBtus(0)
      case None => KBtus(0)
    }
  }

  def countryZero:Energy = {
    country match {
      case "USA" => KBtus(0)
      case _ => Gigajoules(0)
    }
  }

  def getPoolEnergy: Future[Energy] = Future {

    parameters.asOpt[Pool] match {
      case Some(Pool(Some("Indoor"), "USA", "K12School", Some("Recreational"))) => KBtus(1257300)
      case Some(Pool(Some("Indoor"), "USA", "K12School", Some("ShortCourse"))) => KBtus(2095500)
      case Some(Pool(Some("Indoor"), "USA", "K12School", Some("Olympic"))) => KBtus(6266009)
      case Some(Pool(Some("Indoor"), "USA", "Hotel", Some("Recreational"))) => KBtus(1010711)
      case Some(Pool(Some("Indoor"), "USA", "Hotel", Some("ShortCourse"))) => KBtus(1684518)
      case Some(Pool(Some("Indoor"), "USA", "Hotel", Some("Olympic"))) => KBtus(5037084)
      case Some(Pool(Some("Indoor"), "USA", _, Some("Recreational"))) => KBtus(853981)
      case Some(Pool(Some("Indoor"), "USA", _, Some("ShortCourse"))) => KBtus(1423301)
      case Some(Pool(Some("Indoor"), "USA", _, Some("Olympic"))) => KBtus(4255987)
      case Some(Pool(Some("Indoor"), "Canada", "K12School", Some("Recreational"))) => KBtus(1202780) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", "K12School", Some("ShortCourse"))) => KBtus(2004633) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", "K12School", Some("Olympic"))) => KBtus(5993047) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", "Hotel", Some("Recreational"))) => KBtus(962982) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", "Hotel", Some("ShortCourse"))) => KBtus(1604654) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", "Hotel", Some("Olympic"))) => KBtus(4799745) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", _, Some("Recreational"))) => KBtus(810384) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", _, Some("ShortCourse"))) => KBtus(1351587) in Gigajoules
      case Some(Pool(Some("Indoor"), "Canada", _, Some("Olympic"))) => KBtus(4040544) in Gigajoules


      case Some(Pool(Some("Outdoor"), "USA", _, Some("Recreational"))) => KBtus(119914)
      case Some(Pool(Some("Outdoor"), "USA", _, Some("ShortCourse"))) => KBtus(199857)
      case Some(Pool(Some("Outdoor"), "USA", _, Some("Olympic"))) => KBtus(597621)
      case Some(Pool(Some("Outdoor"), "Canada", _, Some("Recreational"))) => KBtus(112790) in Gigajoules
      case Some(Pool(Some("Outdoor"), "Canada", _, Some("ShortCourse"))) => KBtus(187668) in Gigajoules
      case Some(Pool(Some("Outdoor"), "Canada", _, Some("Olympic"))) => KBtus(561108) in Gigajoules

      case Some(Pool(_, _, _, _)) => KBtus(0)
      case None => KBtus(0)
    }
  }
}

case class  ConversionInfo(country:Option[String], reportingUnits: Option[String], buildingType:Option[String], postalCode:Option[String], state:Option[String], buildingName:Option[String])
object ConversionInfo {
  implicit val conversionInfoReads: Reads[ConversionInfo] = Json.reads[ConversionInfo]
}

case class EnergyMetrics(energyType:String,energyName:String,energyUnits:String,energyUse:Double,energyRate:Option[Double])
object EnergyMetrics {
  implicit val energyReads: Reads[EnergyMetrics] = (
    (JsPath \ "energyType").read[String] and
    (JsPath \ "energyName").read[String] and
    (JsPath \ "energyUnits").read[String] and
    (JsPath \ "energyUse").read[Double](min(0.0)) and
    (JsPath \ "energyRate").readNullable[Double]
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

case class Pool(indoorOutdoor:Option[String],country:String, buildingType:String, poolType:Option[String])
object Pool {
  implicit val poolRead: Reads[Pool] = Json.reads[Pool]
}

case class Parking(openParkingArea:Option[Double],partiallyEnclosedParkingArea:Option[Double],
                   fullyEnclosedParkingArea:Option[Double], HDD:Option[Double], hasParkingHeating:Option[Boolean],
                   parkingAreaUnits:Option[String],country:String)
object Parking {
  implicit val parkingRead: Reads[Parking] = Json.reads[Parking]
}

case class EnergyTuple(energyType: String, energyName: String, energyValue: Energy)
case class NetMetered(netMetered: Boolean)
object NetMetered {
  implicit val netMeteredRead: Reads[NetMetered] = Json.reads[NetMetered]
}
