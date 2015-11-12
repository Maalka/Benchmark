package models

/**
 * Created by rimukas on 10/20/15.
 */


import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, KBtus}
import squants.space.{SquareMeters, SquareFeet, Area}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import EnergyConversions.EnergyNumeric

import scala.util._




case class EUICalculator(parameters: JsValue) {

  implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

  def getSiteEnergy: Future[List[Try[Energy]]] = {
    for {
      entries <- Future{parameters.validate[EnergyList]}
      siteEnergyList <- computeSiteEnergy(entries)
    } yield siteEnergyList
  }
  def getTotalSiteEnergy: Future[Energy] = {
    for {
      entries <- Future{parameters.validate[EnergyList]}
      conversionInfo <- Future {parameters.validate[ConversionInfo].
        getOrElse(throw new Exception("Cannot find Country Type"))}
      siteEnergyList <- computeSiteEnergy(entries)
      siteEnergySum <- getSiteEnergySum(siteEnergyList, conversionInfo)

    } yield siteEnergySum
  }

  def getSiteEnergySum(energies: List[Try[Energy]], convert: ConversionInfo): Future[Energy] = {

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

  def getTotalSourceEnergyNoPoolNoParking:Future[Energy] = {
    for {
      sourceEnergyListConverted <- getTotalSourceEnergy
      poolEnergy <- getPoolEnergy
      parkingEnergy <- getParkingEnergy

    } yield sourceEnergyListConverted - poolEnergy - parkingEnergy
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

  def getParkingEnergy: Future[Energy] = {
    val parkingEnergy = parameters.asOpt[Parking] match {
      case Some(Parking(a, b, c, d, e, units, "USA")) => {

        /*  Console.println("Parking - open: " + a + " Partially Enclosed: "+ b +  " Fully Enclosed: " + c +
            " in Units of: " + units)*/
        val openArea:Double = Area((a.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet
        val partiallyEnclosedParkingArea:Double = Area((b.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet
        val fullyEnclosedParkingArea:Double = Area((c.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet

        KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
          (0.009822 * (d.getOrElse(0.0) * e)))
      }
      case Some(Parking(a, b, c, d, e, units, "Canada")) => {

        /*  Console.println("Parking - open: " + a + " Partially Enclosed: "+ b +  " Fully Enclosed: " + c +
            " in Units of: " + units)*/
        val openArea:Double = Area((a.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet
        val partiallyEnclosedParkingArea:Double = Area((b.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet
        val fullyEnclosedParkingArea:Double = Area((c.getOrElse(0.0),units.getOrElse("ftSQ"))).get to SquareFeet

        KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
          (0.009822 * (d.getOrElse(0.0) * e))) in Gigajoules
      }

      case Some(_) => KBtus(0)
      case None => KBtus(0)
    }
    //Console.println("Parking Energy: " + parkingEnergy)
    Future(parkingEnergy)
  }




def getPoolEnergy:Future[Energy] = {
  //Console.println(parameters.asOpt[Pool])
  val poolEnergy = parameters.asOpt[Pool] match {
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
  //Console.println("Pool Energy: " + poolEnergy)
  Future(poolEnergy)
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
