package models

import squants.energy.{MBtus, Energy}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{InputStream}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


case class Emissions(parameters:JsValue) {

  val energyCalcs:EUICalculator = EUICalculator(parameters)
  val buildingProps:BuildingProperties = BuildingProperties(parameters)

  def getTotalEmissions(energyList:EnergyList): Future[Double] = {
    for {
      direct <- getDirectEmissionList(energyList)
      indirect <- getIndirectEmissionList(energyList)
      avoided <- getAvoidedEmissionsSum
    } yield direct.map(_.eValue).sum + indirect.map(_.eValue).sum - avoided
  }

  //this does not subtract the avoided emissions due to renewables being present
  def nonActualTotalEmissions(energyList:EnergyList): Future[Double] = {
    for {
      direct <- getDirectEmissionList(energyList)
      indirect <- getIndirectEmissionList(energyList)
    } yield direct.map(_.eValue).sum + indirect.map(_.eValue).sum
  }

  def getDirectEmissionList(energyList:EnergyList): Future[List[EmissionsTuple]] = {

    for {
      energyTuples <- computeEnergyAndType(energyList)
      directFactors <- emissionsDirectFactors(energyTuples)
    } yield directFactors

  }

  def getIndirectEmissionList(energyList:EnergyList): Future[List[EmissionsTuple]] = {

    for {
      energyTuples <- computeEnergyAndType(energyList)
      eGridCode <- {
        buildingProps.country match {
          case "USA" => getEGrid()
          case _ => Future("Foreign")
        }
      }
      indirectFactors <- emissionsIndirectFactors(energyTuples, eGridCode)
    } yield indirectFactors

  }


  def getAvoidedEmissionsSum(): Future[Double] = {

    val local = for {
      entries <- energyCalcs.getRenewableEnergyList
      energyTuples <- computeEnergyAndType(entries)
      eGridCode <- getEGrid()
      indirectFactors <- emissionsIndirectFactors(energyTuples, eGridCode)
    } yield {indirectFactors.map(_.eValue).sum}
    local.recoverWith{
      case NonFatal(th) =>  Future{0}
    }
  }

  def computeEnergyAndType[T](entries: T): Future[List[(Energy, String)]] = Future {
    entries match {
      case a: EnergyList => a.energies.map {
        case b: EnergyMetrics => {
          energyCalcs.mapEnergy(b.energyUnits,b.energyUse) match {
            case Some(c) => (MBtus(c to MBtus), b.energyType)
            case None => throw new Exception("Could not convert energy to MBtus for Emissions Calc")
          }
        }
      }
      case a: RenewableEnergyList => a.renewableEnergies.map {
        case b: EnergyMetrics => {
          energyCalcs.mapEnergy(b.energyUnits,b.energyUse) match {
            case Some(c) => (MBtus(c to MBtus), b.energyType)
            case None => throw new Exception("Could not convert energy to MBtus for Emissions Calc")
          }
        }
      }
      case _ => throw new Exception("Could not validate energy entries to calculate EUI")
    }
  }

  def emissionsDirectFactors(energyEntry: List[(Energy, String)]): Future[List[EmissionsTuple]] = Future {

    energyEntry.map {
      case a: (Energy, String) => {
        val emissionValue:Double = (a._2, buildingProps.country, buildingProps.state) match {


          //Direct Emissions USA
          case ("naturalGas", "USA", _) => 53.11
          case ("propane", "USA", _) => 64.25
          case ("fuelOil1", "USA", _) => 73.50
          case ("fuelOil2", "USA", _) => 74.21
          case ("fuelOil4", "USA", _) => 75.30
          case ("fuelOil6", "USA", _) => 75.35
          case ("diesel", "USA", _) => 74.21
          case ("kerosene", "USA", _) => 77.69
          case ("coalA", "USA", _) => 104.44
          case ("coalB", "USA", _) => 94.03
          case ("coke", "USA", _) => 114.42

          //Direct Emissions Canada
          case ("propane", "Canada", _) => 64.36
          case ("fuelOil1", "Canada", _) => 74.37
          case ("fuelOil2", "Canada", _) => 74.37
          case ("fuelOil4", "Canada", _) => 74.37
          case ("fuelOil6", "Canada", _) => 78.08
          case ("diesel", "Canada", _) => 76.85
          case ("kerosene", "Canada", _) => 71.24
          case ("coalA", "Canada", _) => 94.35
          case ("coalB", "Canada", _) => 97.41
          case ("coke", "Canada", _) => 91.01

          case ("naturalGas", "Canada", "AB") => 52.98
          case ("naturalGas", "Canada", "BC") => 52.92
          case ("naturalGas", "Canada", "MB") => 51.85
          case ("naturalGas", "Canada", "NB") => 52.23
          case ("naturalGas", "Canada", "NL") => 52.23
          case ("naturalGas", "Canada", "NS") => 52.24
          case ("naturalGas", "Canada", "NT") => 67.69
          case ("naturalGas", "Canada", "NU") => 67.69
          case ("naturalGas", "Canada", "ON") => 51.91
          case ("naturalGas", "Canada", "PE") => 52.23
          case ("naturalGas", "Canada", "QC") => 51.88
          case ("naturalGas", "Canada", "SK") => 50.29
          case ("naturalGas", "Canada", "YT") => 67.69

          case (_, _, _) => 0.0
        }
        EmissionsTuple(a._2,emissionValue/1000*a._1.value)
      }
    }
  }




  def emissionsIndirectFactors(energyEntry: List[(Energy, String)], eGrid: String): Future[List[EmissionsTuple]] = Future {

    energyEntry.map {
      case a: (Energy, String) => {
        val emissionValue:Double = (a._2, buildingProps.country, buildingProps.state, eGrid) match {

          //Indirect Emissions USA
          case ("steam", "USA", _, _) => 66.40
          case ("hotWater", "USA", _, _) => 66.40
          case ("chilledWater", "USA", _, _) => 58.63 //this is average of different chilledWater (See Below)
          case ("wood", "USA", _, _) => 95.05

          //Indirect Emissions Canada
          case ("steam", "Canada", _, _) => 88.54
          case ("hotWater", "Canada", _, _) => 88.54
          case ("chilledWater", "Canada", _, _) => 46.78 //this is average of different chilledWater (See Below)
          case ("wood", "Canada", _, _) => 99.32

          //this is the eGRID data
          case ("grid", "USA", _, "AKGD") => 167.46
          case ("grid", "USA", _, "AKMS") => 59.84
          case ("grid", "USA", _, "AZNM") => 157.24
          case ("grid", "USA", _, "CAMX") => 81.54
          case ("grid", "USA", _, "ERCT") => 162.56
          case ("grid", "USA", _, "FRCC") => 159.77
          case ("grid", "USA", _, "HIMS") => 177.63
          case ("grid", "USA", _, "HIOA") => 216.83
          case ("grid", "USA", _, "MROE") => 215.31
          case ("grid", "USA", _, "MROW") => 205.38
          case ("grid", "USA", _, "NEWE") => 96.75
          case ("grid", "USA", _, "NWPP") => 112.58
          case ("grid", "USA", _, "NYCW") => 82.93
          case ("grid", "USA", _, "NYLI") => 178.30
          case ("grid", "USA", _, "NYUP") => 72.90
          case ("grid", "USA", _, "RFCE") => 133.87
          case ("grid", "USA", _, "RFCM") => 217.77
          case ("grid", "USA", _, "RFCW") => 200.91
          case ("grid", "USA", _, "RMPA") => 253.39
          case ("grid", "USA", _, "SPNO") => 240.42
          case ("grid", "USA", _, "SPSO") => 211.03
          case ("grid", "USA", _, "SRMV") => 137.40
          case ("grid", "USA", _, "SRMW") => 241.97
          case ("grid", "USA", _, "SRTV") => 185.63
          case ("grid", "USA", _, "SRVC") => 143.50


          case ("grid", "Canada", "AB", _) => 316.53
          case ("grid", "Canada", "BC", _) => 9.96
          case ("grid", "Canada", "MB", _) => 1.76
          case ("grid", "Canada", "NB", _) => 178.78
          case ("grid", "Canada", "NL", _) => 6.15
          case ("grid", "Canada", "NS", _) => 243.26
          case ("grid", "Canada", "NT", _) => 115.77
          case ("grid", "Canada", "NU", _) => 115.77
          case ("grid", "Canada", "ON", _) => 35.17
          case ("grid", "Canada", "PE", _) => 6.45
          case ("grid", "Canada", "QC", _) => 0.88
          case ("grid", "Canada", "SK", _) => 257.91
          case ("grid", "Canada", "YT", _) => 14.65


          case ("chilledWaterElectric", "USA", _, _) => 52.70
          case ("chilledWaterAbsorption", "USA", _, _) => 73.89
          case ("chilledWaterEngine", "USA", _, _) => 49.31

          case ("chilledWaterElectric", "Canada", _, _) => 17.19
          case ("chilledWaterAbsorption", "Canada", _, _) => 73.86
          case ("chilledWaterEngine", "Canada", _, _) => 49.29

          case (_, _, _, _) => 0.0
        }
        EmissionsTuple(a._2,emissionValue/1000*a._1.value)
      }
      case _ => throw new Exception("Could not match correct postal code for Emissions Factor")
    }
  }

  lazy val eGridLookUp = loadEGridLookUp

  def loadEGridLookUp: Future[JsValue] = {
    for {
      is <- Future(play.api.Environment.simple().resourceAsStream("eGridDict.json"))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case i => throw new Exception("eGrid - Could not open file: %s".format(i))
        }
      }
    } yield json
  }


  def getEGrid(): Future[String] = {
    val eGridCode = eGridLookUp.map { case a => (a \ buildingProps.postalCode).toOption }
    eGridCode.map {
      case Some(a) => a.as[String]
      case _ => throw new Exception("Could not find PostalCode in eGridDict.json")
    }
  }
}


case class EmissionsTuple(eType: String, eValue: Double)

