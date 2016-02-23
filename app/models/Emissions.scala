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

import scala.util.{Failure, Success, Try}


case class Emissions(parameters:JsValue) {


  val country: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.country
      case _ => throw new Exception("Could not retrieve Country")
    }
  }

  val postalCode: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.postalCode
      case _ => throw new Exception("Could not retrieve PostalCode")
    }
  }
  val state: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.state
      case _ => throw new Exception("Could not retrieve State")
    }
  }


  def getDirectEmissionList(): Future[List[(Double,String)]] = {

    for {
      entries <- getEnergyList
      energyTuples <- computeEnergyAndType(entries)
      directFactors <- emissionsDirectFactors(energyTuples)

    } yield {
      directFactors
    }
  }

  def getIndirectEmissionList(): Future[List[(Double,String)]] = {

    for {
      entries <- getEnergyList
      energyTuples <- computeEnergyAndType(entries)
      eGridCode <- getEGrid()
      indirectFactors <- emissionsIndirectFactors(energyTuples, eGridCode)

    } yield {
      indirectFactors
    }
  }


  def getTotalEmissions(): Future[Double] = {

    for {
      direct <- getDirectEmissionList
      indirect <- getIndirectEmissionList
    } yield direct.map(_._1).sum + indirect.map(_._1).sum
  }


  def computeEnergyAndType[T](entries: T): Future[List[(Energy, String)]] = Future {
    entries match {
      case a: EnergyList => a.energies.map {
        case b: EnergyMetrics => {
          Energy((b.energyUse, b.energyUnits)) match {
            case c: Success[Energy] => (c.get in MBtus, b.energyType)
            case c: Failure[Energy] => throw new Exception("Could not convert energy to MBtus for Emissions Calc")
          }
        }
      }
      case _ => throw new Exception("Could not validate energy entries to calculate EUI")
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


  def emissionsDirectFactors(energyEntry: List[(Energy, String)]): Future[List[(Double,String)]] = Future {

    energyEntry.map {
      case a: (Energy, String) => {
        (a._2, country, state) match {

          //Direct Emissions USA
          case ("naturalGas", "USA", _) => (53.11/1000*a._1.value,a._2)
          case ("propane", "USA", _) => (64.25/1000*a._1.value,a._2)
          case ("fuelOil1", "USA", _) => (73.50/1000*a._1.value,a._2)
          case ("fuelOil2", "USA", _) => (74.21/1000*a._1.value,a._2)
          case ("fuelOil4", "USA", _) => (75.30/1000*a._1.value,a._2)
          case ("fuelOil6", "USA", _) => (75.35/1000*a._1.value,a._2)
          case ("diesel", "USA", _) => (74.21/1000*a._1.value,a._2)
          case ("kerosene", "USA", _) => (77.69/1000*a._1.value,a._2)
          case ("coalA", "USA", _) => (104.44/1000*a._1.value,a._2)
          case ("coalB", "USA", _) => (94.03/1000*a._1.value,a._2)
          case ("coke", "USA", _) => (114.42/1000*a._1.value,a._2)

          //Direct Emissions Canada
          case ("propane", "Canada", _) => (64.36/1000*a._1.value,a._2)
          case ("fuelOil1", "Canada", _) => (74.37/1000*a._1.value,a._2)
          case ("fuelOil2", "Canada", _) => (74.37/1000*a._1.value,a._2)
          case ("fuelOil4", "Canada", _) => (74.37/1000*a._1.value,a._2)
          case ("fuelOil6", "Canada", _) => (78.08/1000*a._1.value,a._2)
          case ("diesel", "Canada", _) => (76.85/1000*a._1.value,a._2)
          case ("kerosene", "Canada", _) => (71.24/1000*a._1.value,a._2)
          case ("coalA", "Canada", _) => (94.35/1000*a._1.value,a._2)
          case ("coalB", "Canada", _) => (97.41/1000*a._1.value,a._2)
          case ("coke", "Canada", _) => (91.01/1000*a._1.value,a._2)

          case ("naturalGas", "Canada", "AB") => (52.98/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "BC") => (52.92/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "MB") => (51.85/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "NB") => (52.23/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "NL") => (52.23/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "NS") => (52.24/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "NT") => (67.69/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "NU") => (67.69/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "ON") => (51.91/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "PE") => (52.23/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "QC") => (51.88/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "SK") => (50.29/1000*a._1.value,a._2)
          case ("naturalGas", "Canada", "YT") => (67.69/1000*a._1.value,a._2)

          case ("grid", _, _) => (0.0,a._2)
          case (_, _, _) => throw new Exception("Could Not Convert to Source Energy")
        }
      }
    }
  }


  def emissionsIndirectFactors(energyEntry: List[(Energy, String)], eGrid: String): Future[List[(Double,String)]] = Future {

    energyEntry.map {
      case a: (Energy, String) => {
        (a._2, country, state, eGrid) match {

          //Indirect Emissions USA
          case ("steam", "USA", _, _) => (66.40/1000*a._1.value,a._2)
          case ("hotWater", "USA", _, _) => (66.40/1000*a._1.value,a._2)
          case ("chilledWater", "USA", _, _) => (58.63/1000*a._1.value,a._2) //this is average of different chilledWater (See Below)
          case ("wood", "USA", _, _) => (95.05/1000*a._1.value,a._2)

          //Indirect Emissions Canada
          case ("steam", "Canada", _, _) => (88.54/1000*a._1.value,a._2)
          case ("hotWater", "Canada", _, _) => (88.54/1000*a._1.value,a._2)
          case ("chilledWater", "Canada", _, _) => (46.78/1000*a._1.value,a._2) //this is average of different chilledWater (See Below)
          case ("wood", "Canada", _, _) => (99.32/1000*a._1.value,a._2)

          //this is the eGRID data
          case ("grid", "USA", _, "AKGD") => (167.46/1000*a._1.value,a._2)
          case ("grid", "USA", _, "AKMS") => (59.84/1000*a._1.value,a._2)
          case ("grid", "USA", _, "AZNM") => (157.24/1000*a._1.value,a._2)
          case ("grid", "USA", _, "CAMX") => (81.54/1000*a._1.value,a._2)
          case ("grid", "USA", _, "ERCT") => (162.56/1000*a._1.value,a._2)
          case ("grid", "USA", _, "FRCC") => (159.77/1000*a._1.value,a._2)
          case ("grid", "USA", _, "HIMS") => (177.63/1000*a._1.value,a._2)
          case ("grid", "USA", _, "HIOA") => (216.83/1000*a._1.value,a._2)
          case ("grid", "USA", _, "MROE") => (215.31/1000*a._1.value,a._2)
          case ("grid", "USA", _, "MROW") => (205.38/1000*a._1.value,a._2)
          case ("grid", "USA", _, "NEWE") => (96.75/1000*a._1.value,a._2)
          case ("grid", "USA", _, "NWPP") => (112.58/1000*a._1.value,a._2)
          case ("grid", "USA", _, "NYCW") => (82.93/1000*a._1.value,a._2)
          case ("grid", "USA", _, "NYLI") => (178.30/1000*a._1.value,a._2)
          case ("grid", "USA", _, "NYUP") => (72.90/1000*a._1.value,a._2)
          case ("grid", "USA", _, "RFCE") => (133.87/1000*a._1.value,a._2)
          case ("grid", "USA", _, "RFCM") => (217.77/1000*a._1.value,a._2)
          case ("grid", "USA", _, "RFCW") => (200.91/1000*a._1.value,a._2)
          case ("grid", "USA", _, "RMPA") => (253.39/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SPNO") => (240.42/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SPSO") => (211.03/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SRMV") => (137.40/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SRMW") => (241.97/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SRTV") => (185.63/1000*a._1.value,a._2)
          case ("grid", "USA", _, "SRVC") => (143.50/1000*a._1.value,a._2)


          case ("grid", "Canada", "AB", _) => (316.53/1000*a._1.value,a._2)
          case ("grid", "Canada", "BC", _) => (9.96/1000*a._1.value,a._2)
          case ("grid", "Canada", "MB", _) => (1.76/1000*a._1.value,a._2)
          case ("grid", "Canada", "NB", _) => (178.78/1000*a._1.value,a._2)
          case ("grid", "Canada", "NL", _) => (6.15/1000*a._1.value,a._2)
          case ("grid", "Canada", "NS", _) => (243.26/1000*a._1.value,a._2)
          case ("grid", "Canada", "NT", _) => (115.77/1000*a._1.value,a._2)
          case ("grid", "Canada", "NU", _) => (115.77/1000*a._1.value,a._2)
          case ("grid", "Canada", "ON", _) => (35.17/1000*a._1.value,a._2)
          case ("grid", "Canada", "PE", _) => (6.45/1000*a._1.value,a._2)
          case ("grid", "Canada", "QC", _) => (0.88/1000*a._1.value,a._2)
          case ("grid", "Canada", "SK", _) => (257.91/1000*a._1.value,a._2)
          case ("grid", "Canada", "YT", _) => (14.65/1000*a._1.value,a._2)

          /*
          Chilled Water different emissions depending on Electric/Absorption/Engine
          case ("chilledWater", "USA", _, _) => 52.70/1000*a._1.value -- Electric
          case ("chilledWater", "USA", _, _) => 73.89/1000*a._1.value -- Absorption
          case ("chilledWater", "USA", _, _) => 49.31/1000*a._1.value -- Engine

          case ("chilledWater", "Canada", _, _) => 17.19/1000*a._1.value -- Electric
          case ("chilledWater", "Canada", _, _) => 73.86/1000*a._1.value -- Absorption
          case ("chilledWater", "Canada", _, _) => 49.29/1000*a._1.value -- Engine
          */
          case (_, _, _, _) => (0.0,a._2)
        }

      }
      case _ => throw new Exception("Could not match correct postal code for Emissions Factor")
    }
  }

  lazy val eGridLookUp = loadEGridLookUp

  def loadEGridLookUp: Future[JsValue] = {
    for {
      is <- Future(Play.current.resourceAsStream("eGridDict.json"))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case _ => throw new Exception("Could not open file")
        }
      }
    } yield json
  }


  def getEGrid(): Future[String] = {
    val eGridCode = eGridLookUp.map { case a => (a \ postalCode).toOption }
    eGridCode.map {
      case Some(a) => a.as[String]
      case _ => throw new Exception("Could not find PostalCode in eGridDict.json")
    }
  }

}




