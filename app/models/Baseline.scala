
package models

import net.sf.ehcache.search.expression.And
import squants.energy.{Energy, KilowattHours}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._
import java.io.{InputStream, FileReader, FileNotFoundException, IOException}
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError




case class EUIMetrics(parameters: JsValue) {


  val predictedEUI: JsResult[Double] = Building.getExpectedEnergy(parameters) match {
    case JsSuccess(score, _) => {
      JsSuccess(score)
    }
    case JsError(err) => JsError("EUI could not be predicted from equation: " + err)
  }

  val sourceEUI: JsResult[Double] = EnergyCalcs.getEUI(parameters) match {
    case JsSuccess(score, _) => {
      JsSuccess(score)
    }
    case JsError(err) => JsError("Source EUI could not be calculated: " + err)
  }

  val euiRatio: Double = sourceEUI.get / predictedEUI.get
  Console.println(euiRatio)

  val ES: Future[Int] = {
    for {
      lookUp <- getLookupTable(parameters: JsValue)
      futureRatio <- loadLookupTable(lookUp).map {
        _.dropWhile(_.Ratio < euiRatio).headOption
      }
      checkRatio <- loadLookupTable(lookUp).map {
        _.lastOption
      }
      lastRatio <- if (futureRatio.isDefined) {
        Future(futureRatio)
      } else {
        Future(checkRatio)
      }
      computeES <- Future(lastRatio.get.ES)

    } yield computeES

  }


  val targetRatio:Future[Option[Double]] = parameters.asOpt[TargetES] match {
    case Some(a) =>
      for {
        lookUp <- getLookupTable(parameters: JsValue)
        targetRatioEntry <- loadLookupTable(lookUp).map {
          _.filter(_.ES == a.target).last.Ratio
        }

      } yield Some(targetRatioEntry)
    case None => Future(None)
  }

  targetRatio.map(println)





  def loadLookupTable(filename:String): Future[Seq[JsonEntry]] = {
    for {
      is <- Future(Play.current.resourceAsStream(filename))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case _ => throw new Exception("Could not open file")
        }
      }
      obj <- Future {
        json.validate[Seq[JsonEntry]] match {
          case JsSuccess(a, _) => a
          case JsError(th) => throw new Exception("Cannot find this: " + th.toString())
        }
      }
    } yield obj
  }


  def getLookupTable(parameters: JsValue): Future[String] = {

    val r = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => Play.current.configuration.getString("baseline.office")
      case Some(CountryBuildingType("Canada", "Office")) => Play.current.configuration.getString("baseline.canadaOffice")
      case Some(_) => None
      case None => None
    }
    Future(r.getOrElse("Lookup Table Not Found"))
  }


}

object EUIMetrics {}

case class TargetES(target:Int)
object TargetES {
  implicit val TargetReads: Reads[TargetES] = (JsPath \ "targetScore").read[Int](Reads.min(0) andKeep
    Reads.max(100)).map(new TargetES(_))
}



case class JsonEntry(ES: Int, CmPercent: Double, Ratio: Double)
object JsonEntry {
  implicit val formatFileName:Reads[JsonEntry] = Json.format[JsonEntry]
}

/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {

  val regressionSegments: Seq[RegressionSegment]
  def energyReduce:Double = regressionSegments.map(_.reduce).sum
  def expectedEnergy = energyReduce
  def squareFunction(s: String):String = "Square" + s.capitalize

  implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

  def convertToMetric(unitType:String,unitValue:String,data:Double):Option[Double] = {
    val metricValue: Option[Double] = (unitType,unitValue) match {
      case ("area","mSQ") => Some(SquareMeters(data) to SquareMeters)
      case ("area","ftSQ") => Some(SquareFeet(data) to SquareMeters)
      case (_,_)  =>  null
    }
    metricValue
  }

  def convertToStandard(unitType:String,unitValue:String,data:Double):Option[Double] = {
    val standardValue: Option[Double] = (unitType,unitValue) match {
      case ("area","mSQ") => Some(SquareMeters(data) to SquareFeet)
      case ("area","ftSQ") => Some(SquareFeet(data) to SquareFeet)
      case (_,_)  =>  null
    }
    standardValue
  }
}

case class PosInt(value: Int)
object PosInt {
  //implicit def posIntToInt(posInt:PosInt):Int = posInt.value   --------> These don't do anything right now and so we are forced to use PosInt.value below
  //implicit def posIntToDouble(posInt:PosInt):Double = posInt.value.toDouble
  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))
}

case class PosDouble(value: Double)
object PosDouble {
 // implicit def posDoubleToDouble(posDouble:PosDouble):Double = posDouble.value  --------> These don't do anything right now and so we are forced to use PosDouble.value below
  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))
}

/**
 *
 * @param a Coefficient
 * @param b Centering Variable
 * @param c User Input
 */

case class RegressionSegment(a:Double, b:Double, c:Double) {
def reduce:Double = {
  //Console.println(a * (c - b))
  a * (c - b) }
}


/**
* Maps the json to the correct BaseLine type.
*/
object Building {
  def getExpectedEnergy(parameters: JsValue): JsResult[Double] = {
    val r = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(CountryBuildingType("Canada", "Office")) => parameters.validate[CanadaOffice]
      case Some(_) => JsError("No matching building by country and type!")
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    for (
      expectedEnergy <- r.map { case (a: BaseLine) => a.expectedEnergy }
    ) yield expectedEnergy
  }

}



/**
* Class to manage the decomposing of Country and Building from the input JSON
* @param country country that the building resides in
* @param buildingType type of building
*/
case class CountryBuildingType(country: String, buildingType: String)

object CountryBuildingType {
implicit val countryBuildingTypeRead: Reads[CountryBuildingType] = Json.reads[CountryBuildingType]
}

/**
* Office Building Type parameters
* @param weeklyOperatingHours
* @param numWorkersMainShift
* @param numComputers
* @param percentHeated
* @param percentCooled
* @param HDD
* @param CDD
* @param isSmallBank  "if is bank branch or financial office AND < 50,000 sq ft in area"
* @param GFA
* @param areaUnits
*/

case class Office(GFA:PosDouble, numComputers:PosInt, weeklyOperatingHours: PosInt, percentHeated:PosInt,
                  percentCooled:PosInt, HDD:PosInt, CDD:PosInt, isSmallBank:Option[Boolean], numWorkersMainShift:PosInt,
                  areaUnits:String) extends BaseLine {

  val floorArea:Double = convertToStandard("area",areaUnits,GFA.value).get

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(floorArea),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / floorArea * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / floorArea * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value),
    RegressionSegment(-64.83 * isSmallBank, 9.535, log(floorArea)),
    RegressionSegment(34.2 * isSmallBank, .5616, log(numWorkersMainShift.value / floorArea * 1000)),
    RegressionSegment(56.3 * isSmallBank, 0, 1)
  )
}

/**
* Office companion object.  Contains built in JSON validation.
*/
object Office {
implicit val officeReads: Reads[Office] = Json.reads[Office]
}

/**
 * Canada Office
 * @param weeklyOperatingHours
 * @param numWorkersMainShift
 * @param numComputers
 * @param percentCooled
 * @param HDD
 * @param CDD
 * @param numServers
 * @param GFA
 * @param areaUnits
 */

case class CanadaOffice(weeklyOperatingHours:PosInt, numWorkersMainShift:PosInt, numComputers:PosInt,
                percentCooled:PosInt, HDD:PosInt, CDD:PosInt, numServers:PosInt, GFA:PosDouble,
                areaUnits:String) extends BaseLine {

  val floorArea:Double = convertToMetric("area",areaUnits,GFA.value).get

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.788, 0, 1), // regression constant
    RegressionSegment(.006325, 57.95, weeklyOperatingHours.value),
    RegressionSegment(.06546, 3.492, numWorkersMainShift.value / floorArea * 100),
    RegressionSegment(.07455, 3.335, (numComputers.value + numServers.value) / floorArea * 100),
    RegressionSegment(.3643, 7.36, log(math.min(floorArea,5000))), // floorArea capped @ 5,000 sq meters during analysis
    RegressionSegment(-0.00002596, 2933, math.min(floorArea,5000)), // floorArea capped @ 5,000 sq meters during analysis
    RegressionSegment(.0002034, 4619, HDD.value),
    RegressionSegment(.06386, 3.703, log(CDD.value) * percentCooled.value)
  )
}

/**
* Office companion object.  Contains built in JSON validation.
*/
object CanadaOffice {
implicit val canadaOfficeReads: Reads[CanadaOffice] = Json.reads[CanadaOffice]
}




