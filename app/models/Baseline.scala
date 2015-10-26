
package models

import squants.space._
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.libs.json.Reads._


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
    try {
      r.map { case (a: BaseLine) => a.expectedEnergy }
    } catch {
      case e: Exception => JsError("Could Not Compute Expected Energy")
    }
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




