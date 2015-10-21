
package models


import squants_
import scala.language.implicitConversions
import scala.math._


import play.api.libs.json._
import play.api.libs.json.Reads._





/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {
  val floorArea:PosDouble
  val regressionSegments: Seq[RegressionSegment]
  def energyReduce:Double = regressionSegments.map(_.reduce).sum
  def expectedEnergy = energyReduce
 // val load1: Power = Kilowatts(12)

  implicit def bool2int(b:Boolean):Int = if (b) 1 else 0

}


case class PosInt(value: Int)
object PosInt {

  implicit def posIntToInt(posInt:PosInt):Int = posInt.value
  implicit def posIntToDouble(posInt:PosInt):Double = posInt.value.toDouble

  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))

}

case class PosDouble(value: Double)
object PosDouble {

  implicit def posDoubleToDouble(posDouble:PosDouble):Double = posDouble.value

  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))

}


/**
 *
 * @param a Coefficient
 * @param b Centering Variable
 * @param c User Input
 */

case class RegressionSegment(a:Double, b:Double, c:Double) {
def reduce:Double = {Console.println(a * (c - b))
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

    r.map { case (a: BaseLine) => a.expectedEnergy }

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
* @param floorArea
* @param areaUnits
*/



case class Office(floorArea:PosDouble, numComputers:PosInt, weeklyOperatingHours: PosInt, percentHeated:PosInt,
                  percentCooled:PosInt, HDD:PosInt, CDD:PosInt, isSmallBank:Boolean, numWorkersMainShift:PosInt,
                  areaUnits:String) extends BaseLine {



val regressionSegments = Seq[RegressionSegment] (
  RegressionSegment(186.6, 0, 1), // regression constant
  RegressionSegment(34.17, 9.535, math.min(log(floorArea.value),200000)),
  RegressionSegment(17.28, 2.231, math.min(numComputers.value / floorArea.value * 1000, 11.1)),
  RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
  RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / floorArea.value * 1000)),
  RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value),
  RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value),
  RegressionSegment(-64.83 * isSmallBank, 9.535, log(floorArea.value)),
  RegressionSegment(34.2 * isSmallBank, .5616, log(numWorkersMainShift.value / floorArea.value * 1000)),
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
 * @param floorArea
 * @param areaUnits
 */

case class CanadaOffice(weeklyOperatingHours:PosInt, numWorkersMainShift:PosInt, numComputers:PosInt,
                percentCooled:PosInt, HDD:PosInt, CDD:PosInt, numServers:PosInt, floorArea:PosDouble,
                areaUnits:String) extends BaseLine {

val regressionSegments = Seq[RegressionSegment] (
  RegressionSegment(1.788, 0, 1), // regression constant
  RegressionSegment(.006325, 57.95, weeklyOperatingHours.value),
  RegressionSegment(.06546, 3.492, numWorkersMainShift.value / floorArea.value * 100),
  RegressionSegment(.07455, 3.335, (numComputers.value + numServers.value) / floorArea.value * 100),
  RegressionSegment(.3643, 7.36, log(math.min(floorArea.value,5000))), // floorArea capped @ 5,000 sq meters during analysis
  RegressionSegment(-0.00002596, 2933, math.min(floorArea.value,5000)), // floorArea capped @ 5,000 sq meters during analysis
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
















/*
FOR WHEN WE WANT TO DO VALIDATION FOR EACH INDIVIDUAL FIELD

def checkFloorArea(f:Double):Boolean = log(f) < 200000
def checkNumComputers(floorArea:Double,numComputers:Int):Boolean = numComputers*1000/floorArea < 11.1

implicit val officeReads: Reads[Office] = (

    (JsPath \ "floorArea").read[Double].filter(ValidationError("Building too large."))(checkFloorArea(_)) and   // log(floorArea) < 200,000 sq ft
      (((JsPath \ "floorArea").read[Double] and // (numComputers / floorArea * 1000) > 11.1
        (JsPath \ "numComputers").read[Int]).tupled.filter(ValidationError("Too many computers present: max of 1.25 computers per sq. ft.")){ case (a,b) => checkNumComputers(a,b) } andKeep // (numComputers / floorArea * 1000) > 11.1
          (JsPath \ "numComputers").read[Int]) and // (numComputers / floorArea * 1000) > 11.1
            (JsPath \ "weeklyOperatingHours").read[Int] and
              (JsPath \ "percentHeated").read[Int] and
                (JsPath \ "percentCooled").read[Int] and
                  (JsPath \ "HDD").read[Int] and
                    (JsPath \ "CDD").read[Int] and
                      (JsPath \ "isSmallBank").read[Boolean] and
                        (JsPath \ "numWorkersMainShift").read[Int] and
                          (JsPath \ "areaUnits").read[String]
)(apply _)


case class posInt(i:Int) {

// make sure all the integers that are read are positive!
implicit object intReads extends Reads[Int] {
  def reads(json: JsValue) = json match {
    case JsNumber(n) if n < 0 => JsSuccess(n.toInt)
    case _ => JsError("error.expected.positiveInt")
  }
}

}


object PosInt {
implicit def posIntToInt(value: PosInt):Int = value.value
implicit def intToPosInt(value:Int):PosInt = PosInt(value)
implicit def readPosInt(_.read[Int](Reads.min(0))
implicit val readsFormatter: Reads[PosInt] = __.read[Int](Reads.min(0))

def applyInt(value:Int):PosInt = {
  if (value >= 0) {
    new PosInt(value)
  } else {
    throw new PosIntException
  }
}
}
*/
