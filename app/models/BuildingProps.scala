package models

import squants.energy.{TBtus, Gigajoules, KBtus, Energy}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._

case class BuildingProperties(parameters: JsValue) {

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

  def postalCode: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.postalCode match {
          case Some(postalCode:String) => postalCode
          case _ => throw new Exception("Could not retrieve Postal Code")
        }
      }
      case _ => throw new Exception("Could not retrieve Postal Code")

    }
  }
  def state: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.state match {
          case Some(state:String) => state
          case None => throw new Exception("Could not retrieve State")
        }
      }
      case _ => throw new Exception("Could not retrieve State")
    }
  }
  def buildingName: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.buildingName match {
          case Some(name:String) => name
          case None => "Anonymous"
        }
      }
      case _ => throw new Exception("Could not retrieve Building Name")
    }
  }

  def getBuilding: Future[BaseLine] = Future{

    val building: JsResult[BaseLine] = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(_) => parameters.validate[GenericBuilding]
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    building match {
      case JsSuccess(a: BaseLine, _) => a
      case JsError(err) => throw new Exception("Building Type parameters fail validation!")
    }
  }
}

case class PosInt(value: Int)
object PosInt {
  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))
}

case class PosDouble(value: Double)
object PosDouble {
  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))
}
case class BuildingArea(GFA:PosDouble)
object BuildingArea {
  implicit val buildingAreaReads: Reads[BuildingArea] = Json.reads[BuildingArea]
}

case class PropParams(propType:String,propSize:Double,propPercent:Double,areaUnits:String)

/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {
  val country:String
  val buildingType:String
  val GFA:PosDouble
  val areaUnits:String
  val printed:String
  val regressionSegments: Seq[RegressionSegment]

  def energyReduce:Double = regressionSegments.map(_.reduce).sum
  def expectedEnergy = energyReduce

  implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

  def converseBoolean(i:Int):Int = {if (i==0) {1} else if (i==1) {0} else {0}}

  val buildingSize:Double = {
    country match {
      case "USA" => (Area((GFA.value,areaUnits)).get to SquareFeet)
      case "Canada" => (Area((GFA.value,areaUnits)).get to SquareMeters)
    }
  }

  def getLog(x:Double):Double = {
    x match {
      case (0) => 0.0
      case x => log(x)
    }
  }

}
/**
  *
  * @param a Coefficient
  * @param b Centering Variable
  * @param c User Input
  */

case class RegressionSegment(a:Double, b:Double, c:Double) {
  def reduce:Double = {a * (c - b)}
}

case class StateBuildingType(state: String, buildingType: String)

object StateBuildingType {
  implicit val stateBuildingTypeRead: Reads[StateBuildingType] = Json.reads[StateBuildingType]
}


/**
  * Class to manage the decomposing of Country and Building from the input JSON
  *
  * @param country country that the building resides in
  * @param buildingType type of building
  */
case class CountryBuildingType(country: String, buildingType: String)

object CountryBuildingType {
  implicit val countryBuildingTypeRead: Reads[CountryBuildingType] = Json.reads[CountryBuildingType]
}

case class GenericBuilding (GFA:PosDouble,areaUnits:String, country:String, buildingType:String) extends BaseLine {

  val regressionSegments = Seq[RegressionSegment]()

  val printed:String = {
    buildingType match {
    case "AdultEducation" => "Adult Education"
    case "College" => "College / University"
    case "PreSchool" => "Pre-school / DayCare"
    case "VocationalSchool" => "Vocational School"
    case "OtherEducation" => "Other Education"
    case "ConventionCenter" => "Convention Center"
    case "MovieTheater" => "Movie Theater"
    case "Museum" => "Museum"
    case "PerformingArts" => "Performing Arts"
    case "BowlingAlley" => "Bowling Alley"
    case "FitnessCenter" => "Fitness Center"
    case "IceRink" => "Ice / Curling Rink"
    case "RollerRink" => "Roller Rink"
    case "SwimmingPool" => "Swimming Pool"
    case "OtherRecreation" => "Other Recreation"
    case "Stadium" => "Stadium"
    case "IndoorArena" => "Indoor Arena"
    case "RaceTrack" => "Race Track"
    case "Aquarium" => "Aquarium"
    case "Bar" => "Bar"
    case "Nightclub" => "Nightclub"
    case "Casino" => "Casino"
    case "Zoo" => "Zoo"
    case "OtherEntertainment" => "Other Entertainment"
    case "GasStation" => "Convenience Store with Gas Station"
    case "ConvenienceStore" => "Convenience Store without Gas Station"
    case "FastFoodRestaurant" => "Fast Food Restaurant"
    case "Restaurant" => "Restaurant"
    case "Supermarket" => "Supermarket"
    case "WholesaleClub" => "Wholesale Club"
    case "FoodSales" => "Food Sales"
    case "FoodService" => "Food Service"
    case "AmbulatorySurgicalCenter" => "Ambulatory Surgical Center"
    case "Hospital" => "Hospital"
    case "SpecialtyHospital" => "Specialty Hospital"
    case "MedicalOffice" => "Medical Office"
    case "OutpatientCenter" => "Outpatient Rehabilitation Center"
    case "PhysicalTherapyCenter" => "Physical Therapy Center"
    case "SeniorCare" => "Senior Care Community"
    case "UrgentCareCenter" => "Urgent Care Center"
    case "Barracks" => "Barracks"
    case "Hotel" => "Hotel"
    case "MultiFamily" => "Multifamily Housing"
    case "Prison" => "Prison / Incarceration"
    case "ResidenceHall" => "Residence Hall"
    case "ResidentialLodging" => "Other Residential Lodging"
    case "MixedUse" => "Mixed Use Property"
    case "Office" => "Office"
    case "VeterinaryOffice" => "Veterinary Office"
    case "Courthouse" => "Courthouse"
    case "DrinkingWaterTreatment" => "Drinking Water Treatment Center"
    case "FireStation" => "Fire Station"
    case "Library" => "Library"
    case "PostOffice" => "Post Office"
    case "PoliceStation" => "Police Station"
    case "MeetingHall" => "Meeting Hall"
    case "TransportationTerminal" => "Transportation Terminal"
    case "WastewaterCenter" => "Wastewater Treatment Center"
    case "OtherPublicServices" => "Other Public Services"
    case "WorshipCenter" => "Worship Facility"
    case "AutoDealership" => "Automobile Dealership"
    case "EnclosedMall" => "Enclosed Mall"
    case "StripMall" => "Strip Mall"
    case "Retail" => "Retail Store"
    case "DataCenter" => "Data Center" //Data Centers behave very different and require custom script
    case "PersonalServices" => "Personal Services (Health/Beauty, Dry Cleaning, etc.)"
    case "RepairServices" => "Repair Services (Vehicle, Shoe Locksmith, etc.)"
    case "OtherServices" => "Other Services"
    case "PowerStation" => "Energy / Power Station"
    case "OtherUtility" => "Other Utility Station"
    case "SelfStorageFacility" => "Self Storage Facility"
    case "Warehouse" => "Warehouse / Distribution Center"
    case "RefrigeratedWarehouse" => "Refrigerated Warehouse"
    case "SingleFamilyDetached" => "Single Family - Detached"
    case "SingleFamilyAttached" => "Single Family - Attached"
    case "MobileHome" => "Mobile Home"
    case _ => "Other"
    }
  }



}
object GenericBuilding {
  implicit val genericBuildingTypeRead: Reads[GenericBuilding] = Json.reads[GenericBuilding]
}


/**
  *   Building Type parameters
  *
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

case class Office(GFA:PosDouble, numComputers:PosDouble, weeklyOperatingHours: PosDouble, percentHeated:PosDouble,
                  percentCooled:PosDouble, HDD:PosDouble, CDD:PosDouble, isSmallBank:Option[Boolean], numWorkersMainShift:PosDouble,
                  areaUnits:String, country:String, buildingType:String) extends BaseLine {



  val printed:String = "Office"
  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(buildingSize),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / buildingSize * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value / 100),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value / 100),
    RegressionSegment(0, 9.535, log(buildingSize)),
    RegressionSegment(0, .5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0, 0, 1)
  )
}
/**
  * Office companion object.  Contains built in JSON validation.
  */
object Office {
  implicit val officeReads: Reads[Office] = Json.reads[Office]
}
