package models


import squants.energy._
import squants.energy.EnergyConversions.EnergyNumeric

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.InputStream

import squants.space.{SquareFeet, SquareMeters}


case class CombinedPropTypes(params: JsValue) {

  val result = params.as[List[JsValue]]
  val buildingProps:BuildingProperties = BuildingProperties(result.head)


  def mediumSizeProps(buildingList:List[BaseLine]):Future[List[Boolean]] = {
    for {
      buildingSizeList <- Future{buildingList.map(a=>a.buildingSize)}
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      mediumSizePropTypes <-  Future(buildingSizeRatios.map{ case a => 0.25 < a && a < 0.5 })
    } yield mediumSizePropTypes
  }


  def getMajorProp(propFilter:List[Boolean]):Future[JsValue] = Future{
    (propFilter,result).zipped.collect{case (a,b) if a == true => b }.head
  }

  def filterPropTypes(propFilter:List[Boolean]):Future[List[JsValue]] = {
    for {
      buildingList <- Future{result}
      mediumPropList <- Future {
        (propFilter, buildingList).zipped.flatMap {
          case (a, b) if a == true => Some(b)
          case (_, _) => None
        }
      }
    } yield mediumPropList
  }

  def sumEnergy(energies:List[Energy]):Future[Energy] = Future{
    buildingProps.country match {
      case "USA" => energies.sum in KBtus
      case "Canada" => energies.sum in Gigajoules
    }
  }

  def checkGenericBuilding:Future[Boolean] = {
    for {
      genericCheck: List[Boolean] <- getBuildingList.map(_.map {
        case a: GenericBuilding => true
        case _ => false
      })
      testGeneric <- {
        genericCheck.contains(true) match {
          case true => Future(true)
          case false => Future(false)
        }
      }
    } yield testGeneric
  }

  def getBuildingList:Future[List[BaseLine]] = {
    Future.sequence(result.map(BuildingProperties(_).getBuilding))
  }

  def majorProp:Future[List[Boolean]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      majorPropType <-  Future(buildingSizeRatios.map{ case a => a > 0.5 })
    } yield majorPropType
  }

  def getAreaWeights:Future[List[Double]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
    } yield buildingSizeRatios
  }

  def getTotalArea(buildingList:List[JsValue]): Future[Double] = {
    for {
      propTypes <- Future.sequence(buildingList.map(BuildingProperties(_).getBuilding))
      propGFASum <- Future(propTypes.map(_.buildingSize).sum)
    } yield propGFASum
  }



/*


  def loadLookupTable(filename:String): Future[Seq[TableEntry]] = {
    for {
      is <- Future(Play.current.resourceAsStream(filename))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case i => throw new Exception("lookUpTable - Could not open file: %s".format(i))
        }
      }
      obj <- Future {
        json.validate[Seq[TableEntry]] match {
          case JsSuccess(a, _) => a
          case JsError(th) => throw new Exception("Cannot find this: " + th.toString())
        }
      }
    } yield obj
  }
*/


  def sourceMedianEUI(building:BaseLine):Future[Energy] = Future{

    val sourceMedian:Double = {
      (building.country,building.buildingType) match {
        case ("USA","AdultEducation") => 141.4
        case ("USA","College") => 262.6
        case ("USA","PreSchool") => 145.7
        case ("USA","DataCenter") => 1.821 //this = Total Energy / IT Energy
        case ("USA","VocationalSchool") => 141.4
        case ("USA","OtherEducation") => 141.4
        case ("USA","ConventionCenter") => 69.8
        case ("USA","MovieTheater") => 85.1
        case ("USA","Museum") => 85.1
        case ("USA","PerformingArts") => 85.1
        case ("USA","BowlingAlley") => 96.8
        case ("USA","FitnessCenter") => 96.8
        case ("USA","IceRink") => 96.8
        case ("USA","RollerRink") => 96.8
        case ("USA","SwimmingPool") => 96.8
        case ("USA","OtherRecreation") => 96.8
        case ("USA","MeetingHall") => 69.8
        case ("USA","IndoorArena") => 85.1
        case ("USA","RaceTrack") => 85.1
        case ("USA","Stadium") => 85.1
        case ("USA","Aquarium") => 85.1
        case ("USA","Bar") => 85.1
        case ("USA","NightClub") => 85.1
        case ("USA","Casino") => 85.1
        case ("USA","Zoo") => 85.1
        case ("USA","OtherEntertainment") => 85.1
        case ("USA","ConvenienceStore") => 536.3
        case ("USA","GasStation") => 536.3
        case ("USA","FastFoodRestaurant") => 1015.3
        case ("USA","Restaurant") => 432.0
        case ("USA","OtherDining") => 432.0
        case ("USA","FoodSales") => 536.3
        case ("USA","FoodService") => 543.2
        case ("USA","AmbulatorySurgicalCenter") => 155.2
        case ("USA","DrinkingWaterTreatment") => 6.61
        case ("USA","SpecialtyHospital") => 389.8
        case ("USA","OutpatientCenter") => 155.2
        case ("USA","PhysicalTherapyCenter") => 155.2
        case ("USA","UrgentCareCenter") => 182.7
        case ("USA","Barracks") => 114.9
        case ("USA","Prison") => 169.9
        case ("USA","ResidentialLodging") => 155.5
        case ("USA","MixedUse") => 123.1
        case ("USA","VeterinaryOffice") => 182.7
        case ("USA","Courthouse") => 169.9
        case ("USA","FireStation") => 154.4
        case ("USA","Library") => 235.6
        case ("USA","MailingCenter") => 100.4
        case ("USA","PostOffice") => 100.4
        case ("USA","PoliceStation") => 154.4
        case ("USA","TransportationTerminal") => 85.1
        case ("USA","OtherPublicServices") => 123.1
        case ("USA","AutoDealership") => 130.1
        case ("USA","EnclosedMall") => 235.6
        case ("USA","StripMall") => 237.6
        case ("USA","Laboratory") => 123.1
        case ("USA","PersonalServices") => 100.4
        case ("USA","RepairServices") => 100.4
        case ("USA","OtherServices") => 100.4
        case ("USA","PowerStation") => 123.1
        case ("USA","OtherUtility") => 123.1
        case ("USA","SelfStorageFacility") => 47.6

        case ("USA","SingleFamilyDetached") => {
          getRegion(buildingProps.state) match {
            case "West" => 67.2
            case "Midwest" => 76.2
            case "South" => 86
            case "Northeast" => 67.5
          }
        }
        case ("USA","SingleFamilyAttached") => {
          getRegion(buildingProps.state) match {
            case "West" => 63.2
            case "Midwest" => 66.6
            case "South" => 82.5
            case "Northeast" => 68.6
          }
        }
        case ("USA","MultiFamilyLessThan5") => {
          getRegion(buildingProps.state) match {
            case "West" => 87.3
            case "Midwest" => 104.8
            case "South" => 113.6
            case "Northeast" => 78.8
          }
        }
        case ("USA","MultiFamilyMoreThan4") => {
          getRegion(buildingProps.state) match {
            case "West" => 81.7
            case "Midwest" => 93.3
            case "South" => 122.4
            case "Northeast" => 98.2
          }
        }
        case ("USA","MobileHome") => {
          getRegion(buildingProps.state) match {
            case "West" => 128.2
            case "Midwest" => 168.9
            case "South" => 162.0
            case "Northeast" => 145.5
          }
        }
        case ("USA",_) => 123.1

        //Canadian Building Medians
        case ("Canada","AdultEducation") => 1.44
        case ("Canada","College") => 1.56
        case ("Canada","PreSchool") => 1.27
        case ("Canada","VocationalSchool") => 1.44
        case ("Canada","OtherEducation") => 1.27
        case ("Canada","ConventionCenter") => 2.47
        case ("Canada","MovieTheater") => 1.63
        case ("Canada","Museum") => 2.47
        case ("Canada","PerformingArts") => 2.47
        case ("Canada","BowlingAlley") => 1.93
        case ("Canada","FitnessCenter") => 1.93
        case ("Canada","IceRink") => 1.93
        case ("Canada","RollerRink") => 1.93
        case ("Canada","SwimmingPool") => 1.93
        case ("Canada","OtherRecreation") => 1.91
        case ("Canada","MeetingHall") => 2.47
        case ("Canada","IndoorArena") => 1.93
        case ("Canada","RaceTrack") => 1.91
        case ("Canada","Stadium") => 1.93
        case ("Canada","Aquarium") => 2.47
        case ("Canada","Bar") => 1.63
        case ("Canada","NightClub") => 1.63
        case ("Canada","Casino") => 1.63
        case ("Canada","Zoo") => 2.47
        case ("Canada","OtherEntertainment") => 2.47
        case ("Canada","ConvenienceStore") => 5.16
        case ("Canada","GasStation") => 5.16
        case ("Canada","FastFoodRestaurant") => 4.21
        case ("Canada","Restaurant") => 4.21
        case ("Canada","OtherDining") => 4.21
        case ("Canada","FoodSales") => 5.16
        case ("Canada","FoodService") => 4.21
        case ("Canada","AmbulatorySurgicalCenter") => 1.5
        case ("Canada","DrinkingWaterTreatment") => 1.84
        case ("Canada","SpecialtyHospital") => 3.12
        case ("Canada","OutpatientCenter") => 1.5
        case ("Canada","PhysicalTherapyCenter") => 1.5
        case ("Canada","UrgentCareCenter") => 1.5
        case ("Canada","Barracks") => 2.05
        case ("Canada","Prison") => 1.74
        case ("Canada","ResidentialLodging") => 1.75
        case ("Canada","MixedUse") => 1.23
        case ("Canada","VeterinaryOffice") => 1.5
        case ("Canada","Courthouse") => 1.74
        case ("Canada","FireStation") => 1.63
        case ("Canada","Library") => 2.47
        case ("Canada","MailingCenter") => 1.67
        case ("Canada","PostOffice") => 1.67
        case ("Canada","PoliceStation") => 1.74
        case ("Canada","TransportationTerminal") => 1.42
        case ("Canada","OtherPublicServices") => 1.23
        case ("Canada","AutoDealership") => 1.52
        case ("Canada","EnclosedMall") => 3.47
        case ("Canada","StripMall") => 2.25
        case ("Canada","Laboratory") => 1.23
        case ("Canada","PersonalServices") => 1.37
        case ("Canada","RepairServices") => 1.37
        case ("Canada","OtherServices") => 2.20
        case ("Canada","PowerStation") => 1.23
        case ("Canada","OtherUtility") => 1.23
        case ("Canada","SelfStorageFacility") => 0.93
        // Canadian Building Medians for Buildings with US Algorithms
        case ("Canada","Hotel") => 1.75
        case ("Canada","WorshipCenter") => 1.06
        case ("Canada","Warehouse") => 0.93
        case ("Canada","RefrigeratedWarehouse") => 1.23
        case ("Canada","SeniorCare") => 1.88
        case ("Canada","Retail") => 1.52
        case ("Canada","ResidenceHall") => 2.05
        case ("Canada","DataCenter") => 1.82 //this = Total Energy / IT Energy

        case ("Canada",_) => 1.23

        case (_,_) => throw new Exception("Could not find Country and Building Type for Median EUI")
      }
    }

    building.country match {
      case "USA" => KBtus(sourceMedian)
      case "Canada" => Gigajoules(sourceMedian)
      case  _ => throw new Exception("Could not find Country and Building Type for Median EUI")
    }
  }

  def getRegion(state:String):String = {

    state match {
      case "WA" => "West"
      case "OR" => "West"
      case "CA" => "West"
      case "MT" => "West"
      case "ID" => "West"
      case "NV" => "West"
      case "WY" => "West"
      case "UT" => "West"
      case "CO" => "West"
      case "AZ" => "West"
      case "NM" => "West"
      case "AK" => "West"
      case "HI" => "West"

      case "ND" => "Midwest"
      case "SD" => "Midwest"
      case "NE" => "Midwest"
      case "KS" => "Midwest"
      case "MN" => "Midwest"
      case "IA" => "Midwest"
      case "MO" => "Midwest"
      case "WI" => "Midwest"
      case "IL" => "Midwest"
      case "MI" => "Midwest"
      case "IN" => "Midwest"
      case "OH" => "Midwest"

      case "OK" => "South"
      case "TX" => "South"
      case "AR" => "South"
      case "LA" => "South"
      case "KY" => "South"
      case "TN" => "South"
      case "MS" => "South"
      case "AL" => "South"
      case "WV" => "South"
      case "DE" => "South"
      case "MD" => "South"
      case "DC" => "South"
      case "VA" => "South"
      case "NC" => "South"
      case "SC" => "South"
      case "GA" => "South"
      case "FL" => "South"

      case "PA" => "Northeast"
      case "NY" => "Northeast"
      case "NJ" => "Northeast"
      case "CT" => "Northeast"
      case "RI" => "Northeast"
      case "MA" => "Northeast"
      case "VT" => "Northeast"
      case "NH" => "Northeast"
      case "ME" => "Northeast"

      //case _ => "Canada"
      case _ => throw new Exception("Could not find Country and Building Type for Median EUI")

    }
  }
}
