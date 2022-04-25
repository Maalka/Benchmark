/**
 * Created by rimukas on 10/12/15.
 */


package controllers
import models._
import com.google.inject.Inject
import play.api.cache.AsyncCacheApi
import play.api.libs.json._
import play.api.Configuration
import play.api.mvc._
import com.eclipsesource.schema.drafts.Version7._

import scala.concurrent.Future
import squants.energy.Energy

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import com.eclipsesource.schema.{FailureExtensions, SchemaType, SchemaValidator}
import com.typesafe.scalalogging.LazyLogging


class BaselineController @Inject() (
                                     val cache: AsyncCacheApi,
                                     cc: ControllerComponents,
                                     configuration: Configuration
                                   )
                                    extends AbstractController(cc) with LazyLogging {


  val validator: SchemaValidator = SchemaValidator()

  implicit def doubleToJSValue(d:Double):JsValue = Json.toJson(d)
  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)
  implicit def listEnergyToJSValue(v: List[Energy]): JsValue = Json.toJson(v.map{
    case e:Energy => e.value
  })

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T):Either[String, JsValue] = {
    response match {
      case v: Energy => Right(v)
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: List[Any] => Right{
        Json.toJson(v.map{
          case a:Energy => energyToJSValue(a)
          case a:EmissionsTuple => JsObject(Seq(a.eType -> Json.toJson(a.eValue)))
          case a:EnergyTuple => JsObject(Seq(a.energyType -> energyToJSValue(a.energyValue)))
          case a:TableEntry => JsObject(Seq(
            "ES" -> JsNumber(a.ES),
            "CmPercent" -> JsNumber(a.CmPercent),
            "Ratio" -> JsNumber(a.Ratio)
            ))
          case a:PropParams => JsObject(Seq(
            "propType" -> JsString(a.propType),
            "propTypeName" -> JsString(a.propTypeName),
            "propSize" -> JsNumber(a.propSize),
            "propPercent" -> JsNumber{
              a.propPercent match {
                case b => roundAt(2)(b*100)}
              },
            "areaUnits" -> JsString{
              a.areaUnits match {
                case "mSQ" => "sq.m"
                case "ftSQ" => "sq.ft"
              }
            }
          ))
        })
      }
      case v: String => Right(Json.toJson(v))
      case None => Left("Could not recognize input type")
    }
  }

  val schema = Json.fromJson[SchemaType](Json.parse(
    """{
       "id": "http://baseline.maalka.com/baseline",
       "type": "array",
       "items": [
           {
               "id": "/items",
               "type": "object",
               "properties": {
                   "CDD": {
                       "id": "/items/properties/CDD",
                       "minimum": 0,
                       "type": "number"
                   },
                   "GFA": {
                       "id": "/items/properties/GFA",
                       "minimum": 0,
                       "type": "number"
                   },
                   "HDD": {
                       "id": "/items/properties/HDD",
                       "minimum": 0,
                       "type": "number"
                   },
                   "annualITEnergy": {
                       "id": "/items/properties/annualITEnergy",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "areaUnits": {
                       "id": "/items/properties/areaUnits",
                       "type": "string",
                       "enum": ["ftSQ","mSQ"]
                   },
                   "avgNumResidents": {
                       "id": "/items/properties/avgNumResidents",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "baselineConstant": {
                       "id": "/items/properties/baselineConstant",
                       "type": "number",
                       "enum": [100,130]
                   },
                   "buildingName": {
                       "id": "/items/properties/buildingName",
                       "type": ["string","null"]
                   },
                   "buildingType": {
                       "id": "/items/properties/buildingType",
                       "type": "string",
                       "enum": ["AdultEducation","College","PreSchool","VocationalSchool","OtherEducation","ConventionCenter","MovieTheater","Museum","PerformingArts",
                       "BowlingAlley","FitnessCenter","IceRink","RollerRink","SwimmingPool","OtherRecreation","Stadium","FinancialOffice","DistributionCenter",
                       "Warehouse","WarehouseRefrigerated","WarehouseUnRefrigerated","SpecialtyHospital","MedicalOffice","OutpatientCenter","PhysicalTherapyCenter","SeniorCare",
                       "UrgentCareCenter","Barracks","Hotel","MultiFamily","Prison","ResidenceHall","ResidentialLodging", "OtherResidentialLodging","MixedUse","Office","VeterinaryOffice",
                       "Courthouse","OtherUtility","SelfStorageFacility","StripMall","Retail","PowerStation","EnergyStation","BankBranch","IndoorArena","RaceTrack","Aquarium",
                       "Bar","Nightclub","Casino","OtherEntertainment","GasStation", "ConvenienceStoreAndGas","ConvenienceStore","FastFoodRestaurant","Restaurant","Supermarket","WholesaleClub",
                       "FoodSales","FoodService","AmbulatorySurgicalCenter","Hospital","DrinkingWaterTreatment","FireStation","Library","PostOffice","PoliceStation","MeetingHall",
                       "TransportationTerminal","WastewaterCenter","OtherPublicServices","WorshipCenter","AutoDealership","EnclosedMall","DataCenter","PersonalServices",
                       "RepairServices","OtherServices","Zoo","K12School","Other","SingleFamilyDetached","SingleFamilyAttached","MobileHome"]
                   },
                   "city": {
                       "id": "/items/properties/city",
                       "type": ["string","null"]
                   },
                   "country": {
                       "id": "/items/properties/country",
                       "type": "string",
                       "enum": ["USA","Canada"]
                   },
                   "defaultValues": {
                       "id": "/items/properties/defaultValues",
                       "type": "boolean"
                   },
                   "gymFloorArea": {
                       "id": "/items/properties/gymFloorArea",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "hasCooking": {
                       "id": "/items/properties/hasCooking",
                       "type": ["boolean","null"]
                   },
                   "hasFoodPreparation": {
                       "id": "/items/properties/hasFoodPreparation",
                       "type": ["boolean","null"]
                   },
                   "hasLaundryFacility": {
                       "id": "/items/properties/hasLaundryFacility",
                       "type": ["boolean","null"]
                   },
                   "parkingAreaUnits": {
                     "id": "/items/properties/parkingAreaUnits",
                     "type": ["string","null"],
                     "enum": ["ftSQ","mSQ",null]
                   },
                   "hasParkingHeating": {
                       "id": "/items/properties/hasParkingHeating",
                       "type": ["boolean","null"]
                   },
                   "openParkingArea": {
                       "id": "/items/properties/openParkingArea",
                       "type": ["number","null"]
                   },
                   "partiallyEnclosedParkingArea": {
                       "id": "/items/properties/partiallyEnclosedParkingArea",
                       "type": ["number","null"]
                   },
                   "fullyEnclosedParkingArea": {
                       "id": "/items/properties/fullyEnclosedParkingArea",
                       "type": ["number","null"]
                   },
                   "totalParkingArea": {
                       "id": "/items/properties/totalParkingArea",
                       "type": ["number","null"]
                   },
                   "hasPool": {
                       "id": "/items/properties/hasPool",
                       "type": ["boolean","null"]
                   },
                   "indoorOutdoor": {
                       "id": "/items/properties/indoorOutdoor",
                       "type": ["boolean","null"]
                   },
                   "isHighSchool": {
                       "id": "/items/properties/isHighSchool",
                       "type": ["boolean","null"]
                   },
                   "isOpenAllWeekdays": {
                       "id": "/items/properties/isOpenAllWeekdays",
                       "type": ["boolean","null"]
                   },
                   "isOpenWeekends": {
                       "id": "/items/properties/isOpenWeekends",
                       "type": ["boolean","null"]
                   },
                   "isOutdoorPool": {
                       "id": "/items/properties/isOutdoorPool",
                       "type": ["boolean","null"]
                   },
                   "isSecondarySchool": {
                       "id": "/items/properties/isSecondarySchool",
                       "type": ["boolean","null"]
                   },
                   "isSmallBank": {
                       "id": "/items/properties/isSmallBank",
                       "type": ["boolean","null"]
                   },
                   "isWarehouseRefrigerated": {
                       "id": "/items/properties/isWarehouseRefrigerated",
                       "type": ["boolean","null"]
                   },
                   "lengthRefrFoodDisplayCases": {
                       "id": "/items/properties/lengthRefrFoodDisplayCases",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "licensedBedCapacity": {
                       "id": "/items/properties/licensedBedCapacity",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "maxNumResidents": {
                       "id": "/items/properties/maxNumResidents",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numBedrooms": {
                       "id": "/items/properties/numBedrooms",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numCashRegisters": {
                       "id": "/items/properties/numCashRegisters",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numCommWashingMachines": {
                       "id": "/items/properties/numCommWashingMachines",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numComputers": {
                       "id": "/items/properties/numComputers",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numElectronicLifts": {
                       "id": "/items/properties/numElectronicLifts",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numFTEWorkers": {
                       "id": "/items/properties/numFTEWorkers",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numMRIMachines": {
                       "id": "/items/properties/numMRIMachines",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numOpenClosedRefrCases": {
                       "id": "/items/properties/numOpenClosedRefrCases",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numRefrUnits": {
                       "id": "/items/properties/numRefrUnits",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numRezUnits": {
                       "id": "/items/properties/numRezUnits",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numRezWashingMachines": {
                       "id": "/items/properties/numRezWashingMachines",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numServers": {
                       "id": "/items/properties/numServers",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numStaffedBeds": {
                       "id": "/items/properties/numStaffedBeds",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numUnitsLowRise1to4": {
                       "id": "/items/properties/numUnitsLowRise1to4",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numWalkinRefrUnits": {
                       "id": "/items/properties/numWalkinRefrUnits",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "numWorkersMainShift": {
                       "id": "/items/properties/numWorkersMainShift",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "percentBetterThanMedian": {
                       "id": "/items/properties/percentBetterThanMedian",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "percentCooled": {
                       "id": "/items/properties/percentCooled",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "percentHeated": {
                       "id": "/items/properties/percentHeated",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "postalCode": {
                       "id": "/items/properties/postalCode",
                       "type": "string"
                   },
                   "energies": {
                       "id": "/items/properties/energies",
                       "items": {
                           "id": "/items/properties/energies/items",
                           "properties": {
                               "energyName": {
                                   "id": "/items/properties/energies/items/properties/energyName",
                                   "type": "string",
                                   "enum": ["Electric (Grid)","Electric (renewable)","Natural Gas","Fuel Oil 1","Fuel Oil 2","Fuel Oil 4","Fuel Oil 5,6","Propane","Kerosene","District Steam",
                                   "District Hot Water","District Chilled Water (Absorption)","District Chilled Water (Electric)","District Chilled Water (Engine)",
                                   "District Chilled Water (Other)","Wood","Coke","Coal (Anthracite)","Coal (Bituminous)","Diesel","Other","On-Site Solar","On-Site Wind","On-Site Other","Electric (renewable)","Sold"]
                               },
                               "energyRate": {
                                   "id": "/items/properties/energies/items/properties/energyRate",
                                   "minimum": 0,
                                   "type": ["number","null"]
                               },
                               "energyType": {
                                   "id": "/items/properties/energies/items/properties/energyType",
                                   "type": "string",
                                   "enum": ["grid","onSiteElectricity","naturalGas","fuelOil1","fuelOil2","fuelOil4","fuelOil6", "propane","kerosene","steam","hotWater",
                                   "chilledWater","wood","coke","coalA","coalB","diesel", "other"]
                               },
                               "energyUnits": {
                                   "id": "/items/properties/energies/items/properties/energyUnits",
                                   "type": "string",
                                   "enum": ["kBtu","MBtu","kWh","MWh","GJ","NG Mcf","NG kcf","NG ccf","NG cf", "NGm3","therms","No1 igal","No1 gal",
                                   "No1 L","No2 igal","No2 gal","No2 L","No4 igal","No4 gal","No4 L","No6 igal","No6 gal","No6 L","Diesel igal","Diesel gal",
                                   "Diesel L","Kerosene igal","Kerosene gal","Kerosene L","Propane igal","Propane gal","Propane cf","Propane ccf",
                                   "Propane kcf","Propane L","Steam lb","Steam klb","Steam Mlb","CHW TonH","CoalA ton","CoalA tonne","CoalA lb",
                                   "CoalA klb","CoalA Mlb","CoalBit ton","CoalBit tonne","CoalBit lb","CoalBit klb","CoalBit Mlb","Coke ton","Coke tonne",
                                   "Coke lb","Coke klb","Coke Mlb","Wood ton","Wood tonne"]
                               },
                               "energyUse": {
                                   "id": "/items/properties/energies/items/properties/energyUse",
                                   "minimum": 0,
                                   "type": "number"
                               }
                           },
                           "required": [
                               "energyUnits",
                               "energyUse",
                               "energyType",
                               "energyName"
                           ],
                           "type": ["object"]
                       },
                       "type": ["array","null"]
                   },
                   "renewableEnergies": {
                       "id": "/items/properties/renewableEnergies",
                       "items": {
                           "id": "/items/properties/renewableEnergies/items",
                           "properties": {
                               "energyName": {
                                   "id": "/items/properties/renewableEnergies/items/properties/energyName",
                                   "type": "string",
                                   "enum": ["On-Site Solar","On-Site Wind","On-Site Other","Electric (renewable)","Sold"]
                               },
                               "energyRate": {
                                   "id": "/items/properties/renewableEnergies/items/properties/energyRate",
                                   "minimum": 0,
                                   "type": ["number","null"]
                               },
                               "energyType": {
                                   "id": "/items/properties/renewableEnergies/items/properties/energyType",
                                   "type": "string",
                                   "enum": ["grid"]
                               },
                               "energyUnits": {
                                   "id": "/items/properties/renewableEnergies/items/properties/energyUnits",
                                   "type": "string",
                                   "enum": ["kBtu","MBtu","kWh","MWh","GJ"]
                               },
                               "energyUse": {
                                   "id": "/items/properties/renewableEnergies/items/properties/energyUse",
                                   "minimum": 0,
                                   "type": "number"
                               }
                           },
                           "required": [
                               "energyUnits",
                               "energyUse",
                               "energyType",
                               "energyName"
                           ],
                           "type": "object"
                       },
                       "type": ["array","null"]
                   },
                   "reportingUnits": {
                       "id": "/items/properties/reportingUnits",
                       "type": "string",
                       "enum": ["us","metric"]
                   },
                   "seatingCapacity": {
                       "id": "/items/properties/seatingCapacity",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "state": {
                       "id": "/items/properties/state",
                       "type": "string",
                       "enum": ["AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID", "IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY", "OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY","AB","BC","NB","MB","NL","NS","NT","NU","ON","PE","QC","SK","YT"]
                   },
                   "studentSeatingCapacity": {
                       "id": "/items/properties/studentSeatingCapacity",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "wastewaterAvgInfluentInflow": {
                       "id": "/items/properties/wastewaterAvgInfluentInflow",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "wastewaterEffluentBiologicalOxygenDemand": {
                       "id": "/items/properties/wastewaterEffluentBiologicalOxygenDemand",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "wastewaterHasNutrientRemoval": {
                       "id": "/items/properties/wastewaterHasNutrientRemoval",
                       "type": ["boolean","null"]
                   },
                   "wastewaterHasTrickleFiltration": {
                       "id": "/items/properties/wastewaterHasTrickleFiltration",
                       "type": ["boolean","null"]
                   },
                   "wastewaterInfluentBiologicalOxygenDemand": {
                       "id": "/items/properties/wastewaterInfluentBiologicalOxygenDemand",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "wastewaterLoadFactor": {
                       "id": "/items/properties/wastewaterLoadFactor",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "wastewaterPlantDesignFlowRate": {
                       "id": "/items/properties/wastewaterPlantDesignFlowRate",
                       "minimum": 0,
                       "type": ["number","null"]
                   },
                   "weeklyOperatingHours": {
                       "id": "/items/properties/weeklyOperatingHours",
                       "minimum": 0,
                       "type": ["number","null"]
                   }
               },
               "required": [
                   "buildingType",
                   "postalCode",
                   "state",
                   "country",
                   "GFA",
                   "areaUnits",
                   "reportingUnits"
               ],
               "anyOf": [
                   {
                       "properties": {
                           "buildingType": {
                               "enum": ["AdultEducation","College","PreSchool","VocationalSchool","OtherEducation","ConventionCenter","MovieTheater","Museum","PerformingArts",
                                   "BowlingAlley","FitnessCenter","IceRink","RollerRink","SwimmingPool","OtherRecreation","Stadium","FinancialOffice","DistributionCenter",
                                   "Warehouse","WarehouseRefrigerated","WarehouseUnRefrigerated","SpecialtyHospital","MedicalOffice","OutpatientCenter","PhysicalTherapyCenter","SeniorCare",
                                   "UrgentCareCenter","Barracks","Hotel","MultiFamily","Prison","ResidenceHall","ResidentialLodging", "OtherResidentialLodging","MixedUse","Office","VeterinaryOffice",
                                   "Courthouse","OtherUtility","SelfStorageFacility","StripMall","Retail","PowerStation","EnergyStation","BankBranch","IndoorArena","RaceTrack","Aquarium",
                                   "Bar","Nightclub","Casino","OtherEntertainment","GasStation", "ConvenienceStoreAndGas","ConvenienceStore","FastFoodRestaurant","Restaurant","Supermarket","WholesaleClub",
                                   "FoodSales","FoodService","AmbulatorySurgicalCenter","Hospital","DrinkingWaterTreatment","FireStation","Library","PostOffice","PoliceStation","MeetingHall",
                                   "TransportationTerminal","OtherPublicServices","WorshipCenter","AutoDealership","EnclosedMall","PersonalServices",
                                   "RepairServices","OtherServices","Zoo","K12School","Other","SingleFamilyDetached","SingleFamilyAttached","MobileHome"]
                           }
                       }
                   },
                   {
                       "properties": {
                           "buildingType": {"enum": ["WastewaterCenter"]},
                           "wastewaterAvgInfluentInflow": {
                               "id": "/properties/wastewaterAvgInfluentInflow",
                               "minimum": 0,
                               "type": "number"
                           },
                           "wastewaterInfluentBiologicalOxygenDemand": {
                               "id": "/properties/wastewaterInfluentBiologicalOxygenDemand",
                               "minimum": 0,
                               "type": "number"
                           },
                           "wastewaterEffluentBiologicalOxygenDemand": {
                               "id": "/properties/wastewaterEffluentBiologicalOxygenDemand",
                               "minimum": 0,
                               "type": "number"
                           },
                           "wastewaterPlantDesignFlowRate": {
                               "id": "/properties/wastewaterPlantDesignFlowRate",
                               "minimum": 0,
                               "type": "number"
                           },
                           "wastewaterLoadFactor": {
                               "id": "/properties/wastewaterPlantDesignFlowRate",
                               "minimum": 0,
                               "type": "number"
                           },
                           "wastewaterHasTrickleFiltration": {
                               "id": "/properties/wastewaterHasTrickleFiltration",
                               "type": "boolean"
                           },
                           "wastewaterHasNutrientRemoval": {
                               "id": "/properties/wastewaterHasNutrientRemoval",
                               "type": "boolean"
                           }
                       },
                       "required": ["wastewaterAvgInfluentInflow", "wastewaterInfluentBiologicalOxygenDemand", "wastewaterEffluentBiologicalOxygenDemand",
                       "wastewaterPlantDesignFlowRate", "wastewaterLoadFactor", "wastewaterHasTrickleFiltration","wastewaterHasNutrientRemoval"]
                   },
                   {
                       "properties": {
                           "buildingType": {"enum": ["DataCenter"]},
                           "annualITEnergy": {
                               "id": "/properties/annualITEnergy",
                               "minimum": 0,
                               "type": "number"
                           }
                       },
                       "required": ["annualITEnergy"]
                   }
               ]
           }
       ]
    }""".stripMargin)).get


  def getZEPIMetrics() = Action.async(parse.json) { implicit request =>

    val Baseline: EUIMetrics = EUIMetrics(request.body, configuration)

    val json: JsValue = request.body

    val result = validator.validate(schema, json)

    result.fold(
      invalid = { errors =>
        Future {
          BadRequest(errors.toJson)
        }
      },
      valid = { post =>



        val futures = Future.sequence(Seq(

          Baseline.getPropOutputList.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.percentBetterMedian.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterTarget.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterActual.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterActualwOnSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterActualwOffSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterActualwOnandOffSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },


          Baseline.actualGoalReduction.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.actualGoalBetter.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.zepiActual.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
//          Baseline.zepiActualwOnSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
//          Baseline.zepiActualwOffSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
//          Baseline.zepiActualwOnandOffSite.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.zepiPercentBetter.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.zepiMedian.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.siteEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.siteEUIwOnSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.siteEUIwOffSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.siteEUIwOnandOffSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.sourceEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.sourceEUIwOnSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.sourceEUIwOffSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.sourceEUIwOnandOffSiteConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },


          Baseline.medianSiteEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.medianSourceEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterSiteEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterSourceEUIConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },


          Baseline.getTotalEmissions.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          //this uses default state energy mixes for emissions calcs rather than scaling by source energies per TargetFinder
          //to follow TargetFinder use Baseline.medianTotalEmissions not Baseline.defaultMedianTotalEmissions
          Baseline.defaultMedianTotalEmissions.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          //to follow TargetFinder use Baseline.percentBetterTotalEmissions not Baseline.defaultPercentBetterTotalEmissions
          Baseline.defaultPercentBetterTotalEmissions.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.siteEmissionsIntensityConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.medianSiteEmissionsIntensityConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },


          Baseline.onSiteRenewableTotal.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.offSitePurchasedTotal.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          //this is the total site energy without accounting for renewable generation and/or purchasing
          Baseline.siteEnergyALL.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },


          //these are specific to Maalka platform
          /*      Baseline.getESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
        Baseline.getTargetESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},
        Baseline.getMedianESScore.map(api(_)).recover{ case NonFatal(th) => apiRecover(th)},*/

          Baseline.siteEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.siteEnergyListConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.sourceEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.sourceEnergyListConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.medianSiteEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.medianSourceEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.percentBetterSiteEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.percentBetterSourceEnergyConverted.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.getDirectEmissionList().map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getIndirectEmissionList().map(api(_)).recover { case NonFatal(th) => apiRecover(th) },

          Baseline.getParkingEnergyOnly.map(api(_)).recover { case NonFatal(th) => apiRecover(th) },
          Baseline.getParkingAreaOnly.map(api(_)).recover { case NonFatal(th) => apiRecover(th) }


        ))

        val fieldNames = Seq(

          "propOutputList",

          "percentBetterMedian",
          "percentBetterTarget",
          "percentBetterActual",
          "percentBetterActualwOnSite",
          "percentBetterActualwOffSite",
          "percentBetterActualwOnandOffSite",


          "percentBetterActualtoGoal",
          "actualGoalBetter",

          "actualZEPI",
//          "actualZEPIwOnSite",
//          "actualZEPIwOffSite",
//          "actualZEPIwOnAndOffSite",

          "percentBetterZEPI",
          "medianZEPI",

          "siteEUI",
          "siteEUIwOnSite",
          "siteEUIwOffSite",
          "siteEUIwOnAndOffSite",

          "sourceEUI",
          "sourceEUIwOnSite",
          "sourceEUIwOffSite",
          "sourceEUIwOnAndOffSite",

          "medianSiteEUI",
          "medianSourceEUI",

          "percentBetterSiteEUI",
          "percentBetterSourceEUI",

          "totalEmissions",
          "medianEmissions",
          "percentBetterEmissions",
          "totalEmissionsIntensity",
          "medianEmissionsIntensity",

          "onSiteRenewableTotal",
          "offSitePurchasedTotal",
          "siteEnergyALL",

          /*      //these are specific to Maalka platform
        "actualES",
        "targetES",
        "medianES",*/

          "totalSiteEnergy",
          "siteEnergyList",
          "totalSourceEnergy",
          "sourceEnergyList",

          "medianSiteEnergy",
          "medianSourceEnergy",

          "percentBetterSiteEnergy",
          "percentBetterSourceEnergy",

          "directSiteEmissions",
          "indirectSiteEmissions",

          "parkingEnergy",
          "parkingArea"
        )

        futures.map(fieldNames.zip(_)).map { r =>
          val errors = r.collect {
            case (n, Left(s)) => Json.obj(n -> s)
          }
          val results = r.collect {
            case (n, Right(s)) => Json.obj(n -> s)
          }
          Ok(Json.obj(
            "values" -> results,
            "errors" -> errors
          ))
        }
      }
    )
  }
}


