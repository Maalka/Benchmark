package models


import squants.energy._
import squants.energy.EnergyConversions.EnergyNumeric

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.{Configuration}

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.InputStream

import squants.space.{SquareFeet, SquareMeters}


case class CombinedPropTypes(params: JsValue, configuration: Configuration) {

  val result = params.as[List[JsValue]]
  val buildingProps:BuildingProperties = BuildingProperties(result.head)

  def convertToZeroScale(value:Energy):Energy = {
    if(buildingProps.getTarget2030Value == false) {
      value
    }else {
      value * 100 / 130
    }
  }

  def getWholeBuildingSourceMedianEnergy:Future[Energy] = {
    for {
      wholeBuildingSourceMedianEUI <- getWholeBuildingSourceMedianEUI
      totalArea <- getTotalArea(result)
    } yield wholeBuildingSourceMedianEUI*totalArea
  }

  def getWholeBuildingSourceMedianEUI:Future[Energy] = {
    for {
      wholeBuildingSourceMedianEUI <- getWholeBuildingSourceMedianEUInoParking
      totalArea <- getTotalArea(result)
      targetbuilding <- BuildingProperties(result.head).getBuilding
      heatingDays <- targetbuilding.getHDD
      parkingEnergy <- getParkingEnergy(result.head, heatingDays)
      adjustedEUI <- Future((wholeBuildingSourceMedianEUI*totalArea + parkingEnergy) / totalArea)
    } yield adjustedEUI
  }

    def getWholeBuildingSourceMedianEUInoParking:Future[Energy] = {

    for {
      majorPropType <- majorProp
      hasGenericBuilding <- checkGenericBuilding
      lookUpSourceMedianEUI <- {
        majorPropType.contains(true) match {
          case true => {
            hasGenericBuilding match {
              case true => getGenericSourceEUI
              case false => getMedianSourceEUI(result)
            }
          }
          case false => getDefaultSourceMedianEUI(buildingProps.country)
        }
      }
    } yield convertToZeroScale(lookUpSourceMedianEUI)
  }

  def getMedianSourceEUI(buildingList:List[JsValue]):Future[Energy] = {
    for {
      totalArea <- getTotalArea(buildingList)
      baselineSourceEnergy <- getMedianSourceEnergy(buildingList)
    }yield baselineSourceEnergy/totalArea

  }

  def getMedianSourceEnergy(buildingList:List[JsValue]):Future[Energy] = {
    for {
      computeSourceEnergy <- {
        buildingList.length match {
          case a if a == 1 => singlePropMedianSourceEnergy(buildingList.head)
          case _ => getWeightedMedianSourceEnergy(buildingList)
        }
      }
    }yield computeSourceEnergy
  }

  def getWeightedMedianSourceEnergy(buildingList:List[JsValue]):Future[Energy] = {
    for {
      weightedTable <- getWeightedTable(buildingList)
      medianEnergy <- getMedianRatio(weightedTable)
    }yield {
      buildingProps.country match {
        case "USA" => KBtus(medianEnergy)
        case "Canada" => Gigajoules(medianEnergy)
        case _ => throw new Exception("Cannot compute Weighted Table Energy")
      }
    }
  }

  def getWeightedTable(propList:List[JsValue]): Future[Seq[TableEntry]] = {
    val ESList:List[Int] = List.range(1,101,1).sorted(Ordering[Int].reverse)
    val cmPercentList:List[Double] = List.range(0,100,1).map(_/100.0)
    for {
      buildingList <- Future.sequence(propList.map(BuildingProperties(_).getBuilding))
      ratioToEnergyList <- Future.sequence{
        buildingList.map {
          case a: BaseLine => {
            for {
              lookUp <- lookupTableGet(a)
              seqTable <- seqTableToEnergy(a,lookUp)
            } yield seqTable
          }
        }
      }
      weightedTable <- Future{ratioToEnergyList.transpose.map(_.sum)}
      zippedTable <- Future((ESList,cmPercentList,weightedTable).zipped.toList)
      formattedTable <- convertTabletoEntries(zippedTable)
    } yield formattedTable
  }


  def seqTableToEnergy(targetBuilding: BaseLine, table:Seq[TableEntry]): Future[Seq[Double]] = Future.sequence{
        table.map{b:TableEntry => tableToEnergy(targetBuilding,b)}
  }

  def tableToEnergy(targetBuilding: BaseLine, tableEntry:TableEntry): Future[Double] = {
    for {
      predictedEnergy <- targetBuilding.expectedEnergy
      table <- Future{
        targetBuilding match {
          case a: ResidenceHall => exp(tableEntry.Ratio * predictedEnergy / 15.717)
          case a: MedicalOffice => exp(tableEntry.Ratio * predictedEnergy / 14.919)
          case a: DataCenter => tableEntry.Ratio * predictedEnergy * a.annualITEnergyKBtu
          case a: BaseLine => tableEntry.Ratio * predictedEnergy * a.buildingSize
        }
      }
    } yield table
  }

  def convertTabletoEntries(table: List[(Int,Double,Double)]): Future[Seq[TableEntry]] = Future{
    table.map { case (a,b,c) =>  TableEntry(a,b,c) }
  }


  def getTargetEUI[T](targetBuilding: T,lookupEUI:Energy,targetRatio:Double):Future[Energy] = Future {
    targetBuilding match {
      case a: ResidenceHall => {
        a.country match {
          case "USA" => KBtus(exp(targetRatio / 15.717 * lookupEUI.value) / a.buildingSize)
          case "Canada" => Gigajoules(exp(targetRatio / 15.717 * lookupEUI.value) / a.buildingSize)
        }
      }
      case a: MedicalOffice => {
        a.country match {
          case "USA" => KBtus(exp(targetRatio / 14.919 * lookupEUI.value) / a.buildingSize)
          case "Canada" => Gigajoules(exp(targetRatio / 14.919 * lookupEUI.value) / a.buildingSize)
        }
      }
      case a: DataCenter => targetRatio * lookupEUI * a.annualITEnergyKBtu / a.buildingSize
      case a: GenericBuilding => throw new Exception("Could not calculate Target EUI - Generic Building: No Algorithm!!")
      case a: BaseLine => targetRatio * lookupEUI
    }
  }

/*
  def getGenericSourceEUI:Future[Energy] = {
    for {
      buildingList <- getBuildingList
      majorPropFilter <- majorProp
      mediumPropFilter <- mediumSizeProps(buildingList)
      majorProp <- getMajorProp(majorPropFilter)
      majorPropType <- BuildingProperties(majorProp).getBuilding
      sourceEUI <- {
            majorPropType match {
              case c: GenericBuilding => singlePropMedianSourceEUI(majorPropType)
              case d: BaseLine => getMedianNoGeneric(majorPropType)
            }
          }
    } yield sourceEUI
  }*/

  def getGenericSourceEUI:Future[Energy] = {

      for {
        buildingList <- getBuildingList
        areaWeights <- getAreaWeights
        sourceEUIList <- Future.sequence{
          buildingList.map{
            case a: BaseLine => singlePropMedianSourceEUI(a)
          }
        }
        weightedSourceEUI <- Future{(sourceEUIList,areaWeights).zipped.map {
          case (a:Energy,b:Double) => a * b
        }.sum}
    } yield {
        buildingProps.country match {
          case "USA" => weightedSourceEUI in KBtus
          case "Canada" => weightedSourceEUI in Gigajoules
          case _ => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
        }
      }

  }

  def getMedianNoGeneric(majorPropType:BaseLine):Future[Energy] =  {
    for {
      buildingList <- getBuildingList
      mediumPropFilter <- mediumSizeProps(buildingList)
      mediumPropTypesJsValues <- filterPropTypes(mediumPropFilter)
      mediumPropTypesList <- Future.sequence(mediumPropTypesJsValues.map(BuildingProperties(_).getBuilding))
      mediumGenericTypes <- getMediumGenericPropTypes(mediumPropTypesList)
      nonGenericPropFilter <- Future{buildingList.map{
        case a if a.isInstanceOf[GenericBuilding] == true => false
        case a if a.isInstanceOf[GenericBuilding] == false => true
        }
      }
      nonGenericPropTypes <- filterPropTypes(nonGenericPropFilter)
      sourceEUI <- {
        mediumGenericTypes.isEmpty match {
          case true => getMedianSourceEUI(nonGenericPropTypes)
          case false => Future {
            (buildingProps.country,majorPropType.buildingType) match {
              case ("USA", "Office") => KBtus(148.1)
              case ("USA", "FinancialOffice") => KBtus(148.1)
              case ("USA", "WorshipCenter") => KBtus(70.7)
              case ("USA", "WastewaterCenter") => KBtus(148.1)
              case ("USA", "Warehouse") => KBtus(60.0)
              case ("USA", "WarehouseUnRefrigerated") => KBtus(60.0)
              case ("USA", "WarehouseRefrigerated") => KBtus(252.6)
              case ("USA", "Supermarket") => KBtus(480.0)
              case ("USA", "SeniorCare") => KBtus(243.2)
              case ("USA", "Retail") => KBtus(114.4)
              case ("USA", "MultiFamily") => KBtus(127.9)
              case ("USA", "ResidenceHall") => KBtus(114.9)
              case ("USA", "MedicalOffice") => KBtus(116.7)
              case ("USA", "K12School") => KBtus(141.1)
              case ("USA", "Hotel") => KBtus(162.1)
              case ("USA", "DataCenter") => KBtus(148.1)
              case ("USA", "Hospital") => KBtus(389.8)
              case ("USA",_) => KBtus(148.1)

              case ("Canada", "Office") => Gigajoules(1.31)
              case ("Canada", "Supermarket") => Gigajoules(1.44)
              case ("Canada", "MedicalOffice") => Gigajoules(1.46)
              case ("Canada", "K12School") => Gigajoules(1.03)
              case ("Canada", "Hospital") => Gigajoules(3.12)
              case ("Canada",_) => Gigajoules(1.68)

              case (_,_) => throw new Exception("Lookup Table Not Found")
              case _ => throw new Exception("Error in getMedianNoGeneric")
            }
          }
        }
      }
    } yield sourceEUI
  }


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


  def getMediumGenericPropTypes(propList:List[BaseLine]):Future[List[BaseLine ]] = Future{
    propList.flatMap{
      case a if a.isInstanceOf[GenericBuilding] => Some(a)
      case _ => None
    }
  }

  def singlePropMedianSourceEUI(targetBuilding:BaseLine):Future[Energy] = {
    for {
      medianEUI <- {
        targetBuilding match {
          case a:GenericBuilding => sourceMedianEUI(targetBuilding)
          case b:BaseLine => {
            for {
              lookupEUI <- computeLookupEUI(targetBuilding)
              lookupTable <- lookupTableGet(targetBuilding)
              targetRatio <- getMedianRatio(lookupTable)
              targetEUI <- getTargetEUI(targetBuilding, lookupEUI, targetRatio)
            } yield targetEUI
          }
        }
      }
    } yield medianEUI
  }

  def singlePropMedianSourceEnergy(targetBuilding:JsValue):Future[Energy] = {
    for {
      targetBuilding <- BuildingProperties(targetBuilding).getBuilding
      totalArea <- Future(targetBuilding.buildingSize)
      medianEUI <- singlePropMedianSourceEUI(targetBuilding)
      } yield medianEUI * totalArea
  }

  def sumEnergy(energies:List[Energy]):Future[Energy] = Future{
    buildingProps.country match {
      case "USA" => energies.sum in KBtus
      case "Canada" => energies.sum in Gigajoules
    }
  }

  def getMedianRatio(lookUp:Seq[TableEntry]):Future[Double] = {
    for {
      targetRatioEntry <- Future(lookUp.filter(_.ES == 51).last.Ratio)
    } yield targetRatioEntry
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
      buildingTypeSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => (a.propTypeName,a.buildingSize)
      }
      ))
      buildingSizeList <- Future(buildingTypeSizeList.groupBy(_._1).map(_._2.map(_._2).sum))

      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      majorPropType <-  Future(buildingSizeRatios.map{ case a => a > 0.5 }.toList)
    } yield majorPropType
  }

  /*
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
*/


  def getAreaWeights:Future[List[Double]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
    } yield buildingSizeRatios
  }



  def getEnergyWeights(buildingList:List[JsValue]): Future[List[Double]] = {
    for {
      propEnergies <- Future.sequence(buildingList.map(expectedSourceEnergy))
      propEnergiesSum <- getTotalPredictedEnergy(buildingList)
      energyWeights <- Future(propEnergies.map(_.value/propEnergiesSum.value))
    } yield energyWeights
  }

  def getTotalPredictedEnergy(buildingList:List[JsValue]): Future[Energy] = {
    for {
      propEnergies <- Future.sequence(buildingList.map(expectedSourceEnergy))
      propEnergiesSum <- sumEnergy(propEnergies)
    } yield propEnergiesSum
  }

  def getTotalArea(buildingList:List[JsValue]): Future[Double] = {
    for {
      propTypes <- Future.sequence(buildingList.map(BuildingProperties(_).getBuilding))
      propGFASum <- Future(propTypes.map(_.buildingSize).sum)
    } yield propGFASum
  }


  def expectedSourceEnergy(parameters:JsValue):Future[Energy] = {
    for {
      targetBuilding <- BuildingProperties(parameters).getBuilding
      expectedEnergy <- computeExpectedEnergy(targetBuilding,buildingProps.country)
    } yield expectedEnergy
  }


  def computeExpectedEnergy(targetBuilding:BaseLine, country:String): Future[Energy] = {
    for {
      predictedEnergy <- targetBuilding.expectedEnergy
      unitlessEnergy <- Future{
        targetBuilding match {
          case a: ResidenceHall => exp(predictedEnergy)
          case a: MedicalOffice => exp(predictedEnergy)
          case a: DataCenter => predictedEnergy * a.annualITEnergyKBtu
          case a: GenericBuilding => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
          case a: BaseLine => predictedEnergy * a.buildingSize
        }
      }
      returnEnergy <- Future{
        country match {
          case "USA" => KBtus(unitlessEnergy)
          case "Canada" => Gigajoules(unitlessEnergy)
          case _ => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
        }
      }
    } yield returnEnergy
  }


  def getParkingEnergy(parkingJSON:JsValue, heatingDays: Double): Future[Energy] = Future {

    implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

    parkingJSON.asOpt[Parking] match {
      case Some(Parking(open,partial,closed,heated,totalArea,units,country,reportingUnits)) => {
        units match {
          case "ftSQ" => {
            val openArea: Double = open.getOrElse(0.0)
            val partiallyEnclosedParkingArea: Double = partial.getOrElse(0.0)
            val fullyEnclosedParkingArea: Double = closed.getOrElse(0.0)

            country match {
              case "USA" => KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
                (0.009822 * (heatingDays * heated * fullyEnclosedParkingArea)))
              case "Canada" => KBtus((6.128 * openArea) + (18.38 * partiallyEnclosedParkingArea) + (23.28 * fullyEnclosedParkingArea) +
                (0.009451 * (heatingDays * heated * fullyEnclosedParkingArea))) in Gigajoules
            }
          }
          case "mSQ" => {
            val openArea: Double = SquareMeters(open.getOrElse(0.0)) to SquareFeet
            val partiallyEnclosedParkingArea: Double = SquareMeters(partial.getOrElse(0.0)) to SquareFeet
            val fullyEnclosedParkingArea: Double = SquareMeters(closed.getOrElse(0.0)) to SquareFeet

            country match {
              case "USA" => KBtus((9.385 * openArea) + (28.16 * partiallyEnclosedParkingArea) + (35.67 * fullyEnclosedParkingArea) +
                (0.009822 * (heatingDays * heated * fullyEnclosedParkingArea)))
              case "Canada" => KBtus((6.128 * openArea) + (18.38 * partiallyEnclosedParkingArea) + (23.28 * fullyEnclosedParkingArea) +
                (0.009451 * (heatingDays * heated * fullyEnclosedParkingArea))) in Gigajoules
            }
          }
        }
      }
      case Some(_) => KBtus(0)
      case None => KBtus(0)
    }
  }

  def getParkingArea(parkingJSON:JsValue): Future[Double] = Future {

    parkingJSON.asOpt[Parking] match {
      case Some(Parking(open,partial,closed,heated,total,units,country,reportingUnits)) => {
        units match {
          case "ftSQ" => reportingUnits match {
            case "us" => total.getOrElse(0.0)
            case "metric" => SquareFeet(total.getOrElse(0.0)) to SquareMeters
          }
          case "mSQ" => reportingUnits match {
            case "us" => SquareMeters(total.getOrElse(0.0)) to SquareFeet
            case "metric" => total.getOrElse(0.0)
          }
        }
      }
      case _ => 0.0
    }
  }



  case class Parking(openParkingArea:Option[Double],partiallyEnclosedParkingArea:Option[Double],
                     fullyEnclosedParkingArea:Option[Double], hasParkingHeating:Option[Boolean],
                     totalParkingArea:Option[Double],parkingAreaUnits:String,country:String,reportingUnits:String)
  object Parking {
    implicit val parkingRead: Reads[Parking] = Json.reads[Parking]
  }



  def lookupTableGet(building: BaseLine): Future[Seq[TableEntry]] = {
    for {
      lookUp <- getLookupTable(building)
      futureTable <- loadLookupTable(lookUp)
    } yield futureTable
  }

  def loadLookupTable(filename:String): Future[Seq[TableEntry]] = {
    for {
      is <- Future(play.api.Environment.simple().resourceAsStream(filename))
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

  def getLookupTable(building:BaseLine): Future[String] = Future{
    
    val r = (building.country,building.buildingType) match {
      case ("USA", "Office") => configuration.get[Option[String]]("baseline.office")
      case ("USA", "FinancialOffice") => configuration.get[Option[String]]("baseline.office")
      case ("USA", "WorshipCenter") => configuration.get[Option[String]]("baseline.worshipCenter")
      case ("USA", "WastewaterCenter") => configuration.get[Option[String]]("baseline.wastewaterCenter")
      case ("USA", "Warehouse") => configuration.get[Option[String]]("baseline.warehouse")
      case ("USA", "WarehouseUnRefrigerated") => configuration.get[Option[String]]("baseline.warehouse")
      case ("USA", "WarehouseRefrigerated") => configuration.get[Option[String]]("baseline.warehouse")
      case ("USA", "DistributionCenter") => configuration.get[Option[String]]("baseline.warehouse")
      case ("USA", "Supermarket") => configuration.get[Option[String]]("baseline.supermarket")
      case ("USA", "SeniorCare") => configuration.get[Option[String]]("baseline.seniorCare")
      case ("USA", "Retail") => configuration.get[Option[String]]("baseline.retail")
      case ("USA", "MultiFamily") => configuration.get[Option[String]]("baseline.multiFamily")
      case ("USA", "ResidenceHall") => configuration.get[Option[String]]("baseline.residenceHall")
      case ("USA", "MedicalOffice") => configuration.get[Option[String]]("baseline.medicalOffice")
      case ("USA", "K12School") => configuration.get[Option[String]]("baseline.K12School")
      case ("USA", "Hotel") => configuration.get[Option[String]]("baseline.hotel")
      case ("USA", "DataCenter") => configuration.get[Option[String]]("baseline.datacenter")
      case ("USA", "Hospital") => configuration.get[Option[String]]("baseline.hospital")
      case ("Canada", "Office") => configuration.get[Option[String]]("baseline.canadaOffice")
      case ("Canada", "Supermarket") => configuration.get[Option[String]]("baseline.canadaSupermarket")
      case ("Canada", "MedicalOffice") => configuration.get[Option[String]]("baseline.canadaMedicalOffice")
      case ("Canada", "K12School") => configuration.get[Option[String]]("baseline.canadaK12School")
      case ("Canada", "Hospital") => configuration.get[Option[String]]("baseline.canadaHospital")
      case (_,_) => throw new Exception("Lookup Table Not Found")
      case _ => throw new Exception("Error in getLookupTable")
    }
    r.getOrElse("Lookup Table Not Found")
  }


  def computeLookupEUI(targetBuilding: BaseLine): Future[Energy] = {

    for {
      predictedEUI <- {
        targetBuilding match {
          case a: GenericBuilding => throw new Exception("Lookup EUI could not be computed - Generic Building: No Algorithm!")
          case a: BaseLine => a.expectedEnergy
        }
      }
      convertedEUI <- Future{
        targetBuilding.country match {
          case "USA" => KBtus(predictedEUI)
          case "Canada" => Gigajoules(predictedEUI)
        }
      }
    } yield convertedEUI
  }



  def getDefaultSourceMedianEUI(country:String):Future[Energy] = Future{
    country match {
      case "USA" => KBtus(123.1)
      case "Canada" => Gigajoules(1.23)
      case _ => throw new Exception("Could not find Country and Building Type for Median EUI")
    }
  }

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
        case ("Canada","WarehouseUnRefrigerated") => 0.93
        case ("Canada","WarehouseRefrigerated") => 1.23
        case ("Canada","SeniorCare") => 1.88
        case ("Canada","Retail") => 1.52
        case ("Canada","ResidenceHall") => 2.05
        case ("Canada","DataCenter") => 1.82 //this = Total Energy / IT Energy

        case ("Canada",_) => 1.23

        case (_,_) => throw new Exception("Could not find Country and Building Type for Median EUI")
        case _ => throw new Exception("Error in sourceMedianEUI")
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
      case _ => throw new Exception("Could not find State to identify country region")

    }
  }
}

case class TableEntry(ES: Int, CmPercent: Double, Ratio: Double)
object TableEntry {
  implicit val tableEntryWrites: Writes[TableEntry] = Json.writes[TableEntry]
  implicit val tableEntryReads: Reads[TableEntry] = Json.reads[TableEntry]
}


