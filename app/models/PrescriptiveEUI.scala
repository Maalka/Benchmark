package models

import java.io.InputStream

import play.{Environment, api}
import play.api.{Environment, Play}
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import squants.space.{Area, SquareFeet, SquareMeters}



case class PrescriptiveValues(parameters:JsValue) {

  def lookupPrescriptiveElectricityWeighted: Future[ElectricityDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      validatedPropList <- getValidatedPropList
      areaWeights <- getAreaWeights(validatedPropList)
      elecDistList:List[ElectricityDistribution] <- Future.sequence(validatedPropList.map(lookupPrescriptiveElectricity(_)))
      weightedElecDistList <- getWeightedElecDistList(areaWeights,elecDistList)
    } yield weightedElecDistList
  }


  def lookupPrescriptiveNGWeighted: Future[NaturalGasDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      validatedPropList <- getValidatedPropList
      areaWeights <- getAreaWeights(validatedPropList)
      ngDistList: List[NaturalGasDistribution] <- Future.sequence(validatedPropList.map(lookupPrescriptiveNG(_)))
      weightedNGDistList <- getWeightedNGDistList(areaWeights,ngDistList)
    } yield weightedNGDistList
  }


  def lookupPrescriptiveElectricity(propDesc:ValidatedPropTypes): Future[ElectricityDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ propDesc.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[ElectricityDistribution] match {
              case JsSuccess(b: ElectricityDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (Electric) data!")
          }
        }
    } yield euiDist
  }
  def lookupPrescriptiveNG(propDesc:ValidatedPropTypes): Future[NaturalGasDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ propDesc.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[NaturalGasDistribution] match {
              case JsSuccess(b: NaturalGasDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (NG) data!")
          }
        }
    } yield euiDist
  }



  def getWeightedElecDistList(areaWeights:List[Double], distList:List[ElectricityDistribution]):Future[ElectricityDistribution] = Future {

    val weightedList = (distList, areaWeights).zipped.map {
      case (b: ElectricityDistribution, c: Double) => {
        ElectricityDistribution(
          b.elec_htg * c,
          b.elec_clg * c,
          b.elec_intLgt * c,
          b.elec_extLgt * c,
          b.elec_intEqp * c,
          b.elec_extEqp * c,
          b.elec_fans * c,
          b.elec_pumps * c,
          b.elec_heatRej * c,
          b.elec_humid * c,
          b.elec_heatRec * c,
          b.elec_swh * c,
          b.elec_refrg * c,
          b.elec_gentor * c,
          b.elec_net * c,
          b.site_EUI * c
        )
      }
    }

    val elec_htg = weightedList.map(_.elec_htg).sum
    val elec_clg = weightedList.map(_.elec_clg).sum
    val elec_intLgt = weightedList.map(_.elec_intLgt).sum
    val elec_extLgt = weightedList.map(_.elec_extLgt).sum
    val elec_intEqp = weightedList.map(_.elec_intEqp).sum
    val elec_extEqp = weightedList.map(_.elec_extEqp).sum
    val elec_fans = weightedList.map(_.elec_fans).sum
    val elec_pumps = weightedList.map(_.elec_pumps).sum
    val elec_heatRej = weightedList.map(_.elec_heatRej).sum
    val elec_humid = weightedList.map(_.elec_humid).sum
    val elec_heatRec = weightedList.map(_.elec_heatRec).sum
    val elec_swh = weightedList.map(_.elec_swh).sum
    val elec_refrg = weightedList.map(_.elec_refrg).sum
    val elec_gentor = weightedList.map(_.elec_gentor).sum
    val elec_net = weightedList.map(_.elec_net).sum
    val site_EUI = weightedList.map(_.site_EUI).sum

    ElectricityDistribution(elec_htg, elec_clg,elec_intLgt, elec_extLgt, elec_intEqp, elec_extEqp, elec_fans, elec_pumps, elec_heatRej, elec_humid, elec_heatRec, elec_swh, elec_refrg, elec_gentor, elec_net, site_EUI)

    }


  def getWeightedNGDistList(areaWeights:List[Double], distList:List[NaturalGasDistribution]):Future[NaturalGasDistribution] = Future {

    val weightedList = (distList, areaWeights).zipped.map {
      case (b: NaturalGasDistribution, c: Double) => {
        NaturalGasDistribution(
          b.ng_htg * c,
          b.ng_clg * c,
          b.ng_intLgt * c,
          b.ng_extLgt * c,
          b.ng_intEqp * c,
          b.ng_extEqp * c,
          b.ng_fans * c,
          b.ng_pumps * c,
          b.ng_heatRej * c,
          b.ng_humid * c,
          b.ng_heatRec * c,
          b.ng_swh * c,
          b.ng_refrg * c,
          b.ng_gentor * c,
          b.ng_net * c,
          b.site_EUI * c
        )
      }
    }

    val ng_htg = weightedList.map(_.ng_htg).sum
    val ng_clg = weightedList.map(_.ng_clg).sum
    val ng_intLgt = weightedList.map(_.ng_intLgt).sum
    val ng_extLgt = weightedList.map(_.ng_extLgt).sum
    val ng_intEqp = weightedList.map(_.ng_intEqp).sum
    val ng_extEqp = weightedList.map(_.ng_extEqp).sum
    val ng_fans = weightedList.map(_.ng_fans).sum
    val ng_pumps = weightedList.map(_.ng_pumps).sum
    val ng_heatRej = weightedList.map(_.ng_heatRej).sum
    val ng_humid = weightedList.map(_.ng_humid).sum
    val ng_heatRec = weightedList.map(_.ng_heatRec).sum
    val ng_swh = weightedList.map(_.ng_swh).sum
    val ng_refrg = weightedList.map(_.ng_refrg).sum
    val ng_gentor = weightedList.map(_.ng_gentor).sum
    val ng_net = weightedList.map(_.ng_net).sum
    val site_EUI = weightedList.map(_.site_EUI).sum

    NaturalGasDistribution(ng_htg, ng_clg,ng_intLgt, ng_extLgt, ng_intEqp,ng_extEqp, ng_fans, ng_pumps, ng_heatRej, ng_humid, ng_heatRec, ng_swh, ng_refrg, ng_gentor, ng_net, site_EUI)

  }


  def getAreaWeights(validatedPropList: List[ValidatedPropTypes]):Future[List[Double]] = {
    for {
      propSizeList <- Future(validatedPropList.map(_.floor_area))
      propsizeSum:Double <- Future(propSizeList.sum)
      propSizeRatios <- Future(propSizeList.map{_/propsizeSum})
    } yield propSizeRatios
  }

  def chooseLookupTable(validatedPrescriptiveParams: ValidatedPrescriptiveParams): Future[String] =  Future{
    validatedPrescriptiveParams.prescriptive_resource match {
          case 0 => "prescriptive_0.json"
          case _ => throw new Exception("Cannot Identify Appropriate Lookup Table: Check prescriptive_resource value!")
        }
  }

  def loadLookupTable(filename:String): Future[JsValue] = {
    for {
      is <- Future(play.api.Environment.simple().resourceAsStream(filename))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case i => throw new Exception("Prescriptive EUI Lookup - Could not open file: %s".format(i))
        }
      }
    } yield json
  }

  def validatePrescriptiveParams(buildingParams: PrescriptiveParams): Future[ValidatedPrescriptiveParams] = Future {
    val climateZone = buildingParams.climate_zone match {
      case Some(b: String) => b
      case _ => throw new Exception("Could not Identify Climate Zone")
    }
    val prescriptive_resource = buildingParams.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    ValidatedPrescriptiveParams(climateZone, prescriptive_resource)
  }

  def getPrescriptiveParams: Future[ValidatedPrescriptiveParams] = {
    parameters.validate[PrescriptiveParams] match {
      case JsSuccess(a: PrescriptiveParams, _) => validatePrescriptiveParams(a)
      case _ => throw new Exception("Prescriptive Resource or Climate Zone Incorrect!")
    }
  }



  def getValidatedPropParams(propDesc: PropDesc): Future[ValidatedPropTypes] = Future {
    val propType:String = propDesc.building_type match {
      case Some(b: String) => b
      case _ => throw new Exception("Not a Proper Building Type")
    }
    val units:String = propDesc.floor_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case _ => throw new Exception("Floor Area Units must be either ftSQ or mSQ")
    }
    val floorArea:Double = propDesc.floor_area match {
      case Some(a: Double) => {
        units match {
          case "mSQ" => (Area((a, "mSQ")).get to SquareFeet)
          case "ftSQ" => (Area((a, "ftSQ")).get to SquareFeet)
          case _ => throw new Exception("Floor Area Units Must be mSQ or ftSQ! ")
        }
      }
      case _ => throw new Exception("No Floor Area Found! ")
    }
    println(propType, floorArea, "ftSQ")
    ValidatedPropTypes(propType,floorArea,"ftSQ")
  }


  def getValidatedPropList: Future[List[ValidatedPropTypes]] =  {

    for {
      props:PropList <-
        Future {
          parameters.validate[PropList] match {
              case JsSuccess(b: PropList, _) => b
              case JsError(err) => throw new Exception("Problem with submitted Prop List!")
          }
        }
      validatedProps <- {
        props.prop_types.isEmpty match {
          case true => throw new Exception("Prop List is Empty!")
          case _ => Future.sequence(props.prop_types.map(getValidatedPropParams(_)))
        }


      }
    } yield validatedProps
  }
}


case class ElectricityDistribution(elec_htg:Double,elec_clg:Double,elec_intLgt:Double = 0.0,elec_extLgt:Double = 0.0,elec_intEqp:Double = 0.0,
                                   elec_extEqp:Double = 0.0, elec_fans:Double = 0.0,elec_pumps:Double = 0.0,elec_heatRej:Double = 0.0,
                                   elec_humid:Double = 0.0, elec_heatRec:Double = 0.0,elec_swh:Double = 0.0,elec_refrg:Double = 0.0,
                                   elec_gentor:Double = 0.0,elec_net:Double = 0.0, site_EUI:Double = 0.0)

object ElectricityDistribution {
  implicit val ElectricityDistributionReads: Reads[ElectricityDistribution] = Json.reads[ElectricityDistribution]
}
case class NaturalGasDistribution(ng_htg:Double = 0.0,ng_clg:Double = 0.0,ng_intLgt:Double = 0.0,ng_extLgt:Double = 0.0,
                                  ng_intEqp:Double = 0.0,ng_extEqp:Double = 0.0,ng_fans:Double = 0.0,ng_pumps:Double = 0.0,
                                  ng_heatRej:Double = 0.0,ng_humid:Double = 0.0,ng_heatRec:Double = 0.0, ng_swh:Double = 0.0,
                                  ng_refrg:Double = 0.0,ng_gentor:Double = 0.0,ng_net:Double = 0.0,site_EUI:Double = 0.0)

object NaturalGasDistribution {
  implicit val NaturalGasDistributionReads: Reads[NaturalGasDistribution] = Json.reads[NaturalGasDistribution]
}




case class PrescriptiveParams(climate_zone: Option[String], prescriptive_resource: Option[Int])

object PrescriptiveParams {
  implicit val PrescriptiveParamsReads: Reads[PrescriptiveParams] = Json.reads[PrescriptiveParams]
}

case class ValidatedPrescriptiveParams(climate_zone: String, prescriptive_resource:Int)

object ValidatedPrescriptiveParams {
  implicit val ValidatedPrescriptiveParamsReads: Reads[ValidatedPrescriptiveParams] = Json.reads[ValidatedPrescriptiveParams]
}





case class ValidatedPropList(prop_types: List[ValidatedPropTypes])

object ValidatedPropList {
  implicit val ValidatedPropListReads: Reads[ValidatedPropList] = Json.reads[ValidatedPropList]
}

case class ValidatedPropTypes(building_type: String,floor_area: Double, floor_area_units: String)

object ValidatedPropTypes {
  implicit val ValidatedPropTypesReads: Reads[ValidatedPropTypes] = Json.reads[ValidatedPropTypes]
}




case class PropList(prop_types: List[PropDesc])

object PropList {
  implicit val PropListReads: Reads[PropList] = Json.reads[PropList]
}

case class PropDesc(building_type: Option[String],floor_area: Option[Double], floor_area_units: Option[String])

object PropDesc {
  implicit val PropDescReads: Reads[PropDesc] = Json.reads[PropDesc]
}