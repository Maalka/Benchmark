package models

import squants.energy.{TBtus, Gigajoules, KBtus, Energy}
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{InputStream}


case class TablePredictedEnergy(params: JsValue) {

  val result = params.as[List[JsValue]]
  getTotalPredictedEnergy.map(println)
  Future.sequence(result.map(expectedSourceEnergy)).map(println)
  Future.sequence(result.map(getBuilding).map(_.map(_.buildingSize))).map(println)

  def getPropWeights: Future[List[Double]] = {
    for {
      propEnergies <- Future.sequence(result.map(expectedSourceEnergy))
      propEnergiesSum <- getTotalPredictedEnergy
      propWeights <- Future(propEnergies.map(_.value/propEnergiesSum))
    } yield propWeights
  }

  def getTotalPredictedEnergy: Future[Double] = {
    for {
      propEnergies <- Future.sequence(result.map(expectedSourceEnergy))
      propEnergiesSum <- Future(propEnergies.map(_.value).sum)
    } yield propEnergiesSum
  }

  def getWeightedTable: Future[Seq[TableEntry]] = {
    val ESList:List[Int] = List.range(1,101,1).sorted(Ordering[Int].reverse)
    val cmPercentList:List[Double] = List.range(0,100,1).map(_/100.0)

    for {
      tableList <- Future.sequence(result.map(lookupTableGet).map(_.map(_.map(_.Ratio))))
      propWeights <- getPropWeights
      weightedTable <- makeWeightedTable(tableList,propWeights)
      zippedTable <- Future((ESList,cmPercentList,weightedTable).zipped.toList)
      formattedTable <- convertTabletoEntries(zippedTable)
      //jsonTable <- Future(convertEntriesToJson(formattedTable))
    } yield formattedTable
  }

  def makeWeightedTable(tableList:List[Seq[Double]],propWeights:List[Double]):Future[List[Double]] = Future {
    tableList.transpose.map(_.zip(propWeights).map{ case (a, b) => a * b }.sum)
  }

  def convertTabletoEntries(table: List[(Int,Double,Double)]): Future[Seq[TableEntry]] = Future{
    table.map { case (a,b,c) =>  TableEntry(a,b,c) }
  }

  def convertEntriesToJson(entries: Seq[TableEntry]): JsValue = Json.toJson(entries)


  def lookupTableGet(parameters: JsValue): Future[Seq[JsonEntry]] = {
    for {
      lookUp <- getLookupTable(parameters)
      futureTable <- loadLookupTable(lookUp)
    } yield futureTable
  }

  def expectedSourceEnergy(parameters:JsValue):Future[Energy] = {
    for {
      targetBuilding <- getBuilding(parameters)
      expectedEnergy <- computeExpectedEUI(targetBuilding,parameters.asOpt[CountryBuildingType])
    } yield expectedEnergy
  }

  def computeExpectedEUI[T](targetBuilding: T,countryBuilding:Option[CountryBuildingType]): Future[Energy] = Future{
    val unitlessEnergy = targetBuilding match {
      case a: ResidenceHall => exp(a.expectedEnergy)
      case a: MedicalOffice => exp(a.expectedEnergy)
      case a: GenericBuilding => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
      case a: BaseLine => a.expectedEnergy * a.buildingSize
    }
    countryBuilding match {
      case Some(CountryBuildingType("USA", _)) => KBtus(unitlessEnergy)
      case Some(CountryBuildingType("Canada", _)) => Gigajoules(unitlessEnergy)
      case _ => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
    }
  }

  def getBuilding(parameters:JsValue): Future[BaseLine] = Future{

    val countryBuilding = parameters.asOpt[CountryBuildingType]

    val building: JsResult[BaseLine] = countryBuilding match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(CountryBuildingType("USA", "WorshipCenter")) => parameters.validate[WorshipCenter]
      case Some(CountryBuildingType("USA", "WastewaterCenter")) => parameters.validate[WastewaterCenter]
      case Some(CountryBuildingType("USA", "Warehouse")) => parameters.validate[Warehouse]
      case Some(CountryBuildingType("USA", "Supermarket")) => parameters.validate[Supermarket]
      case Some(CountryBuildingType("USA", "SeniorCare")) => parameters.validate[SeniorCare]
      case Some(CountryBuildingType("USA", "Retail")) => parameters.validate[Retail]
      case Some(CountryBuildingType("USA", "ResidenceHall")) => parameters.validate[ResidenceHall]
      case Some(CountryBuildingType("USA", "MultiFamily")) => parameters.validate[MultiFamily]
      case Some(CountryBuildingType("USA", "MedicalOffice")) => parameters.validate[MedicalOffice]
      case Some(CountryBuildingType("USA", "K12School")) => parameters.validate[K12School]
      case Some(CountryBuildingType("USA", "Hotel")) => parameters.validate[Hotel]
      case Some(CountryBuildingType("USA", "Hospital")) => parameters.validate[Hospital]
      case Some(CountryBuildingType("USA", "DataCenter")) => parameters.validate[DataCenter]
      case Some(CountryBuildingType("Canada", "Office")) => parameters.validate[CanadaOffice]
      case Some(CountryBuildingType("Canada", "Supermarket")) => parameters.validate[CanadaSupermarket]
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => parameters.validate[CanadaMedicalOffice]
      case Some(CountryBuildingType("Canada", "K12School")) => parameters.validate[CanadaK12School]
      case Some(CountryBuildingType("Canada", "Hospital")) => parameters.validate[CanadaHospital]
      case Some(_) => parameters.validate[GenericBuilding]
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    building match {
      case JsSuccess(a: BaseLine, _) => a
      case JsError(err) => throw new Exception("Building Type parameters fail validation!")
    }
  }

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

  def getLookupTable(parameters:JsValue): Future[String] = {

    val countryBuilding = parameters.asOpt[CountryBuildingType]

    val r = countryBuilding match {
      case Some(CountryBuildingType("USA", "Office")) => Play.current.configuration.getString("baseline.office")
      case Some(CountryBuildingType("USA", "WorshipCenter")) => Play.current.configuration.getString("baseline.worshipCenter")
      case Some(CountryBuildingType("USA", "WastewaterCenter")) => Play.current.configuration.getString("baseline.wastewaterCenter")
      case Some(CountryBuildingType("USA", "Warehouse")) => Play.current.configuration.getString("baseline.warehouse")
      case Some(CountryBuildingType("USA", "Supermarket")) => Play.current.configuration.getString("baseline.supermarket")
      case Some(CountryBuildingType("USA", "SeniorCare")) => Play.current.configuration.getString("baseline.seniorCare")
      case Some(CountryBuildingType("USA", "Retail")) => Play.current.configuration.getString("baseline.retail")
      case Some(CountryBuildingType("USA", "MultiFamily")) => Play.current.configuration.getString("baseline.multiFamily")
      case Some(CountryBuildingType("USA", "ResidenceHall")) => Play.current.configuration.getString("baseline.residenceHall")
      case Some(CountryBuildingType("USA", "MedicalOffice")) => Play.current.configuration.getString("baseline.medicalOffice")
      case Some(CountryBuildingType("USA", "K12School")) => Play.current.configuration.getString("baseline.K12School")
      case Some(CountryBuildingType("USA", "Hotel")) => Play.current.configuration.getString("baseline.hotel")
      case Some(CountryBuildingType("USA", "DataCenter")) => Play.current.configuration.getString("baseline.datacenter")
      case Some(CountryBuildingType("USA", "Hospital")) => Play.current.configuration.getString("baseline.hospital")
      case Some(CountryBuildingType("Canada", "Office")) => Play.current.configuration.getString("baseline.canadaOffice")
      case Some(CountryBuildingType("Canada", "Supermarket")) => Play.current.configuration.getString("baseline.canadaSupermarket")
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => Play.current.configuration.getString("baseline.canadaMedicalOffice")
      case Some(CountryBuildingType("Canada", "K12School")) => Play.current.configuration.getString("baseline.canadaK12School")
      case Some(CountryBuildingType("Canada", "Hospital")) => Play.current.configuration.getString("baseline.canadaHospital")
      case Some(_) => None
      case None => None
    }
    Future(r.getOrElse("Lookup Table Not Found"))
  }
}

case class TableEntry(ES: Int, CmPercent: Double, Ratio: Double)
object TableEntry {
  implicit val tableEntryWrites: Writes[TableEntry] = Json.writes[TableEntry]
  implicit val tableEntryReads: Reads[TableEntry] = Json.reads[TableEntry]
}


