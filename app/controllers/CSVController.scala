package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import java.io._
import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.{ByteString, Timeout}

import com.google.inject.Inject

import play.api.cache.AsyncCacheApi
import play.api.libs.json._
  import play.api.mvc._
import play.api.Configuration
import services.{BulkCSVService, StorageService}
import _root_.util.Logging

import scala.language.implicitConversions
import scala.language.postfixOps


class CSVController @Inject() (val cache: AsyncCacheApi,
                               cacheApi: AsyncCacheApi,
                               cc: ControllerComponents,
                               bulkCSVService: BulkCSVService,
                               storageModule: StorageService,
                               configuration: Configuration)(implicit val actorSystem: ActorSystem) extends AbstractController(cc) with Logging {

  import scala.concurrent.ExecutionContext.Implicits.global

  def upload = Action(parse.multipartFormData) { implicit request =>
    import java.nio.file.Files

    val targetFileName = s"2030-${java.util.UUID.randomUUID().toString()}.zip"

    val tempDir = Files.createTempDirectory("Results")

    val reportingUnits:String = request.body.dataParts("reportingUnits") match {
      case Seq(reportingUnits) => reportingUnits
      case _ => "us"
    }
    request.body.file("attachment") match {
      case Some(upload) if upload.filename.takeRight(3) != "csv" =>
        BadRequest(
          Json.obj(
            "response" -> "Selected file is not a CSV",
            "status" -> "KO"
          )
        )
      case Some(upload) => {
        val filename = upload.filename
        val uploadedFile = new File(tempDir + File.separator + filename)
        upload.ref.moveTo(uploadedFile)
        bulkCSVService.processCSVFile(uploadedFile, targetFileName, reportingUnits)
        Ok(Json.obj("targetFileName" -> targetFileName))
      }
      case None =>
        BadRequest("File is missing")
    }
  }

  def getProcessedCSV(targetFileName: String, data: Boolean) = Action.async { implicit request =>
    bulkCSVService.getProcessedCSVFile(targetFileName, data)
      .map {
        case Some(inputStream) =>
          val dataContent: Source[ByteString, _] = StreamConverters.fromInputStream(() => inputStream)
          Ok.chunked(dataContent)
            .as("application/zip")
            .withHeaders(
              "Content-Disposition" -> "attachment; filename=Results.zip"
            )
        case None =>
          Ok(Json.obj("targetFileNamePath" -> routes.CSVController.getProcessedCSV(targetFileName, true).url))
      }.recover {
        case th: BulkCSVService.JobFailedException =>
          ExpectationFailed(s"Processed CSV by the name '$targetFileName' failed.  See Logs")
        case th: BulkCSVService.JobNotFinishedException =>
          Conflict //BadRequest(s"Processed CSV by the name '$targetFileName' was not found"")
        case th: BulkCSVService.JobNotFoundException =>
          NotFound//(s"Processed CSV by the name '$targetFileName' was not found"")
        case _ => InternalServerError("Failed: ")
      }
  }
}
