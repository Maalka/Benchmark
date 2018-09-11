package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import akka.stream.ActorMaterializer
import akka.actor.ActorSystem
import akka.actor.Status.Success
import akka.stream._
import akka.stream.scaladsl._
import akka.util.{ByteString, Timeout}
import com.github.tototoshi.csv._
import com.google.inject.Inject
import models._
import parsers.ParseCSV
import parsers.ParseCSV.NotValidCSVRow
import play.api.cache.{AsyncCacheApi, CacheApi}
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Environment}
import squants.energy.Energy

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.language.postfixOps
import scala.util.{Failure, Try}


class CSVController @Inject() (val cache: AsyncCacheApi, cc: ControllerComponents, configuration: Configuration) extends AbstractController(cc) with Logging {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val actorSystem = ActorSystem("ServiceName")
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(10 seconds)


  def futureToFutureTry[T](future: Future[T]): Future[Try[T]] = {
    future.map(Try(_)).recover {
      case NonFatal(th) => Failure(th)
    }
  }

  def upload = Action.async(parse.multipartFormData) { implicit request =>
    import java.nio.file.Files

    implicit object MyFormat extends DefaultCSVFormat {
      override val quoting: Quoting = QUOTE_NONE
    }

    val tempDir = Files.createTempDirectory("Results")
    val processedEntries = new File(tempDir + File.separator + "Results.csv")
    val writer = CSVWriter.open(processedEntries)

    val unprocessedEntries = new File(tempDir + File.separator + "Errors.csv")
    val error_writer = CSVWriter.open(unprocessedEntries)

    writer.writeRow(Seq("Building ID", "Baseline Score", "Baseline Site EUI (kBtu/ft2/yr)", "Baseline Source EUI (kBtu/ft2/yr)"))

    request.body.file("attachment").map {
      case upload if upload.filename.takeRight(3) != "csv" =>
        Future(BadRequest(
          Json.obj(
            "response" -> "Selected file is not a CSV",
            "status" -> "KO"
          )
        ))
      case upload => {
        val filename = upload.filename
        val uploadedFile = new File(tempDir + File.separator + filename)
        upload.ref.moveTo(uploadedFile)

        val CSVWriterFlow = Flow[Any].map {
          case scala.util.Success(metrics:JsValue) => {

            // success write success row
            val metricsList = Seq(
              metrics \ "values" \\ "buildingName",
              metrics \ "values" \\ "medianZEPI",
              metrics \ "values" \\ "medianSiteEUI",
              metrics \ "values" \\ "medianSourceEUI"
            ).flatten
            println(metricsList)
            writer.writeRow(metricsList)
          }
          case Left(metrics:NotValidCSVRow) => error_writer.writeRow(metrics.badEntriesWithErrors)

          case Failure(metrics) =>
            metrics match {
              case NonFatal(th) => {
                error_writer.writeRow(Seq(th.getMessage))
              }
            }
        }

        val parseCSV: ParseCSV = new ParseCSV

        val fileStream1 = new FileInputStream(uploadedFile)

        val csvTemp = CSVlistCompute(configuration)

        parseCSV.toPortfolioFlow(fileStream1)
          .mapAsync(4) {
            case Right(js) => {
              futureToFutureTry[JsValue](
                csvTemp.getMetrics(js)
              )
            }
            case ex => Future(ex)
          }
          .via(CSVWriterFlow).runWith(Sink.ignore).map { _ =>
          error_writer.close()
          writer.close()


          val path = Files.createTempFile("2030", "zip")
          val file = path.toFile

          val zip = new ZipOutputStream(Files.newOutputStream(path))

          Seq(processedEntries, unprocessedEntries, uploadedFile).foreach { f =>
            zip.putNextEntry(new ZipEntry("Results/%s".format(f.getName)))
            val in = new BufferedInputStream(new FileInputStream(f))
            var b = in.read()
            while (b > -1) {
              zip.write(b)
              b = in.read
            }
            in.close()
            zip.closeEntry()
          }

          zip.close()

          Ok.sendFile(path.toFile)
            .as("application/zip")
            .withHeaders(
              "Content-Disposition" -> "attachment; filename=Results.zip"
            )
        }.recover {
          case NonFatal(th) =>
            //println(th)
            BadRequest(
              Json.obj(
                "response" -> "Failed to parse CSV",
                "status" -> "KO"
              )
            )
        }
      }
    }.getOrElse {
      Future {
        Ok("File is missing")
      }
    }
  }
}
