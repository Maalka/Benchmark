package services

import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink}
import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat, QUOTE_NONE, Quoting}
import javax.inject.Inject
import models.CSVlistCompute
import parsers.ParseCSV
import parsers.ParseCSV.NotValidCSVRow
import play.api.Configuration
import play.api.cache.AsyncCacheApi
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

import util.Logging

object BulkCSVService {
  val RUNNING = "RUNNING"
  val FINISHED = "FINISHED"
  val FAILED = "FAILED"

  case class JobNotFoundException(targetFileName: String) extends Exception
  case class JobNotFinishedException(targetFileName: String) extends Exception
  case class JobFailedException(targetFileName: String, exception: String) extends Exception
}

class BulkCSVService @Inject ()(
                                 configuration: Configuration,
                                 storageModule: StorageService,
                                 asyncCacheApi: AsyncCacheApi,
                                 parseCSV: ParseCSV
                               )(implicit ec: ExecutionContext,
                                 val actorSystem: ActorSystem) extends Logging {

  import BulkCSVService._




  implicit object MyFormat extends DefaultCSVFormat {
    override val quoting: Quoting = QUOTE_NONE
  }

  def getProcessedCSVFile(targetFileName: String, data: Boolean) = {
    logging.info(s"Looking up key: $targetFileName")
    asyncCacheApi.get[String](targetFileName).map { status =>
      logging.info(s"Status from cache: $status")
      status
    }.map {
      case Some(status) if status == RUNNING => throw JobNotFinishedException(targetFileName)
      case Some(status) if status == FINISHED => status
      case Some(status) if status.startsWith(FAILED) => throw JobFailedException(targetFileName, status)
      case Some(status) =>
        throw JobNotFoundException(targetFileName)
      case None => throw JobNotFoundException(targetFileName)
    }
      .map { status =>
        if (!data) {
          None
        } else {
          Some(storageModule.getFile("2030", targetFileName))
        }
      }
  }


  def processCSVFile(csvFile: File, targetFileName: String) {

    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val tempDir = Files.createTempDirectory("Results")
    val processedEntries = new File(tempDir + File.separator + "Results.csv")
    val writer = CSVWriter.open(processedEntries)

    val unprocessedEntries = new File(tempDir + File.separator + "Errors.csv")
    val error_writer = CSVWriter.open(unprocessedEntries)

    writer.writeRow(Seq("Building ID", "Baseline Score", "Baseline Site EUI (kBtu/ft2/yr)", "Baseline Source EUI (kBtu/ft2/yr)"))



    val fileStream1 = new FileInputStream(csvFile)

    val csvTemp = CSVlistCompute(configuration)

    asyncCacheApi
      .set(targetFileName, RUNNING)
      .flatMap { _ =>
        logging.debug("Set Cache and now starting stream")
        parseCSV.toPortfolioFlow(fileStream1)
          .mapAsync(4) {
            case Right(js) => {
              futureToFutureTry[JsValue](
                csvTemp.getMetrics(js)
              )
            }
            case ex => Future(ex)
          }
          .via(CSVWriterFlow(error_writer, writer))
          .runWith(Sink.ignore)
      }
      .map { _ =>
        logging.info("Finished processing csv")
        error_writer.close()
        writer.close()

        val path = Files.createTempFile("2030", "zip")
        val file = path.toFile

        val zip = new ZipOutputStream(Files.newOutputStream(path))

        Seq(processedEntries, unprocessedEntries, csvFile).foreach { f =>
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

        logging.info("Saving processed csv")

        storageModule.putFileFromFile("2030", targetFileName, path.toFile)
      }
      .flatMap { _ =>
        asyncCacheApi.set(targetFileName, FINISHED)
      }
      .recoverWith {
        case NonFatal(th) =>
          logging.error(th, "Failed to process CSV")
          asyncCacheApi.set(targetFileName, s"$FAILED: ${th}")
      }
  }


  private def futureToFutureTry[T](future: Future[T]): Future[Try[T]] = {
    future.map(Try(_)).recover {
      case NonFatal(th) => Failure(th)
    }
  }

  private def CSVWriterFlow(error_writer: CSVWriter, writer: CSVWriter) = Flow[Any].map {
    case scala.util.Success(metrics: JsValue) => {

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
    case Left(metrics: NotValidCSVRow) => error_writer.writeRow(metrics.badEntriesWithErrors)

    case Failure(metrics) =>
      metrics match {
        case NonFatal(th) => {
          error_writer.writeRow(Seq(th.getMessage))
        }
      }
  }
}
