package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import akka.stream.ActorMaterializer
import akka.actor.ActorSystem
import akka.dispatch.Envelope
import akka.stream._
import akka.stream.scaladsl._
import akka.util.Timeout
import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat, QUOTE_NONE, Quoting}
import com.google.inject.Inject
import models._
import models.CSVlistCompute
import play.api.cache.{AsyncCacheApi, CacheApi}
import play.api.libs.EventSource
import play.api.libs.Files.TemporaryFile
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc._
import play.api.{ Configuration, Environment }

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


class CSVController @Inject() (val cache: AsyncCacheApi, cc: ControllerComponents, system:ActorSystem, configuration: Configuration) extends AbstractController(cc) with Logging {

  import scala.concurrent.ExecutionContext.Implicits.global



  val decider: Supervision.Decider = {
    case NonFatal(th) =>


      val sw = new StringWriter
      th.printStackTrace(new PrintWriter(sw))
      //Console.println(sw.toString)
      //Supervision.Resume
      Supervision.Stop

    case _ => Supervision.Stop
  }

  implicit val actorSystem = ActorSystem("ServiceName")
  implicit val materializer = ActorMaterializer()

  implicit val timeout = Timeout(5 seconds)
  //val listCompute = new CSVlistCompute

  val calculateDegreeDays = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    val zipWith = builder.add(ZipWith[Try[Int],Try[Int],JsValue,(Try[Int],Try[Int],JsValue)]((_, _, _)))

    val broadcast = builder.add(Broadcast[(JsValue,DegreeDays)](3))
    broadcast.out(0).mapAsync(1) { v =>
      futureToFutureTry[Int](v._2.lookupCDD)
    } ~> zipWith.in0
    broadcast.out(1).mapAsync(1) { v =>
      futureToFutureTry[Int](v._2.lookupHDD)
    } ~> zipWith.in1
    broadcast.out(2).map(_._1) ~> zipWith.in2
    FlowShape(broadcast.in, zipWith.out)

  })


  def futureToFutureTry[T](future: Future[T]): Future[Try[T]] = {
    future.map(Try(_)).recover {
      case NonFatal(th) => Failure(th)
    }

  }


  def upload = Action.async(parse.multipartFormData) { implicit request =>
    import java.nio.file.Files

    var tempDir = Files.createTempDirectory("Results")
    val processedEntries = new File(tempDir + File.separator + "Results.csv")


    implicit object MyFormat extends DefaultCSVFormat {
      override val quoting: Quoting = QUOTE_NONE
    }


    val writer = CSVWriter.open(processedEntries)


    val unprocessedEntries = new File(tempDir + File.separator + "Errors.csv")
    val error_writer = CSVWriter.open(unprocessedEntries)

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


        val reader = CSVReader.open(uploadedFile)

        val csvTemp = CSVlistCompute(configuration)
        val csvList = CSVcompute(reader.all)


        csvList.outputUnits match {
          case "sq.m" => writer.writeRow(List("Building ID", "Baseline Score", "Baseline Site EUI (kWh/m2/yr)", "Baseline Source EUI (kWh/m2/yr)"))//, "Baseline Site Energy (kWh/m2)", "Baseline Source Energy (kWh/m2)"))
          case "sq.ft" => writer.writeRow(List("Building ID", "Baseline Score", "Baseline Site EUI (kBtu/ft2/yr)", "Baseline Source EUI (kBtu/ft2/yr)"))//, "Baseline Site Energy (kBtu/ft2)", "Baseline Source Energy (kBtu/ft2)"))
          case _ => writer.writeRow(List("Building ID", "Baseline Score", "Baseline Site EUI", "Baseline Source EUI"))//, "Baseline Site Energy", "Baseline Source Energy"))
        }


        val CSVWriterFlow = Flow[(Try[Int], Try[Int], JsValue, Try[JsValue])].map {
          case (hdd, cdd, js, Success(metrics)) => {

            // success write success row
            val metricsList = List(
              metrics \ "values" \\ "buildingName",
              metrics \ "values" \\ "medianZEPI",
              metrics \ "values" \\ "medianSiteEUI",
              metrics \ "values" \\ "medianSourceEUI"
              //metrics \ "values" \\ "medianSiteEnergy",
              //metrics \ "values" \\ "medianSourceEnergy"
            ).flatten
            writer.writeRow(metricsList)
          }

          case (_, _, _, Failure(metrics)) =>
            metrics match {
              case NonFatal(th) => {
                writer.writeRow(List(th.getMessage))
                // println(th.getMessage)
              }
            }
        }


        Source.fromIterator(() => csvList.goodBuildingJsonList.toIterator).map {
          js => (js, DegreeDays(js))
        }.via(calculateDegreeDays).mapAsync(1) {
          case (ccdTry, hddTry, js) if ccdTry.isSuccess && hddTry.isSuccess => {
            futureToFutureTry[JsValue](
              csvTemp.getMetrics(Json.toJson(List(Json.obj("CDD" -> ccdTry.get, "HDD" -> hddTry.get) ++ js.asInstanceOf[JsObject])))
            ).map {
              case Success(metrics) => (ccdTry, hddTry, js, Try(metrics))
              case i => (ccdTry, hddTry, js, i)
            }
          }
          case (ccdTry, hddTry, js) => Future(
            (ccdTry, hddTry, js, Failure(new Throwable("Invalid Postal Code")))
          )

        }.via(CSVWriterFlow).runWith(Sink.ignore).map { r =>
          error_writer.writeAll(csvList.badEntriesWithErrors)
          error_writer.close()
          writer.close()

          import java.io.ByteArrayOutputStream
          val baos = new ByteArrayOutputStream()

          // Create the zip file from the files
          val zip = new ZipOutputStream(new BufferedOutputStream(baos))
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

          // Source the importStream into an Akka Source
          val source = StreamConverters.fromOutputStream(() => baos)
          // Chunk and return the values
          Ok(baos.toByteArray).withHeaders(
          "Content-Type" -> "application/zip",
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

    //al f = new File("out_list.csv")

  }
}

