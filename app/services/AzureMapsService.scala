package services

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.LazyLogging
import play.api.Configuration
import play.api.cache.AsyncCacheApi
import play.api.libs.json.Json
import scala.util.control.NonFatal

import java.time.{LocalDate, YearMonth}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AzureMapsService @Inject ()(
                                   configuration: Configuration,
                                   asyncCacheApi: AsyncCacheApi,
                                 )(implicit ec: ExecutionContext, actorSystem: ActorSystem) extends LazyLogging {

  def fetchLatLongFromZip(zipcode: String, country: String): Future[Option[(Double, Double)]] = {
    asyncCacheApi.getOrElseUpdate[Option[(Double, Double)]](zipcode)(fetchFromAzureLatLongFromZip(zipcode, country))
  }

  private def fetchFromAzureLatLongFromZip(zipcode: String, country: String): Future[Option[(Double, Double)]] = {


    val azureMapsSubscriptionKey = configuration.get[String]("azure.key")
    val apiUrl = s"https://atlas.microsoft.com/search/address/structured/json?api-version=1.0&countryCode=${country}&postalCode=${zipcode}&subscription-key=${azureMapsSubscriptionKey}"

    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = apiUrl
    )

    Http().singleRequest(request).flatMap { response =>
      response.entity.dataBytes.runFold("")(_ ++ _.utf8String).map { body =>
        val json = Json.parse(body)
        val totalResults = (json \ "summary" \ "totalResults").as[Int]
        if (totalResults == 0) {
          None
        } else {
          val position = (json \ "results").head \ "position"
          val latitude = (position \ "lat").as[Double]
          val longitude = (position \ "lon").as[Double]
          Some((latitude, longitude))
        }
      }
    }
  }

  def fetchMonthlyHDDAndCDD(latLong: (Double, Double), startDate: LocalDate, endDate: LocalDate): Future[Option[(Double, Double)]] = {
    implicit val materializer = ActorMaterializer()
    val latLongString = s"${latLong._1},${latLong._2}"
    val azureMapsSubscriptionKey = configuration.get[String]("azure.key")
    val apiUrl = s"https://atlas.microsoft.com/weather/historical/actuals/daily/json?unit=imperial&startDate=${startDate}&endDate=${endDate}&api-version=1.1&query=${latLongString}&subscription-key=$azureMapsSubscriptionKey"
    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = apiUrl
    )

    Http().singleRequest(request).flatMap { response =>
      response.entity.dataBytes.runFold("")(_ ++ _.utf8String).map { body =>
        val json = Json.parse(body)
        val hdd = (json \\ "heating").map(_ \ "value").map(_.as[Double]).foldLeft(0.0)(_ + _)
        val cdd = (json \\ "cooling").map(_ \ "value").map(_.as[Double]).foldLeft(0.0)(_ + _)
        Option((hdd, cdd))
      }
    }
  }

  def getMonthStartAndEndDates(year: Int): List[(LocalDate, LocalDate)] =
    (1 to 12).map { month =>
      val yearMonth = YearMonth.of(year, month)
      (yearMonth.atDay(1), yearMonth.atEndOfMonth())
    }.toList

  def fetchHDDAndCDD(maybeLatLong: Option[(Double, Double)]): Future[Option[(Double, Double)]] = {
    maybeLatLong match {
      case None =>
        Future.successful(None)
      case Some(latLong) =>
        val monthDates = getMonthStartAndEndDates(2023)

        Future
          .sequence(monthDates.map { case (start, end) =>
            fetchMonthlyHDDAndCDD(latLong, start, end)
          })
          .map { monthlyData =>
            val combined = monthlyData.flatten
            val totalHDD = combined.map(_._1).sum
            val totalCDD = combined.map(_._2).sum
            Some((totalHDD, totalCDD))
          }
          .recover {
            case NonFatal(error) =>
              logger.error("Error fetching HDD and CDD", error)
              None
          }
    }
  }

  def fetchHDDAndCDDFromZipcode(zipcode: String): Future[Option[(Double, Double)]] = {
    (fetchLatLongFromZip(zipcode, "CA") zip fetchLatLongFromZip(zipcode, "US"))
      .flatMap { case (latLongCA, latLongUS) =>
        fetchHDDAndCDD(latLongCA orElse latLongUS)
      }
      .recover {
        case NonFatal(e) =>
          logger.info("error", e)
          None
      }
  }
}
