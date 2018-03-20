import javax.inject.Inject

import play.api.http.HttpFilters
import play.api.mvc.{RequestHeader, EssentialAction, EssentialFilter}
import play.api.{Logger, Application, ApplicationLoader, BuiltInComponentsFromContext}
import play.filters.cors.CORSFilter
import play.filters.gzip.GzipFilter
import scala.concurrent.ExecutionContext.Implicits.global

class HTTPRequestLoggingFilter extends EssentialFilter {
  def apply(nextFilter: EssentialAction) = new EssentialAction {
    def apply(requestHeader: RequestHeader) = {

      val startTime = System.currentTimeMillis

      nextFilter(requestHeader).map { result =>

        val endTime = System.currentTimeMillis
        val requestTime = endTime - startTime

        Logger.info(s"${requestHeader.method} ${requestHeader.uri}" +
          s" took ${requestTime}ms and returned ${result.header.status}")

        result.withHeaders(
          "Request-Time" -> requestTime.toString,
          "Access-Control-Allow-Origin" -> "*",
          "Access-Control-Allow-Headers" -> "Content-Type"


        )
      }
    }
  }
}

class Filters @Inject() (corsFilter: CORSFilter, gzipFilter: GzipFilter) extends HttpFilters {
  def filters = Seq(corsFilter, gzipFilter, new HTTPRequestLoggingFilter())
}
