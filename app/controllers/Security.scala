package controllers

import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._

/**
 * Security actions that should be used by all controllers that need to protect their actions.
 * Can be composed to fine-tune access control.
 */
trait Security { self: Controller =>

  val cache: CacheApi

  val AuthTokenHeader = "X-XSRF-TOKEN"
  val AuthTokenCookieKey = "XSRF-TOKEN"
  val AuthTokenUrlKey = "auth"

  /**
    * Checks that the token is:
    * - present in the cookie header of the request,
    * - either in the header or in the query string,
    * - matches a token already stored in the play cache
    */
  def HasToken[A](p: BodyParser[A] = parse.anyContent)(
    f: String => Long => Request[A] => Result): Action[A] =
    Action(p) { implicit request =>
      request.cookies.get("XSRF-TOKEN").fold {
        Unauthorized(Json.obj("message" -> "Invalid XSRF Token cookie"))
      } { xsrfTokenCookie =>
        val maybeToken = request.headers.get(AuthTokenHeader).orElse(request.getQueryString(AuthTokenUrlKey))
        maybeToken flatMap { token =>
          cache.get[Long](token) map { userId =>
            if (xsrfTokenCookie.value == token) {
              f(token)(userId)(request)
            } else {
              Unauthorized(Json.obj("message" -> "Invalid Token"))
            }
          }
        } getOrElse Unauthorized(Json.obj("message" -> "No Token"))
      }
    }
}
