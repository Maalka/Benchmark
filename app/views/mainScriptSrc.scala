package views.html.helper

import play.api.templates.Html
import play.api.mvc.{Call}
import play.api.{Play, Mode}
import controllers.routes

trait RequiresApp {
  implicit val app = play.api.Play.current
}

/** Resolves the path to your main RequireJS script depending on the current environment */
object mainScriptSrc extends RequiresApp {
  def apply(scriptName: String, devSuffix: String = "Dev", testSuffix: String = "Dev", prodSuffix: String = "Prod"): String = {
    val call: Call = app.mode match {
      case Mode.Dev => routes.Assets.at(s"${scriptName}${devSuffix}")
      case Mode.Test => routes.Assets.at(s"${scriptName}${testSuffix}")
      case Mode.Prod => routes.Assets.at(s"${scriptName}${prodSuffix}")
    }
    call.url
  }
}
