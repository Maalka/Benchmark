package controllers

import com.google.inject.Inject
import play.api.cache.{AsyncCacheApi, SyncCacheApi}
import play.api.libs.json._
import play.api.mvc._
import models.User

/** Example controller; see conf/routes for the the mapping to routes */
class Users @Inject() (val cache: AsyncCacheApi) extends Controller with Security {

  /** Retrieves a logged in user if the authentication token is valid.
    *
    * If the token is invalid, [[HasToken]] does not invoke this function.
    *
    * @return The user in JSON format.
    */
//  def authUser() = HasToken(playBodyParsers.empty) { token => userId => implicit request =>
//    Ok(Json.toJson(User.findOneById(userId)))
//  }
//
//  /** Retrieves the user for the given id as JSON */
//  def user(id: Long) = HasToken(playBodyParsers.empty) { token => userId => implicit request =>
//    Ok(Json.toJson(User.findOneById(id)))
//  }
//
//  /** Creates a user from the given JSON */
//  def createUser() = HasToken(playBodyParsers.json) { token => userId => implicit request =>
//    // TODO Implement User creation, typically via request.body.validate[User]
//    NotImplemented
//  }
//
//  /** Updates the user for the given id from the JSON body */
//  def updateUser(id: Long) = HasToken(playBodyParsers.json) { token => userId => implicit request =>
//    // TODO Implement User creation, typically via request.body.validate[User]
//    NotImplemented
//  }
//
//  /** Deletes a user for the given id */
//  def deleteUser(id: Long) = HasToken(playBodyParsers.empty) { token => userId => implicit request =>
//    // TODO Implement User creation, typically via request.body.validate[User]
//    NotImplemented
//  }

}
