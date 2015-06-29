package models

import org.joda.time.DateTime

case class User(
  id: Option[Long],
  email: String,
  password: String,
  name: String,
  dateOfBirth: Option[DateTime],
  createdAt: DateTime = DateTime.now()
)

object User {

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit val UserFromJson: Reads[User] = (
    (__ \ "id").readNullable[Long] ~
    (__ \ "email").read(Reads.email) ~
    (__ \ "password").read[String] ~
    (__ \ "name").read[String] ~
    (__ \ "dateOfBirth").readNullable[DateTime] ~
    (__ \ "createdAt").read(DateTime.now())
  )(User.apply _)

  implicit val UserToJson: Writes[User] = (
    (__ \ "id").writeNullable[Long] ~
    (__ \ "email").write[String] ~
    (__ \ "password").writeNullable[String] ~ // make nullable so password can be omitted
    (__ \ "name").write[String] ~
    (__ \ "dateOfBirth").writeNullable[DateTime] ~
    (__ \ "createdAt").write[DateTime]
  )((user: User) => (
    user.id,
    user.email,
    None, // here we skip the password
    user.name,
    user.dateOfBirth,
    user.createdAt
  ))


  def findOneById(id: Long): Option[User] = {
    // TODO: find the corresponding user
    //
    // For now return a fake user
    if (id == 3) {
      Some(User(Some(3L), "test@test.com", "mypassword", "John Smith", None))
    } else {
      None
    }
  }

  def findByEmailAndPassword(email: String, password: String): Option[User] = {
    // TODO: find the corresponding user; don't forget to encrypt the password
    //
    // For now return a fake user
    Some(User(Some(3L), email, password, "John Smith", None))
  }

}
