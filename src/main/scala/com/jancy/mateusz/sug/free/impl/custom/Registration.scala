package com.jancy.mateusz.sug.free.impl.custom

import com.jancy.mateusz.sug.free.model.{Meta, User}

object Registration extends LangMonad {
  val DEFAULT_SERVICES = List("Mail", "Calendar")
  val SYS_MAIL = "sys@mail.com"
  val HELLO_SUBJEST = "Hello"

  def generateMessage(user: User, meta: Meta): String = s"Hello ${user.firstName}, Services are: ${meta.services.mkString(", ")}....."

  def generateMail(user: User): List[String] = List(s"${user.login}@mail.com")

  def createAccount(firstName: String, lastName: String) = for {
    user <- createUser(firstName, lastName)
    meta <- createMeta(user.login, DEFAULT_SERVICES, generateMail(user))
    _ <- sendMail(SYS_MAIL, meta.mails, "Hello", generateMessage(user, meta))
  } yield user

  def removeAccount(login: String) = for {
    _ <- removeUser(login)
    _ <- removeMeta(login)
  } yield ()

  def chaneLastName(login: String, lastName: String) = for {
    user <- getUser(login)
    _ <- removeAccount(login)
    newUser <- createAccount(user.firstName, lastName)
  } yield newUser
}
