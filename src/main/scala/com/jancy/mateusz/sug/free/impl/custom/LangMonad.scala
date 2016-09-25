package com.jancy.mateusz.sug.free.impl.custom

import com.jancy.mateusz.sug.free.impl.custom.Lang._
import com.jancy.mateusz.sug.free.model.{Meta, User}

import scalaz._

trait LangMonad {

  protected def sendMail(from: String, to: List[String], subject: String, message: String): Free[Lang, Unit] =
    Free.liftF(SendMail(from, to, subject, message, ()))

  protected def createMeta(login: String, services: List[String], mails: List[String]): Free[Lang, Meta] =
    Free.liftF(CreateMeta(login, services, mails, identity))

  protected def getMeta(login: String): Free[Lang, Meta] =
    Free.liftF(GetMeta(login, identity))

  protected def removeMeta(login: String): Free[Lang, Unit] =
    Free.liftF(RemoveMeta(login, ()))

  protected def createUser(firstName: String, lastName: String): Free[Lang, User] =
    Free.liftF(CreateUser(firstName, lastName, identity))

  protected def getUser(login: String): Free[Lang, User] =
    Free.liftF(GetUser(login, identity))

  protected def removeUser(login: String): Free[Lang, Unit] =
    Free.liftF(RemoveUser(login, ()))
}
