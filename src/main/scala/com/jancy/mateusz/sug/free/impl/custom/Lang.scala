package com.jancy.mateusz.sug.free.impl.custom

import com.jancy.mateusz.sug.free.model._

import scalaz.Functor

sealed trait Lang[+Next]

object Lang {

  case class SendMail[Next](from: String, to: List[String], subject: String, message: String, next: Next) extends Lang[Next]

  case class CreateMeta[Next](login: String, services: List[String], mails: List[String], next: Meta => Next) extends Lang[Next]

  case class GetMeta[Next](login: String, next: Meta => Next) extends Lang[Next]

  case class RemoveMeta[Next](login: String, next: Next) extends Lang[Next]

  case class CreateUser[Next](firstName: String, lastName: String, next: User => Next) extends Lang[Next]

  case class GetUser[Next](login: String, next: User => Next) extends Lang[Next]

  case class RemoveUser[Next](login: String, next: Next) extends Lang[Next]

  case class Error(ex: Throwable) extends Lang[Nothing]

  implicit val functor = new Functor[Lang] {
    override def map[A, B](a: Lang[A])(f: (A) => B): Lang[B] = a match {
      case SendMail(from, to, subject, message, next) =>
        SendMail(from, to, subject, message, f(next))
      case CreateMeta(login, services, mails, next) =>
        CreateMeta(login, services, mails, n => f(next(n)))
      case GetMeta(login, next) =>
        GetMeta(login, n => f(next(n)))
      case RemoveMeta(login, next) =>
        RemoveMeta(login, f(next))
      case CreateUser(firstName, lastName, next) =>
        CreateUser(firstName, lastName, n => f(next(n)))
      case GetUser(login, next) =>
        GetUser(login, n => f(next(n)))
      case RemoveUser(login, next) =>
        RemoveUser(login, f(next))
      case Error(msg) =>
        Error(msg)
    }
  }
}
