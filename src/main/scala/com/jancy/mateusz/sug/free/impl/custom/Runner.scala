package com.jancy.mateusz.sug.free.impl.custom

import com.jancy.mateusz.sug.free.impl.custom.Lang._
import com.jancy.mateusz.sug.free.service.{MailService, MetaService, UserService}

import scalaz._

object Runner {

  def run[T](lang: Free[Lang, T]): Either[Throwable, T] = lang.resume.fold({
    case SendMail(from, to, subject, message, next) =>
      MailService.send(from, to, subject, message)
      run(next)

    case CreateMeta(login, services, mails, next) =>
      val meta = MetaService.create(login, services, mails)
      run(next(meta))

    case GetMeta(login, next) =>
      val meta = MetaService.get(login)
      run(next(meta))

    case RemoveMeta(login, next) =>
      MetaService.remove(login)
      run(next)

    case CreateUser(firstName, lastName, next) =>
      val login = UserService.generateUniqueLogin(firstName, lastName)
      val user = UserService.create(login, firstName, lastName)
      run(next(user))

    case GetUser(login, next) =>
      val user = UserService.get(login)
      user match {
        case Some(u) => run(next(u))
        case None => run(Free.liftF(Error(new Throwable(s"No user found for $login"))))
      }

    case RemoveUser(login, next) =>
      UserService.remove(login)
      run(next)

  }, (result: T) => Right(result))
}
