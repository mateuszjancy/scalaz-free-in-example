package com.jancy.mateusz.sug.free

package object model {

  case class Meta(login: String, services: List[String], mails: List[String])

  case class User(login: String, firstName: String, lastName: String)

}