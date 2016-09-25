package com.jancy.mateusz.sug.free.service

import com.jancy.mateusz.sug.free.model.User

import scala.collection.mutable

object UserService {

  private val ldap = mutable.Map.empty[String, User]

  def generateUniqueLogin(firstName: String, lastName: String): String = s"$firstName.$lastName@mail.com"

  def create(login: String, firstName: String, lastName: String): User = {
    val user = User(login, firstName, lastName)
    ldap += (login -> user)
    user
  }

  def get(login: String): Option[User] = ldap.get(login)

  def remove(login: String): Unit = ldap - login
}
