package com.jancy.mateusz.sug.free.service

import com.jancy.mateusz.sug.free.model.Meta

import scala.collection.mutable

object MetaService {
  val ACTIVE = true

  val db = mutable.Map.empty[String, Meta]

  def create(login: String, services: List[String], mails: List[String]): Meta = {
    val meta = Meta(login, services, mails)
    db += (login -> meta)
    meta
  }

  def get(login: String): Meta = db(login)

  def remove(login: String): Unit = db - login
}
