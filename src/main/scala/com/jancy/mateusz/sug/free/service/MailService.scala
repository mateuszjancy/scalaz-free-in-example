package com.jancy.mateusz.sug.free.service

object MailService {
  def send(from: String, to: List[String], subject: String, message: String): Unit =
    println(s"Sending mail from: $from, to: $to, with subject: $subject and message: $message")
}
