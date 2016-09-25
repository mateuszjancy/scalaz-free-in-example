package com.jancy.mateusz.sug.free

import com.jancy.mateusz.sug.free.impl.custom.{Registration, Runner}

object Main extends App {
  val result = Runner.run(Registration.createAccount("mateusz", "jancy"))
  println(result)
}
