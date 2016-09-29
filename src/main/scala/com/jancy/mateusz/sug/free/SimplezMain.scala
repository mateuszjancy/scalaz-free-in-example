package com.jancy.mateusz.sug.free

import scala.io.StdIn
import scala.util.Random
import scala.util.control.NonFatal
import scalaz._

object SimplezMain extends App {

  //ADT
  sealed trait Lang[+Next]

  case class RandomNumber[Next](next: Int => Next) extends Lang[Next]

  case class Guess[Next](next: Int => Next) extends Lang[Next]

  case class Answer[Next](number: Int, answer: Int, next: Next) extends Lang[Next]

  case class Fail(ex: Throwable) extends Lang[Nothing]

  //Functor
  implicit val functor = new Functor[Lang] {
    override def map[A, B](lang: Lang[A])(f: (A) => B): Lang[B] =
      lang match {
        case RandomNumber(next) => RandomNumber(n => f(next(n)))
        case Guess(next) => Guess(n => f(next(n)))
        case Answer(n, q, next) => Answer(n, q, f(next))
        case Fail(ex) => Fail(ex)
      }
  }

  //Monads
  def randomNumber: Free[Lang, Int] = Free.liftF(RandomNumber(identity))

  def guess: Free[Lang, Int] = Free.liftF(Guess(identity))

  def answer(number: Int, answer: Int): Free[Lang, Unit] = Free.liftF(Answer(number, answer, ()))

  //Program
  def quiz: Free[Lang, Unit] = for {
    n <- randomNumber
    g <- guess
    _ <- answer(n, g)
  } yield ()

  //Interpreter
  def run[T](program: Free[Lang, T]): Either[Throwable, T] = program.resume.fold({
    case RandomNumber(next) =>
      run(next(Random.nextInt(10)))
    case Guess(next) =>
      println("Try to guess random number (from 0 to 10):")
      try {
        run(next(StdIn.readInt()))
      } catch {
        case NonFatal(ex) => run(Free.liftF(Fail(ex)))
      }

    case Answer(n, q, next) =>
      println(if (n == q) "Correct!!" else s"Wrong!!! Correct answer was: $n")
      run(next)
    case Fail(ex) => Left(ex)
  }, (a: T) => Right(a))

  run(quiz)
}
