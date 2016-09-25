package com.jancy.mateusz.sug.free

import scala.io.StdIn
import scala.util.Random


object SimpleMain extends App {

  //Free
  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  case class Done[F[_] : Functor, A](a: A) extends Free[F, A]

  case class More[F[_] : Functor, A](k: F[Free[F, A]]) extends Free[F, A]

  class Free[F[_], A](implicit F: Functor[F]) {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Done(a) => f(a)
      case More(k) => More[F, B](F.map(k)(_ flatMap f))
    }

    def map[B](f: A => B): Free[F, B] = flatMap { x => Done(f(x)) }
  }

  //ADT
  sealed trait Lang[Next]

  case class RandomNumber[Next](next: Int => Next) extends Lang[Next]

  case class Guess[Next](next: Int => Next) extends Lang[Next]

  case class Answer[Next](number: Int, answer: Int, next: Next) extends Lang[Next]

  //Functor
  implicit val functor = new Functor[Lang] {
    override def map[A, B](lang: Lang[A])(f: (A) => B): Lang[B] =
      lang match {
        case RandomNumber(next) => RandomNumber(n => f(next(n)))
        case Guess(next) => Guess(n => f(next(n)))
        case Answer(n, q, next) => Answer(n, q, f(next))
      }
  }

  //Monads
  def randomNumber: Free[Lang, Int] = More(RandomNumber(x => Done(x)))

  def guess: Free[Lang, Int] = More(Guess(x => Done(x)))

  def answer(number: Int, answer: Int): Free[Lang, Unit] = More(Answer(number, answer, Done(())))

  //Program
  def quiz: Free[Lang, Unit] = for {
    n <- randomNumber
    g <- guess
    _ <- answer(n, g)
  } yield ()

  //Interpreter
  def run[T](program: Free[Lang, T]): T = program match {
    case More(RandomNumber(next)) =>
      run(next(Random.nextInt(10)))
    case More(Guess(next)) =>
      println("Try to guess random number (from 0 to 10):")
      run(next(StdIn.readInt()))
    case More(Answer(n, q, next)) =>
      println(if (n == q) "Correct!!" else s"Wrong!!! Correct answer was: $n")
      run(next)
    case Done(done) => done
  }

  run(quiz)
}
