package com.sirheadless.freeMonade.secondTry

import cats._
import cats.free.Free
import cats.{Id, ~>}

object FreeMonadTest extends App {

  import Programm1._
  import Compiler1._
  import GameBoard._


  val result = program.foldMap(impureCompiler)
  print(result)
}

object GameBoard {

  sealed trait GameSymbol
  case object X extends GameSymbol
  case object O extends GameSymbol
  case object NoSymbol extends GameSymbol

  sealed trait TTTBoardADT[A]
  case class Put[T](index: T, symbol: GameSymbol) extends TTTBoardADT[Unit]
  case class Get[T](index: T) extends TTTBoardADT[Option[GameSymbol]]
  case class GetAll[T]() extends TTTBoardADT[Map[T, GameSymbol]]

}

object TTTBoardDsl {
  import GameBoard._

  type TTTBoard[A] = Free[TTTBoardADT, A]
  import cats.free.Free.liftF

  def put[T](index: T, symbol: GameSymbol): TTTBoard[Unit] =
    liftF[TTTBoardADT, Unit](Put[T](index, symbol))

  def get[T](index: T): TTTBoard[Option[GameSymbol]] =
    liftF[TTTBoardADT, Option[GameSymbol]](Get[T](index: T))

  def getAll[T](): TTTBoard[Map[T, GameSymbol]] =
    liftF[TTTBoardADT, Map[T, GameSymbol]](GetAll[T])
}

object Programm1 {

  import GameBoard._
  import TTTBoardDsl._

  def program: TTTBoard[Map[Int, GameSymbol]] =
    for {
      _ <- put(1, X)
      _ <- put(2, X)
      _ <- put(3, X)
      _ <- put(7, O)
      list <- getAll[Int]
    } yield list
}

// 4. Impure compiler
object Compiler1 {

  import GameBoard._
  import cats.arrow.FunctionK
  import Programm1._

  def impureCompiler: TTTBoardADT ~> Id = new (TTTBoardADT ~> Id) {

    var board = List.fill(9)(NoSymbol: GameSymbol)

    def apply[A](fa: TTTBoardADT[A]): Id[A] = fa match {
      case Put(index: Int, symbol) =>
        println(s"Insert ${symbol} at postition ${index}")
        board = board.updated(index, symbol)
        ()
      case Get(index: Int) =>
        println(s"Get symbol of index ${index}")
        board.lift(index).asInstanceOf[A]
      case GetAll() =>
//        new Map[Int, Game]
        board.foldLeft((0, List.empty: List[(Int,GameSymbol)]))((acc, symbol) => (((acc._1 + 1, (acc._1 , symbol) :: acc._2))))._2.toMap.asInstanceOf[A]
    }
  }

  def run() = program.foldMap(impureCompiler)
}
