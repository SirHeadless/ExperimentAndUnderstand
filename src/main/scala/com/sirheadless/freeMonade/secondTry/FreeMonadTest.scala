package com.sirheadless.freeMonade.secondTry

import cats._
import cats.free.Free
import cats.{Id, ~>}

object FreeMonadTest extends App {

  import Program1._
  import Compiler1._
  import Program2._
  import Compiler2._
  import Program3._
  import PureCompiler._
  import GameBoard._


  val result1 = program1.foldMap(impureCompiler1)
  println(result1)

  val result2 = program2.foldMap(impureCompiler2)
  println(result2)

  println("############## Result3 ################")
  val result3 = program3.foldMap(pureCompiler)
  result3.map(println(_))

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

object Program1 {

  import GameBoard._
  import TTTBoardDsl._

  def program1: TTTBoard[Map[Int, GameSymbol]] =
    for {
      _ <- put(1, X)
      _ <- put(2, X)
      _ <- put(3, X)
      _ <- put(7, O)
      list <- getAll[Int]
    } yield list
}

object Program2 {

  import GameBoard._
  import TTTBoardDsl._

  def program2: TTTBoard[Map[(Int, Int), GameSymbol]]  =
    for {
      _ <- put((0,1), X)
      _ <- put((0,2), X)
      _ <- put((1,0), X)
      _ <- put((2,1), O)
      list <- getAll[(Int,Int)]
    } yield list
}

object Program3 {

  import GameBoard._
  import TTTBoardDsl._

  def program3: TTTBoard[Option[GameSymbol]]  =
    for {
      _ <- put((0,1), X)
      _ <- put((0,2), X)
      _ <- put((1,0), X)
      _ <- put((2,1), O)
      symbol <- get(2)
    } yield symbol
}


// 4. Impure compiler
object Compiler1 {

  import GameBoard._
  import cats.arrow.FunctionK
  import Program1._

  def impureCompiler1: TTTBoardADT ~> Id = new (TTTBoardADT ~> Id) {

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
//        board.foldLeft((0, List.empty: List[(Int,GameSymbol)]))((acc, symbol) => (((acc._1 + 1, (acc._1 , symbol) :: acc._2))))._2.toMap.asInstanceOf[A]
        board.indices.zip(board).toMap.asInstanceOf[A]
    }
  }

  def run() = program1.foldMap(impureCompiler1)
}


object Compiler2 {

  import GameBoard._
  import cats.arrow.FunctionK
  import Program2._

  def impureCompiler2: TTTBoardADT ~> Id = new (TTTBoardADT ~> Id) {

    var board = List(List.fill(3)(NoSymbol: GameSymbol), List.fill(3)(NoSymbol: GameSymbol), List.fill(3)(NoSymbol: GameSymbol))

    def apply[A](fa: TTTBoardADT[A]): Id[A] = fa match {
      case Put(index: (Int, Int), symbol) =>
        println(s"Insert ${symbol} at postition ${index}")
        board = board.updated(index._1, board(index._1).updated(index._2, symbol))
        ()
      case Get(index: (Int, Int)) =>
        println(s"Get symbol of index ${index}")
        board.lift(index._1).flatMap(_.lift(index._2)).asInstanceOf[A]
      case GetAll() =>
        //        new Map[Int, Game]
      val helperBoard = board.flatMap(row => row.indices.zip(row))

        helperBoard.indices.zip(helperBoard).map(x => ((x._1,x._2._1), x._2._2)).toMap.asInstanceOf[A]
    }
  }

  def run() = program2.foldMap(impureCompiler2)
}

// 6. Use Pure Compiler


object PureCompiler {
  import cats.data.State
  import Program3._

  import GameBoard._

  type TTTBoardState[A] = State[List[GameSymbol], A]
  val pureCompiler: TTTBoardADT ~> TTTBoardState = new (TTTBoardADT ~> TTTBoardState) {
    override def apply[A](fa: TTTBoardADT[A]): TTTBoardState[A] = fa match {
      case Put(index: Int, symbol: GameSymbol) => State.modify(_.updated(index, symbol))
      case Get(index: Int)  => State.inspect(_.lift(index).asInstanceOf[A])
    }
  }

  def run() = program3.foldMap(pureCompiler)
}