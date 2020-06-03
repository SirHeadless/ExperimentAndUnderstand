package com.sirheadless.experiment

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, RunnableGraph, Sink, Source}

import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

object SourceSink {

  implicit val actorSystem = ActorSystem("akka-system")
  implicit val flowMaterializer = ActorMaterializer()

  def main(args: Array[String]): Unit = {
    val source = Source(1 to 10)
    val sink = Sink.fold[Int, Int](0)(_ + _)

    // connect the Source to the Sink, obtaining a RunnableGraph
    val runnable: RunnableGraph[Future[Int]] = source.toMat(sink)(Keep.right)

    // materialize the flow and get the value of the FoldSink
    val sum: Future[Int] = runnable.run()
//    sum.onComplete{
//      case Success(value) => println(value)
//      case Failure(exception) => println("Error")
//    }
    sum.foreach{
      sum => println(sum)
    }
  }

//  def main(args: Array[String]): Unit = {
//    val source = Source(1 to 10)
//    val sink = Sink.fold[Int, Int](0)(_ + _)
//
//    // materialize the flow, getting the Sinks materialized value
//    val sum: Future[Int] = source.runWith(sink)
//  }

}
