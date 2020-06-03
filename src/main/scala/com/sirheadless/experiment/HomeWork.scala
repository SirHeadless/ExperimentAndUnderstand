package com.sirheadless.experiment

object HomeWork extends App {
  import scala.math.Ordering._

  val test = List(1,4,3,2,6,7,2,2,4,6,7,3,8,3,4,2)

  val result = test.foldLeft(List.empty: List[Int])((x,y) =>
    if (x.size < 3)
      (y :: x).sorted
    else if (y > x(0))
      (y :: x).sorted.tail
    else x
  )

//  print(result)

//  val result2 = test.fold[List[Int]](List.empty: List[Int])((x: List[Int],y: List[Int]) =>
//    if (x.size < 3)
//      (y :: x).sorted
//    else if (y > x(0))
//      (y :: x).sorted.tail
//    else x
//  )
}
