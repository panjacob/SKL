import scala.:+
import scala.annotation.tailrec
import scala.collection.mutable

object Hello extends App {
  val const: Int = 12
  var let: Int = 12
  var favouriteFloat: Float = 3.141F

  def function(x: Int, y: Int): Int = x + y + 1

  lazy val mix: Int = 1337

  //  if (favouriteFloat == 3.141F)
  //    println(f"My favourite number is ${favouriteFloat}%8.2f")
  //  else
  //    println(s"favourite float is not ${favouriteFloat}")

  def superFunc(): String = "Zwraca string!"

  @tailrec
  def loop(acc: Int = 0): Unit =
    if (acc < 10) {
      println(acc)
      loop(acc + 1)
    }

  //  loop()


  def higherOrderFunction(f: Int => String): Unit = {
    var acc = 0
    while (acc < 10) {
      println(f(acc))
      acc += 1
    }
  }

  def rendered(n: Int): String =
    Console.YELLOW + n + Console.RESET

  //  List(1,2,3,4).foreach(println)

  //  higherOrderFunction(rendered)

  //  println(List(3, 6, 8, 6, 3, 1).sortWith((a, b) => a <= b))
  //  println(List(3, 6, 8, 6, 3, 1).sortWith(_ <= _)) //Pojebany skrocik

  def caseFunction(x: Int): String = x match {
    case 4 => "String 4"
    case _ => "Not 4"
  }


  val result =
    List(1, 2, 3, 4, 5, 6, 7, 8, 9) match {
      case List(_, _, third, fourth, _*) if third == fourth - 1 => true
      case _ => false
    }
  //  println(result)

  val List(_, _, third, fourth, rest@_*) = List(1, 2, 3, 4, 5, 6, 7)
  //  println(third)
  //  println(fourth)

  //  println(Seq(1,2,3))
  //  println(List(1,2,3,4,5,6,7))
  //  println(IndexedSeq(1,2,3))
  //  println(Vector(1,2,3))
  //  println(Set(1,2,3,4,5,6,7))
  //  println(mutable.HashSet(1,2,3,4,5,6,7))
  //  List(1,2,3,4,5,6).foreach(println)
  //  List(1,2,3,4,5,6).map(_ + 1).foreach(println)
  //  List(1, 2, 3, 4, 5, 6).filter(_ % 2 != 0).foreach(println)

  //  List(1, 2, 3, 4, 5, 6).flatMap { n =>
  //    if (n % 2 == 0)
  //      List.empty
  //    else
  //      List(n)
  //  }.foreach(println)

  val matrix = {
    List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )
  }
  //  matrix.foreach(println)
  //  matrix.flatten.foreach(println) //flatten splaszcza liste do jednego wymiaru
  //  matrix.map(_.reverse).flatten.foreach(println)
  //  matrix.flatMap(_.reverse).foreach(println)

  //  println(Map((1, "One")))
  //  Map((1 -> "I"), (2 -> "II"), (3 -> "III")).map {
  //    tuple2 =>
  //      val key = tuple2._1
  //      val value = tuple2._2
  //      key -> value.toLowerCase
  //  }.foreach(println)
  //
  //  Map((1 -> "I"), (2 -> "II"), (3 -> "III")).map {
  //    case (key, value) => key -> value.toLowerCase
  //  }.foreach(println) //Skrocona wersja

  //  Range(start = 0, end = 10, step = 2).foreach(println)
  //  0 to 9 by 2 foreach println
  //  0.to(9).by(2).foreach(println)
  //  0 until 9 by 2 foreach println

  //  for (i <- 0 to 9)
  //    println(i)

  val chessBoard =
    for (c <- 'a' to 'h')
      yield for (n <- 0 to 8)
        yield c -> n

  val chessBoard2 =
    ('a' to 'h').map { c =>
      (0 to 8).map({ n =>
        c -> n
      })
    }

  val chessBoard3 = for (c <- 'a' to 'h'; n <- 0 to 8) yield c -> n

  //  println(chessBoard)

  def fibonacci(n: Int): Int =
    if (n <= 1) 1 else fibonacci(n - 1) * n

  //  1 to 9 map fibonacci foreach println

  def fibbonacciTail(n: Int): Int = {
    @tailrec
    def loop(x: Int, acc: Int): Int =
      if (x <= 1)
        acc
      else
        loop(x = x - 1, acc = acc * x)

    loop(x = n, acc = 1)
  }

  //  1 to 9 map fibbonacciTail foreach println

  //  Dodawanie do listy, vectorow
  val list = List(1, 2, 3)
  println(0 :: list)
  println(0 +: list)
  println(0 +: List(1, 2, 3))
  println(List(1, 2, 3) :+ 4)
  println(list ::: List(4, 5, 6))
  println(list ++ List(4, 5, 6))

  val set = Set(1, 2, 3)
  println(set)
  println(set + 0)
  println(set ++ Set(4, 5, 6))

  val map = Map(1 -> "I", 2 -> "II", 3 -> "III")
  println(map)
  println(map + (4 -> "IV"))
  println(map + (3 -> "used to be III"))
  println(map ++ Map(3 -> "used to be III", 4 -> "IV"))

  val sum = List(1, 2, 3, 4).foldLeft(0)(_ + _)

  println(sum)

  def totalLength(l: List[String]): Int = {
    l match {
      case List() => 0
      case h :: t => {
        val pom = totalLength(t)
        h.length + pom
      }
    }
  }

  def totalLength2(l: List[String]):Int = {
    l.map(_.length).sum
  }
  println(totalLength2(List("Ala", "ma", "kota")))
}