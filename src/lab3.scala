import scala.annotation.tailrec
import scala.collection.mutable

object lab3 extends App {
  def fibonacci(n: BigInt): BigInt =
    if (n == 0 || n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)

  //  println(fibonacci(50))


  def young(x: Int): Int = {
    x match {
      case 0 => 1
      case _ => old(x - 1)
    }
  }

  def old(x: Int): Int = {
    x match {
      case 0 => 1
      //      case _ => old(x - 1) + young(x-1)
      case _ => if (x % 4 == 0) 0 else old(x - 1) + young(x - 1)
    }
  }

  println(young(2))
  for (i <- 0 to 10)
    println(s"[${i}] young: ${young(i)}    old: ${old(i)}")

  //  Sortowanie listy rekurencyjnie. Chcemy zeby było posortowane rosnąco
  def putInList(l: List[Int], rest: List[Int]): List[Int] = {
    l
  }

  def sortList(l: List[Int]): List[Int] = {
    l match {
      case Nil => l
      case h :: t => putInList(List(h), t)
    }
  }

}