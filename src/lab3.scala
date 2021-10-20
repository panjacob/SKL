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

  //  println(young(2))
  //  for (i <- 0 to 10)
  //    println(s"[${i}] young: ${young(i)}    old: ${old(i)}")


  def sortList(l: List[Int]): List[Int] = {
    def wstawSort(posortowane: List[Int], chaos: List[Int]): List[Int] =
      chaos match {
        case Nil => posortowane
        case h :: t => wstawSort(wstaw(posortowane, h), t)
      }

    def wstaw(l: List[Int], n: Int): List[Int] = {
      l match {
        case Nil => List(n)
        case h :: t => if (n < h) n :: l else h :: wstaw(t, n)
      }
    }

    wstawSort(Nil, l)
  }

  def sortListParam[T](l: List[T], lt: (T, T) => Boolean): List[T] = {
    def wstawSort(posortowane: List[T], chaos: List[T]): List[T] =
      chaos match {
        case Nil => posortowane
        case h :: t => wstawSort(wstaw(posortowane, h), t)
      }

    def wstaw(l: List[T], n: T): List[T] = {
      l match {
        case Nil => List(n)
        case h :: t => if (lt(n, h)) n :: l else h :: wstaw(t, n)
      }
    }

    wstawSort(Nil, l)
  }

  val l = List(5, 2, 1, 9, 6, 7, 3, 5, 7, 0)
  println(sortList(l))

  def lt(a: Int, b: Int): Boolean = a < b
  def gt(a: Int, b: Int): Boolean = a > b

  println(sortListParam(l, lt))
  println(sortListParam(l, gt))
}