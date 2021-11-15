
import scala.annotation.tailrec
import scala.collection.mutable

object lab6 extends App {
  def isPrime(x: Int, l: List[Int]): Boolean = {
    if (l.toSet.contains(x)) true
    l match {
      case h :: t => if (x % h == 0) false else isPrime(x, t)
      case List() => true
    }
  }

//  def primes(x: Int): List[Int] = {
//    if (x < 2) List()
//    else {
//      val l = primes(x - 1)
//    }
//  }

  //  println(isPrime(5))
//  primes(10)
}