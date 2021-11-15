
import scala.annotation.tailrec
import scala.collection.mutable

object lab7 extends App {
  val table = Map(
    'ż' -> 5,
    'a' -> 1,
    'b' -> 1
  )

  def wart(s: String, d: Map[Char, Int]): Int = {
    //    s.map(x => println(d(x)))
    s.foldLeft(0)((total, x) => total + d.getOrElse(x, 0))
  }

  def validate(s: String, d: Map[Char, Int]): Boolean = {
    s.foldLeft(false)((isBad, x) => isBad || !d.keySet.contains(x))
  }

//  println(wart("żółć", table))
  println(validate("", table))
}