
import scala.annotation.tailrec
import scala.collection.mutable

object lab5 extends App {
  def lt(a: Int, b: Int): Boolean = a < b

  def ltchar(a: Char, b: Char): Boolean = a < b

  def mergeSort[T](xs: List[T], lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (left, right) = xs splitAt (n)
      merge(mergeSort(left, lt), mergeSort(right, lt))
    }
  }

  val lchar = List('s', 'z', 'a', 'l', 'f', 'r', 'q', 't')
  val l = List(9, 4, 6, 2, 34, 7, 54, 3, 2, 1)
  println(mergeSort(l, lt))
  println(mergeSort(lchar, ltchar))
}