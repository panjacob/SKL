import scala.annotation.tailrec
import scala.collection.mutable

object lab2 extends App {
  val s: String = "ala ma duzego kota"
  val listS = s.toList

  type result = Int

  def count_iter(s: String, z: Char): result = {
    s.toList.map((x: Char) => if (x == 'a') 1 else 0).sum
  }

  def count_rec(s: String, z: Char): result = {
    s match {
      case "" => {
        println("String empty returning 0")
        0
      }
      case _ => {
        val returnedValue = count_rec(s.tail, z) + (if (s.head == z) 1 else 0)
        println(s"String has: ${s}   returnedValue = ${returnedValue}")

        count_rec(s.tail, z) + (if (s.head == z) 1 else 0)
      }
    }
  }

  //  def countAll_rec(s: String): Map[Char, Int] = {
  //    s match {
  //      case "" => {
  //        Map()
  //      }
  //      case _ => {
  //        println(s"String has: ${s}")
  ////        countAll_rec(s.tail)
  //
  //        countAll_rec(s.tail) ++ Map(s.head -> +1)
  //      }
  //    }
  //  }
  def countAllRec(s: String): Map[Char, Int] = {
    @tailrec
    def loop(x: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (x == "") {
        println(s"Returning: ${x}")
        acc
      } else {
        val newAcc: Map[Char, Int] =
          if (acc.keySet.contains(x.head)) {
            acc + (x.head -> (acc(x.head) + 1))
          } else
            acc + (x.head -> 1)
        println(s"Got: ${x}        |     newAcc = ${newAcc}")

        loop(x = x.tail, newAcc)
      }


    loop(x = s, acc = Map.empty)
  }

  def countAllRecClean(s: String): Map[Char, Int] = {
    @tailrec
    def loop(x: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (x == "")
        acc
      else {
        val newAcc: Map[Char, Int] =
          if (acc.keySet.contains(x.head)) acc + (x.head -> (acc(x.head) + 1))
          else acc + (x.head -> 1)
        loop(x = x.tail, acc = newAcc)
      }

    loop(x = s, acc = Map.empty)
  }

  def countAllRecClean2(l: String): Map[Char, Int] = {
    l match {
      case "" => Map()
      case _ => {
        val newAcc = countAllRecClean2(l.tail)
        newAcc + (l.head -> (newAcc.getOrElse(l.head, 0)+1))
      }
    }
  }

  println(countAllRecClean2(s))


}
