
import Mines.board

import scala.annotation.tailrec
import scala.collection.mutable


object Mines extends App {
  type Location = (Int, Int)

  def printBoard(board: Array[Array[Char]]): Unit = {
    for (j <- 0 until ySize) {
      for (i <- 0 until xSize)
        print(" " + board(i)(j) + " ")
      println()
    }
    println()
  }

  def isXinLocation(board: Array[Array[Char]], location: Location): Boolean = {
    val (x, y) = location
    if (x < 0 || y < 0) false
    else if (x >= board.length || y >= board(0).length) false
    else board(x)(y) == 'X'
  }

  def findContinents(board: Array[Array[Char]], l: Char = 'A', forbiddenLocations: Array[Location] = Array.empty): Array[Array[Char]] = {
    val location = findX(board, (0, 0))
    if (location._1 == -1 && location._2 == -1) return board

    board(location._1)(location._2) = l
    val newNeighbours = findNeighbours(board, location, forbiddenLocations)
    println(newNeighbours.mkString(f"Neighbours: ${l}[", ", ", "]"))
    val newBoard = fillBoardWithSpecificLetter(board, newNeighbours, l)
    //    printBoard(newBoard)
    val newL = (l + 1).toChar
    val newForbiddenLocations = forbiddenLocations ++ newNeighbours
    findContinents(newBoard, newL, newForbiddenLocations)
    board
  }

  def findNeighbours(board: Array[Array[Char]], location: Location, forbiddenLocations: Array[Location]): Array[Location] = {
    //    println("debug location", location)
    val top = (location._1, location._2 + 1)
    val right = (location._1 + 1, location._2)
    val bottom = (location._1, location._2 - 1)
    val left = (location._1 - 1, location._2)

    //    println("forbidden", forbiddenLocations.mkString("Array(", ", ", ")"))
    //    println("TOP", top, "right", right, "bottom", bottom, "left", left)
    val topRes =
    if (!forbiddenLocations.contains(top) && isXinLocation(board, top)) findNeighbours(board, top, forbiddenLocations :+ top) else Array.empty[Location]
    forbiddenLocations +: (topRes)
    val rightRes =
      if (!forbiddenLocations.contains(left) && isXinLocation(board, right)) findNeighbours(board, right, forbiddenLocations :+ right) else Array.empty[Location]
    forbiddenLocations +: (rightRes)
    val bottomRes =
      if (!forbiddenLocations.contains(bottom) && isXinLocation(board, bottom)) findNeighbours(board, bottom, forbiddenLocations :+ bottom) else Array.empty[Location]
    forbiddenLocations +: (bottomRes)
    val leftRes =
      if (!forbiddenLocations.contains(left) && isXinLocation(board, left)) findNeighbours(board, left, forbiddenLocations :+ left) else Array.empty[Location]
    forbiddenLocations +: (leftRes)
    //    println(topRes.mkString("Array(", ", ", ")"), rightRes.mkString("Array(", ", ", ")"), bottomRes.mkString("Array(", ", ", ")"), leftRes.mkString("Array(", ", ", ")"))
    location +: (topRes ++ rightRes ++ bottomRes ++ leftRes)
  }


  @tailrec
  def findX(board: Array[Array[Char]], location: Location): (Location) = {
    val (x, y) = location
    val xSize = board.length
    val ySize = board(0).length
    if (isXinLocation(board, location)) (x, y)
    else if (x >= xSize) findX(board, (0, y + 1))
    else if (y >= ySize) (-1, -1)
    else if (x < xSize) findX(board, (x + 1, y))
    else (-2, -2)
  }

  def fillBoardWithSpecificLetter(board: Array[Array[Char]], locations: Array[Location], l: Char): Array[Array[Char]] = {
    for (location <- locations) {
      //      println("location", location, l)
      board(location._1)(location._2) = l
    }

    board
  }


  val xSize = 10
  val ySize = 10
  val board = Array.ofDim[Char](xSize, ySize)
  val r = scala.util.Random

  for (i <- 0 until xSize; j <- 0 until ySize)
    board(i)(j) = if (r.nextInt(10) > 6) 'X' else '.'
  printBoard(board)


  val newBoard = findContinents(board)
  println("Result visualised")
  printBoard(newBoard)

  def main(): Unit = {

  }
}