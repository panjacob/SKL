
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
    //    println("isXinLocation: ", location)

    val (x, y) = location
    if (x < 0 || y < 0) return false
    else if (x >= board.length || y >= board(0).length) return false
    else board(x)(y) == 'X'
  }

  def findContinents(board: Array[Array[Char]], l: Char = 'A'): Array[Array[Char]] = {
    //    printBoard(board)
    val location = findX(board, (0, 0))
    if (location._1 == -1 && location._2 == -1) return board

    board(location._1)(location._2) = l
    val newNeighbours = findNeighbours(board, location, (-1, -1))
    println(newNeighbours.mkString(f"Neighbours: ${l}[", ", ", "]"))
    val newBoard = fillBoardWithSpecificLetter(board, newNeighbours, l)
    //    printBoard(newBoard)
    val newL = (l + 1).toChar
    findContinents(newBoard, newL)
    //    if (i != -1 && j != -1) {
    //      board(i)(j) = ll
    //      findContinents(board, l, ll)
    //    } else
    //      return board

    //    val newL = (l + 1).toChar
    //    board(x)(y) = newL
    //    val (newBoard, found) = findNeighbours(board, newL, x, y)
    //    if (found) findContinents(newBoard, l, x, y)
    //    else findContinents(newBoard, 'X', x, y)
    board
  }

  def findNeighbours(board: Array[Array[Char]], location: Location, locationParent: Location): Array[Location] = {
//    println("debug location", location)
    val top = (location._1, location._2 + 1)
    val right = (location._1 + 1, location._2)
    val bottom = (location._1, location._2 - 1)
    val left = (location._1 - 1, location._2)


    val topRes =
      if (top != locationParent && isXinLocation(board, top)) findNeighbours(board, top, location) else Array.empty[Location]
    val rightRes =
      if (right != locationParent && isXinLocation(board, right)) findNeighbours(board, right, location) else Array.empty[Location]
    val bottomRes =
      if (bottom != locationParent && isXinLocation(board, bottom)) findNeighbours(board, bottom, location) else Array.empty[Location]
    val leftRes =
      if (left != locationParent && isXinLocation(board, left)) findNeighbours(board, left, location) else Array.empty[Location]

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


  val newBoard = findContinents(board)
  println("Result visualised")
  printBoard(newBoard)
}