package com.tkroman.kpi.y2022.l1
import scala.annotation.tailrec


sealed trait IntExpr
case class Lit(x: Int) extends IntExpr
case class Add(a: IntExpr, b:IntExpr) extends IntExpr
case class Mul(a: IntExpr, b:IntExpr) extends IntExpr
case class UnMin(x: IntExpr) extends IntExpr


def eval( intExpr : IntExpr): Int =
intExpr match {
  case Lit(x) => x
  case Add(a, b) => eval(a) + eval(b)
  case Mul(a, b) => eval(a) * eval(b)
  case UnMin(x) => eval(x) * (-1)
}

def toString ( intExpr : IntExpr ) : String =
intExpr match {
  case Lit(x) => x.toString
  case Add(a,b) => "(" + toString(a) + " + " + toString(b) + ")"
  case Mul(a, b) => toString(a) + " * " + toString(b)
  case UnMin(x) => "-"  + toString(x) 
}

def solveRPN(eqn: String): String = {
    val items = eqn.split(" ")
    val accumulator = List[String]()
    items.foldLeft(accumulator)(foldingFunction).head
}

def foldingFunction (stack: List[String], a: String): List[String] = stack match {
  case List() => a :: stack  
  case List(_) => a :: stack
  case x::y::ys => a match {
      case "*" =>  x + "*" + y :: ys
      case "+" =>  "(" + x + "+" + y + ")"  :: ys
      case s: String => s :: stack
  }
}


@main def run(): Unit =
  println(solveRPN("4 2 * 5 + 6 * 7 +"))
 
