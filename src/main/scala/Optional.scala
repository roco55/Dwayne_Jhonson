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

@main def run(): Unit =
  println(eval(UnMin(UnMin(Lit(1)))))
  println(toString(Lit(7)))
  println(toString(Add(Lit(7), Lit(7))))
  println(toString(Mul(Lit(7), Add(Lit(7), Lit(7)))))
  println(toString(Add(Mul(Lit(7), Lit(7)), Mul(Lit(7), Lit(7)))))
  println(toString(UnMin(Add(Mul(Lit(7), Lit(7)), Mul(Lit(7), Lit(7))))))
