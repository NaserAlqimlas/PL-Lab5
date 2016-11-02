package jsy.lab5

import jsy.lab5.ast._
import jsy.lab5.Parser.parse
import jsy.util.{DoWith, JsyApplication}
import jsy.util.DoWith._

trait Lab5Like { a: JsyApplication =>

  /* Environments */
  def empty[A]: Map[String,A] = Map.empty
  def lookup[A](env: Map[String,A], x: String): A = env(x)
  def extend[A](env: Map[String,A], x: String, a: A): Map[String,A] = {
    env + (x -> a)
  }

  /* Rename */
  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr]
  def myuniquify(e: Expr): Expr

  /* Map and MapFirst */
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]]
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]]
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]]

  /* Type Inference */
  def castOk(t1: Typ, t2: Typ): Boolean
  def isBindex(m: Mode, e: Expr): Boolean
  type TEnv = Map[String, MTyp]
  def typeof(env: TEnv, e: Expr): Typ

  /* Step */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean
  def substitute(e: Expr, v: Expr, x: String): Expr
  def isRedex(mode: Mode, e: Expr): Boolean
  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr]
  def step(e: Expr): DoWith[Mem,Expr]

  /* Lower */
  def lower(e: Expr): Expr

  /** Interface for rename with a given fresh op. */
  def rename[W](e: Expr)(z: W)(fresh: String => DoWith[W,String]): Expr = {
    val (_, r) = rename(empty, e)(fresh)(z)
    r
  }

  /** Interface to run your rename with a global counter for each variable name. */
  def uniquify(e: Expr): Expr = {
    def fresh(x: String): DoWith[Map[String,Int],String] = doget flatMap { env =>
      val i = env.getOrElse(x, 0)
      val xp = x + i
      doput( env + (x -> (i+1)) ) map { _ => xp }
    }
    rename(e)(empty: Map[String,Int])(fresh)
  }
  def uniquify(s: String): Expr = uniquify(parse(s))

  /** Interface to run myuniquify. */
  def myuniquify(s: String): Expr = myuniquify(parse(s))

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    val t = typeof(empty, e)
    println(s"## type ${e} : ${pretty(t)}")
    t
  }

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    //require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e, n)
      else if (isValue(e)) doreturn( e )
      else {
        doget[Mem] flatMap { m =>
          println("## step %4d:%n##  %s%n##  %s".format(n, m, e))
          step(e) flatMap { ep => loop(ep, n + 1) }
        }
        /* The following commented-out code is the same as the above but using Scala's
        for-yield syntax for calls to flatMap:

        for {
          m <- doget[Mem]
          _ = println("## step %4d:%n##  %s%n##  %s".format(n, m, e))
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield epp
        */
      }
    val (m,v) = loop(e, 0)(memempty)
    println("## result:%n##  %s%n##  %s".format(m, v))
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(lower(parse(s)))

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("# ============================================================")
      println("# File: " + file.getName)
      println("# Parsing ...")
    }

    val exprin =
      handle(None: Option[Expr]) {Some{
        jsy.lab5.Parser.parseFile(file)
      }} getOrElse {
        return
      }

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Lowering %s ...".format(exprin))
    }

    val expr =
      handle(None: Option[Expr]) {Some{
        lower(exprin)
      }} getOrElse {
        return
      }

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Type checking %s ...".format(expr))
    }

    val welltyped = handle(false) {
      val t = inferType(expr)
      true
    }
    if (!welltyped) return

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Stepping ...".format(expr))
    }

    handle() {
      val v = iterateStep(expr)
      println(pretty(v))
    }
  }

}
