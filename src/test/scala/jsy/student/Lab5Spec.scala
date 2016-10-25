package jsy.student

import jsy.lab5.Lab5Like
import jsy.lab5.ast._
import jsy.lab5.Parser.parse
import jsy.tester.JavascriptyTester
import jsy.util.DoWith
import jsy.util.DoWith._
import org.scalatest._

class Lab5Spec(lab5: Lab5Like) extends FlatSpec {
  import lab5._

  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith(l1) { (i: Int) => if (i < 0) Some(doreturn(-i)) else None }
     assertResult((true,gold1)) { dowith(true) }
     assertResult((42,gold1)) { dowith(42) }
  }

  "mapWith(List)" should "map the elements of a list in a DoWith" in {
    val l = List(1, 2, 3, 4, 5)
    val r1 = l.map { i => i + 1 }

    def dowith1[W]: DoWith[W,List[Int]] = mapWith(l) { i: Int => doreturn(i + 1) }
    assertResult((true,r1)) { dowith1(true) }
    assertResult((42,r1)) { dowith1(42) }

    assertResult((2 * l.length + 1, r1)) {
      val dw: DoWith[Int,List[Int]] = mapWith(l) { i: Int =>
        domodify[Int](s => s + 2) map { _ => i + 1 }
      }
      dw(1)
    }
  }

  "rename" should "rename in a DoWith" in {
    val e1 = parse("const a = 1 + a; a")
    val e1p = parse("const aa = 1 + a; aa")

    assertResult((1,e1p)) {
      rename(empty, e1){ x => domodify[Int](n => n + 1) map { _ => x + x } }(0)
    }
  }

  "uniquify" should "uniquify with a counter for each variable" in {
    val e1 = parse("const a = 1; a")
    val e1p = parse("const a1 = 1; a1")
    val e2 = parse("const b = 2; b")
    val e2p = parse("const b0 = 2; b0")
    val e = Decl(MConst, "a", e1, e2)
    val ep = Decl(MConst, "a0", e1p, e2p)
    assertResult(ep) { uniquify(e) }
  }


  /* Tests based on rules */

  "CastOkNull" should "perform CastOkNull" in {
    assertResult(true) {
      castOk(TNull, TObj(Map.empty))
    }
  }

  "DoNeg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = Unary(Neg, e1)
    assertResult( N(-5) ) {
      val (_, r) = step(e2)(memempty)
      r
    }
  }

  // Probably want to write some tests for castOk, typeInfer, substitute, and step.

}

// An adapter class to pass in your Lab5 object.
class Lab5SpecRunner extends Lab5Spec(jsy.student.Lab5)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab5.
// The test expects a corresponding .ans file with the expected result.
class Lab5JsyTests extends JavascriptyTester(None, "lab5", jsy.student.Lab5)

class Lab5Suite extends Suites(
  new Lab5SpecRunner,
  new Lab5JsyTests
)