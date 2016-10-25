/*
 * CSCI 3155: Lab 5 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab5.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab5.Parser.parse

// Imports the ast nodes
import jsy.lab5.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab5._

// Parse code with assignments
parse("var x = 1; x = 2; console.log(x)")
parse("const x = {f: 1}; x.f = 2; console.log(x.f)")

// Parse code with null
parse("null")
parse("<Null>null")
parse("<{f: number}>null")

// Parse functions
parse("function (x: number) { return x }")
parse("function (var x: number) { x = 0; return x }")
parse("var y = 1; (function (ref x: number) { x = 0; return x })(y)")
parse("(function (name x: number) { return 0 })(console.log(100))")

// DoWith exercise: Rename
val e1 = parse("const a = 1; a")
val e1p = parse("const x1 = 1; x1")
val e2 = parse("const a = 2; a")
val e2p = parse("const x2 = 2; x2")
val e = Decl(MConst, "a", e1, e2)
val ep = Decl(MConst, "x0", e1p, e2p)
//assert(rename(e) == ep) // uncomment when you are ready to test your rename function.

// Aliasing example from handout
val aliasingex = parse("""
  const x = { f: 1 }
  const y = x
  x.f = 2
  console.log(y.f)
""")
//iterateStep(aliasingex) // uncomment when you are ready to test your step function.
