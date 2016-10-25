/*
 * CSCI 3155
 *
 * Meeting 19-20: Encapsulating computation.
 */

// In this note, we explore the ideas of encapsulated data types. In particular,
// we see that we can generalize from collections with higher-order methods
// to other kinds of data structures with such methods.

/***** Option *****/

// We often want to "string together" operations on a data type.

def getSomething: Option[String] = ???

// An Option[A] type represents an optional value, and it is often used to
// represent possible error in computing a result. That is, either return
// Some of the result or None to indicate error.

// Suppose you compute something (in useSomething1) in terms of getSomething.
// Since getSomething returns an Option type, one common case is that we also
// want to make useSomething1 return an Option type that returns None if
// either getSomething returns None or the work in useSomething1 fails.

def useSomething1: Option[String] =
  getSomething match {
      // If get getSomething returns None, then useSomething1 returns None.
    case None => None
      // Otherwise, useSomething1 can start doing work.
    case Some(s) =>
      val sp = s.trim
        // Check for an error condition (sp.length == 0). If so, return None.
      if (sp.length == 0) None
        // Otherwise, continue doing work.
      else Some(sp.toUpperCase)
  }

// There is a lot of "None handling" scaffolding mixed with the "work".

// Now, let's think of Option[String] as zero-or-one element list. That is,
// None is the zero element list and Some(s) is the one element list.

// So we write the following code in useSomething2 to do the same thing as
// useSomething1.

def useSomething2: Option[String] =
  getSomething map
    { s => s.trim } filter
    { s => s.length == 0 } map
    { s => s.toUpperCase }

// Wow, the "None handling" scaffolding is gone! The "None handling" scaffolding is
// precisely what is factored into the 'map' library method.

// Let's take a look at the 'map' library method for Options.

def map[A,B](aopt: Option[A])(f: A => B): Option[B] = aopt match {
  case None => None
  case Some(a) => Some(f(a))
}

// It is no accident that it is called 'map' like 'map' for Lists.

def map[A,B](al: List[A])(f: A => B): List[B] =
  al.foldRight(Nil: List[B]) { (h, acc) => f(h) :: acc }

// Observe that useSomething2 would be the same code if we changed all of
// Options to Lists.

def getSomethingList: List[String] = ???

def useSomethingList2: List[String] =
  getSomethingList map
    { s => s.trim } filter
    { s => s.length == 0 } map
    { s => s.toUpperCase }

// Key Idea: The concept of 'map' is generic to many data types. It defines
// a generic method that takes a function f to transform "the contents" of
// data type.

// Another common programming pattern is that we need to "sequence" work. What
// if we have two pieces of work that each could "error" by returning "None.
// Let's consider writing such code in seq1.

def seq1[A,B,T](aopt: Option[A], bopt: A => Option[B], f: (A,B) => T): Option[T] =
  // We have done work to produce aopt.
  aopt match {
      // If aopt is None, then we also need to return None.
    case None => None
      // Now, we can continue with the next piece of work in bopt that depends
      // on the result a.
    case Some(a) => bopt(a) match {
        // If this work returns None, then we need to return None.
      case None => None
        // Otherwise, we can do soemthing with both a and b.
      case Some(b) => Some(f(a, b))
    }
  }

// Let's try to get rid of the "None handling" scaffolding with 'map'.

def seq2[A,B,T](aopt: Option[A], bopt: A => Option[B], f: (A,B) => T): Option[Option[T]] =
  aopt map { a => bopt(a) map { b => f(a,b) } }

// Almost. We can get rid of the innermost bit of "None handling," but then the
// inner expression has an Option[T], so the overall type is Option[Option[T]],
// not Option[T], as we want.

// So it seems we are stuck with "None handling" code with 'map'.

def seq3[A,B,T](aopt: Option[A], bopt: A => Option[B], f: (A,B) => T): Option[T] =
  aopt match {
    case None => None
    case Some(a) => bopt(a) map { b => f(a,b) }
  }

// To factor out sequencing, we introduce another higher-order method called
// 'flatMap'. It is like 'map,' except it permits the callback to return an
// Option, and it will handle the "flattening".

def flatMap[A,B](aopt: Option[A])(f: A => Option[B]): Option[B] = aopt match {
  case None => None
  case Some(a) => f(a)
}

// Note that a 'map' can be implemented in terms of 'flatMap'. That is, 'flatMap'
// is more general. But because 'map' provides more scaffolding and is so often
// needed, most data types that have a notion of 'map' provide both the more
// restricted 'map' and the more general 'flatMap'.

// Now, let's use 'flatMap' to get our implementation of 'seq' with "None handling"
// factored into library methods.

def seq4[A,B,T](aopt: Option[A], bopt: A => Option[B], f: (A,B) => T): Option[T] =
  aopt flatMap { a => bopt(a) map { b => f(a,b) } }

// Key Idea: We generalize 'map' to 'flatMap'. The 'flatMap' is "sequencing"
// operators for Options.

// This is such a common pattern that Scala has special syntax for this: for-yield.

def seq5[A,B,T](aopt: Option[A], bopt: A => Option[B], f: (A,B) => T): Option[T] =
  for {
    a <- aopt
    b <- bopt(a)
  } yield f(a,b)

// Any type with map and flatMap defined can use this syntax, so it is not specific
// to Options.

/***** List *****/

// If we want to "sequence" two computations that return Lists, then the
// principle is the same.
// Let's first implement "sequencing" for Lists with two nested foldRights.

def seq1[A,B,T](alist: List[A], blist: A => List[B], f: (A,B) => T): List[T] =
  alist.foldRight(Nil: List[T]) {
    (a, acc) => blist(a).foldRight(acc) {
      (b, acc) => f(a,b) :: acc
    }
  }
seq1(List(1,2), { (a: Int) => List(a, a + 1) }, { (a: Int, b:Int) => (a,b) })

// The above code calls f on each pair of (a,b) where a is from alist and b is
// from blist (in order) and returns the list for the result of each call to
// f.

// A computation that returns a List can be seen as returning possibly multiple
// results. Sequencing such multiple-result computations is then captured by
// the above code.
// Or we should be able to the same thing with 'flatMap'.
def seq2[A,B,T](alist: List[A], blist: A => List[B], f: (A,B) => T): List[T] =
  alist flatMap { a => blist(a) map { b => f(a,b) } }
seq2(List(1,2), { (a: Int) => List(a, a + 1) }, { (a: Int, b:Int) => (a,b) })

def flatMap[A,B](alist: List[A])(f: A => List[B]): List[B] =
  alist.foldRight(Nil: List[B]) { (h, acc) => f(h) ::: acc }
def duplicate[A](l: List[A]): List[A] =
  l flatMap { a => List(a,a) }
duplicate(List(1,2))


/***** DoWith *****/

// In this section, we consider the idea that other data types also have their
// own versions of "sequencing" or 'flatMap.' These data types need not be
// "collections" in the traditional sense like List or even Option.

import jsy.lab5.ast._
import jsy.lab5.Parser.parse

// Let's consider a function that globally renames bound variables in an
// expression language by using variables x0, x1, ...

// The 'fresh' helper function creates variable names named xi given i
// and returns (i+1, "x" + i). The contract of the function is that
// that i on input is the next available variable number, and it
// returns an i' that is the next available variable number
// "allocating" a new variable number.

// Such kind of parameter-return value pairs can be viewed as a
// "state" variable where a call "updates" the state. Here, the
// "state" is the Int representing the next available variable
// number.
def fresh(i: Int): (Int, String) = (i+1, "x" + i)
// Now, let's implement rename.

def rename(env: Map[String, String], e: Expr)(i: Int): (Int,Expr) = e match {
  case N(_) => (i, e)
  case Var(x) => (i, Var(env.getOrElse(x, x)))
  case Binary(Plus, e1, e2) =>
    // First rename e1
    val (ip, e1p) = rename(env, e1)(i)
    // Then rename e2. Note that the "state" that was returned from renaming
    // e1 needs to be passed as the "state" parameter to renaming e2.
    val (ipp, e2p) = rename(env, e2)(ip)
    // Now the returned state is ipp.
    (ipp, Binary(Plus, e1p, e2p))
  case Decl(MConst, x, e1, e2) =>
    val (ip, xp) = fresh(i)
    val (ipp, e1p) = rename(env, e1)(ip)
    val (ippp, e2p) = rename(env + (x -> xp), e2)(ipp)
    (ippp, Decl(MConst, xp, e1p, e2p))
  case _ => ???
}

// Observations. When we need to recurse and do the sequence of renaming operations
// (e.g., for case Binary(Plus, e1, e2)), then we have to extremely careful with
// "threading" the "state" variable (i.e., i, ip, ipp). This is both "scaffolding"
// error-prone "scaffolding" to get right.

// Key Idea: We can implement a "sequencing" or 'flatMap' operator that implements
// this scaffolding.
//
// But for what data type? We see that 'rename' takes as input a
// (Map[String,String],Expr) and returns a function value of type
// Int => (Int, Expr). So we just need to encapsulate a value of this type and
// define 'map' and 'flatMap' that does this "sequencing."
// Let's call this data type DoWith, which takes two type parameters W and R.
// The W type parameter is the "state" type, while R is the "result" type.

sealed class DoWith[W,+R](doer: W => (W,R)) {
  def apply(w: W) = doer(w)

  def map[B](f: R => B): DoWith[W,B] = new DoWith[W,B]({
    (w: W) => {
      val (wp, r) = doer(w)
      (wp, f(r))
    }
  })

  def flatMap[B](f: R => DoWith[W,B]): DoWith[W,B] = new DoWith[W,B]({
    (w: W) => {
      val (wp, r) = doer(w)
      f(r)(wp) // same as f(a).apply(s)
    }
  })
}

// We also define some specialized constructor functions for DoWith.
//
// The doget function constructs a DoWith that makes the "current" state the result.
// Intuitively, it "gets" the state.
//
// The doput function constructs a DoWith that makes the given state w the "current"
// state. Intuitively, it "puts" the given state.
//
// The doreturn function constructs a DoWith that leaves the "current" state as-is
// and returns a given result r.
//
// The domodify function constructs a DoWith that "modifies" the state using the
// given function f.
def doget[W]: DoWith[W, W] = new DoWith[W, W]({ w => (w, w) })
def doput[W](w: W): DoWith[W, Unit] = new DoWith[W, Unit]({ _ => (w, ()) })
def doreturn[W, R](r: R): DoWith[W, R] = new DoWith[W, R]({ w => (w, r) }) /* doget map { _ => r } */
def domodify[W](f: W => W): DoWith[W, Unit] = new DoWith[W, Unit]({ w => (f(w), ()) }) /* doget flatMap { w => doput(f(w)) } */
// Observe that 'seq' with DoWith is the same as with Option or List.

def seq[W,A,B,T](adw: DoWith[W,A], bdw: A => DoWith[W,B], f: (A,B) => T): DoWith[W,T] =
  adw flatMap { a => bdw(a) map { b => f(a,b) } }

/***** Example: An evaluator *****/
// Let's consider the subset of JavaScripty with only *global* variables, numbers,
// number addition, sequencing, and variable assignment.

val prog1 = parse("""
  x = 3
  x = x + 1
  x + 2
""")
// We model memory as a Map from variable names to the number values that they
// store.
type Mem = Map[String, Double]

// Let's first implement a big-step evaluator with "explicit state passing."

def eval1(e: Expr)(m: Mem): (Mem, Double) = e match {
  case N(n) => (m, n)
  case Var(x) => (m, m(x))
  case Binary(Plus, e1, e2) =>
    val (mp, n1) = eval1(e1)(m)
    val (mpp, n2) = eval1(e2)(mp)
    (mpp, n1 + n2)
  case Binary(Seq, e1, e2) =>
    val (mp, _) = eval1(e1)(m)
    val (mpp, n2) = eval1(e2)(mp)
    (mpp, n2)
  case Assign(Var(x), e2) =>
    val (mp, n2) = eval1(e2)(m)
    (mp + (x -> n2), n2)
  case _ => ???
}
eval1(prog1)(Map.empty)
// Good luck debugging if you have a typo between mp and mpp!

// Now, let's implement the evaluator with DoWith.

def eval2(e: Expr): DoWith[Mem,Double] = e match {
  case N(n) => doreturn(n)
  case Var(x) => doget flatMap { m => doreturn(m(x)) }
  case Binary(Plus, e1, e2) =>
    eval2(e1) flatMap { n1 =>
      eval2(e2) map { n2 =>
        n1 + n2
      }
    }
  case Binary(Seq, e1, e2) =>
    eval2(e1) flatMap { _ =>
      eval2(e2) map { n2 =>
        n2
      }
    }
  case Assign(Var(x), e2) =>
    eval2(e2) flatMap { n2 =>
      domodify[Mem] { m => m + (x -> n2)} map { _ =>
        n2
      }
    }
  case _ => ???
}
eval2(prog1)(Map.empty)

// First, observe that the "threading state" scaffolding is gone.

// We use doreturn and domodify to construct DoWith objects that either return
// the "current" state untouched and "modify" state, respectively. Then, the
// computations are "sequenced" using 'flatMap.'
