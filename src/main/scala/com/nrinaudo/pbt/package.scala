package com.nrinaudo

import org.scalacheck.{Prop, Test, Arbitrary, Gen, Cogen}

package object pbt {

  // Hack to make sure that my examples fail with high enough, messy enough ints
  implicit val arbInt: Arbitrary[Int] =
    Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue).filter { i =>  math.abs(i) > 100000 })

  // ScalaCheck's functions have a horrible toString implementation.
  // Note that I considered using the Pretty mechanism provided by ScalaCheck, but that forced it to crop up in the
  // slides, which I want to avoid - it's a weird implementation detail I don't want to talk about.
  implicit def arbFun[A: Cogen, C: Arbitrary]: Arbitrary[A => C] = Arbitrary(
    Gen.function1[A, C](Arbitrary.arbitrary[C]).map { f => new (A => C) {
      override def apply(a: A) = f(a)
      override def toString = "<function>"
    }}
  )

  def toShortString(a: Any): String = {
    val str = a.toString

    if(str.length > 50) str.substring(0, 50) + "…"
    else str
  }

  // Checks a property with:
  // - more HTML friendly output
  // - no "original argument" display, which would break everything when called on a total :->.
  // - a controlled seed
  def validate(prop: Prop): Unit = {

    val result = Test.check(Test.Parameters.default.withInitialSeed(18237L), prop)

    result.status match {
      case Test.Failed(args, _) =>
        println("✖ Failed with:")
        args
          .map(arg => toShortString(arg.arg))
          .zipWithIndex
          .foreach { case (arg, i) => println(s" - ARG_$i: $arg") }

      case Test.Passed =>
        println("✔ Passed")

      case s =>
        sys.error(s"Unexpected test status: $s")
    }
  }
}
