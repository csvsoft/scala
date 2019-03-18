package com.test

import fastparse.NoWhitespace._
import fastparse._
import org.scalatest.{FunSuite, Matchers}

class fastparse extends FunSuite with Matchers {

  test("basic") {
    def parseA[_: P] = P("a")

    val Parsed.Success(value, successIndex) = parse("a", parseA(_))
    assert(value == (), successIndex == 1)

    val f@Parsed.Failure(label, index, extra) = parse("b", parseA(_))
    assert(
      label == "",
      index == 0)

  }

  test("Either") {
    def either[_: P] = P("a" | "b")

    val Parsed.Success(_, 1) = parse("a", either(_))
  }

  /**
    * The parsing will end early if parses successfully, Success#index stores the last index consumed
    */
  test("sequence parsing") {
    def ab[_: P] = P("a" ~ "b") // b follows a
    val Parsed.Success(_, index) = parse("abc", ab(_))
    index shouldBe 2
  }

  /**
    * The End parser force the parser to consume the whole input.
    */
  test("Start End") {
    def withEnd[_: P] = P("a".rep ~ "b" ~ End)

    def withStart[_: P] = P(Start ~ "a" ~ "c")

    val Parsed.Failure(_, 4, _) = parse("aaaba", withEnd(_))
    val Parsed.Success(_, 2) = parse("ac", withStart(_))
  }

  test("Pass- always succeed,Fail always fails without consuming any input") {
    val Parsed.Success((), 0) = parse("asdad", Pass(_))
    val Parsed.Failure(_, 0, _) = parse("asdad", Fail(_))
  }

  /**
    * Index -- provides the current index of the parse into the input string
    */
  test("Index") {
    def finder[_: P] = P("hay".rep ~ Index ~ "needle" ~ "hay".rep)

    val Parsed.Success(9, _) = parse("hayhayhayneedlehay", finder(_))
    val Parsed.Success(index, _) = parse("needlehay", finder(_))
    index shouldBe (0)
  }

  test("Capture with .! --") {
    def capture1[_: P] = P("a".rep.! ~ "b" ~ End)

    val Parsed.Success(value, 4) = parse("aaab", capture1(_))
    value shouldBe ("aaa")

    // capture multiple strings
    def capture3[_: P] = P("a".rep.! ~ "b".! ~ "c".! ~ End)

    val Parsed.Success(("aaa", "b", "c"), 5) = parse("aaabc", capture3(_))

    // Capture sequence with  .!.rep
    def captureRep[_: P] = P("a".!.rep ~ "b" ~ End)

    val Parsed.Success(Seq("a", "a", "a"), 4) = parse("aaab", captureRep(_))

    // Capture option with .!.?
    def captureOpt[_: P] = P("a" ~ "b".!.? ~ "c")

    val Parsed.Success(valueOpt, _) = parse("ac", captureOpt(_))
    valueOpt shouldBe (None)

    // Captured some
    val Parsed.Success(valueSome, _) = parse("abc", captureOpt(_))
    valueSome shouldBe Some("b")


  }

  test("AnyChar") {
    def ab[_: P] = P("'" ~ AnyChar.! ~ "'")

    val Parsed.Success(value, 3) = parse("'-'", ab(_))
    value shouldBe ("-")
  }

  /**
    * &(...) operator wraps a parser, only succeeds if it succeeds, but consumes no input
    */
  test("Postive LookAhead ") {
    def hello[_: P] = P(("hello" ~ &(" ")).!.rep)

    val Parsed.Success(Seq("hello"), _) = parse("hello ", hello(_))
  }

  /**
    * !... operator wraps a parser and only succeeds if it fails
    */
  test("Negative LookAhead") {
    def keyword[_: P] = P("hello" ~ !" " ~ AnyChar ~ "world") // hello then followed by anychar except space, then followed by world
    val Parsed.Failure(_, 5, _) = parse("hello world", keyword(_))
  }


  test("map") {
    def binary[_: P] = P(("0" | "1").rep.!)

    def binaryNum[_: P] = P(binary.map(Integer.parseInt(_, 2)))

    val Parsed.Success("1100", _) = parse("1100", binary(_))
    val Parsed.Success(12, _) = parse("1100", binaryNum(_))

    val Parsed.Failure(failureString, index, extra) = parse("45", binaryNum(_))
    failureString shouldBe ("")


  }

  test("flatMap") {
    def leftFlag[_: P] = P("<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")

    def rightFlag[_: P](s: String) = P("</" ~ s.! ~ ">")

    def xml[_: P] = P(
      for {
        left <- leftFlag
        right <- rightFlag(left)
      } yield right
    )

    val Parsed.Success("a", _) = parse("<a></a>", xml(_))
  }


  test("filter") {
    def digits[_: P] = P(CharPred(c => '0' <= c && c <= '9').rep(1).!).map(_.toInt)

    def even[_: P] = P(digits.filter(_ % 2 == 0))

    val Parsed.Success(12, _) = parse("12", even(_))
    val failure = parse("13", even(_)).asInstanceOf[Parsed.Failure]

  }

  test("opaque -- Hide parser's implementation details and provide a higher-level error message") {

    def digit[_: P] = CharIn("0-9")

    def letter[_: P] = CharIn("A-Z")

    def twice[T, _: P](p: => P[T]) = p ~ p

    def numberPlate[_: P] = P(twice(digit) ~ "-" ~ twice(letter))

    // Suppress implementation details from the error message with opaque
    def opaqueNumberPlate[_: P] = numberPlate.opaque("<number-plate>")


    def errorMessage[T](p: P[_] => P[T], str: String) = parse(str, p) match {
      case f: Parsed.Failure => f.trace().longAggregateMsg
      case _ => "OK"
    }

    val err = errorMessage(opaqueNumberPlate(_), "11-A3-22")
    assert(err == """Expected <number-plate>:1:1, found "11-A3-22"""")

    val ok = errorMessage(opaqueNumberPlate(_), "11-AB-22")
    assert(ok == "OK")
  }


  test("log") {


    def f[_: P] = P("C").log

    def fool[_: P] = P((f | "A") ~ "B".!).log

    val result = parse("AB", fool(_))


  }

  /**
    * CharPred -- Takes a Char => Boolean predicate and creates a parser that parses any single character that satisfies that predicate.
    */
  test("CharPred"){
    def cp[_:P] = P(CharPred(_.isUpper).rep.! ~ "." ~ End)
    val Parsed.Success("ABC",_) = parse("ABC.",cp(_))

  }

  /**
    * literal strings containing regex-style character range 'a-z','0-9', - is reserved char
    */
  test("CharIn"){
    def ci[_:P] = P (CharIn("a-z").rep.! ~ End)
    val Parsed.Success("abc",_) = parse("abc",ci(_))

    def plusminus[_:P] = P (CharIn("""+\-""").rep.! ~ End)
    val Parsed.Success("+-",_) = parse("+-",plusminus(_))

  }

  /**
    * CharsWhileIn continues consuming characters as long as they are within the set you characters you passed to it. Like CharIn, this parser takes in literal strings representing regex-style character ranges that it accepts
    */
  test("CharsWhileIn"){
    def cwi[_:P] = P (CharsWhileIn("0-9").! )
    val Parsed.Success("123",_) = parse("123",cwi(_))
  }


  /**
    * CharsWhile -- this parser continually chomps away at characters as long as they continue passes the given predicate.
    */
  test("CharsWhile"){
    def cw[_:P] = P (CharsWhile(!_.isSpaceChar).rep.! ~ End)
    val Parsed.Success("+-",_) = parse("+-",cw(_))
  }

  /**
    * one of any number of strings that you give it
    */
  test("StringIn/StringInIgnoreCase "){
    def stringIn[_:P] = P (StringIn("cow","cattle").!.rep(1))
    val Parsed.Success(Seq("cow"),_) = parse("cow",stringIn(_))
    val Parsed.Success(Seq("cow"),_) = parse("cowmoo",stringIn(_))
    val Parsed.Failure(_,_,_) = parse("cwc",stringIn(_))
  }

  /**
    * A "cut" (a ~/ b or a./ in Fastparse) is a marker in a recursive-descent parser that states "you cannot backtrack past this point". In FastParse
    */

  test("Cuts ~/"){
    def alpha[_:P] = P(CharIn("a-zA-Z"))
    def varToken[_:P] = P(alpha.rep(1))
    def cut[_:P] = P("val " ~/ varToken.! | "def " ~/ varToken.!)

    val Parsed.Success("name",_) = parse("val name",cut(_))
    val result = parse("val 123",cut(_))
    val resultMsg = result match{
      case f:Parsed.Failure =>f.trace().longAggregateMsg
      case _ => "OK"
    }
    resultMsg shouldBe("""Expected cut:1:1 / varToken:1:5 / alpha:1:5 / [a-zA-Z]:1:5, found "123"""")


  }






}
