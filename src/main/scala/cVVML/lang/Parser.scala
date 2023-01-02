package cVVML.lang

import cats.parse.{LocationMap, Parser => P, Parser0 => P0}
import cats.parse.Numbers._
import cats.syntax.all._
import P._
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp

import Syntax._

object Parser:

  /**
    * Main function that parses a string.
    *
    * @param c string representing a program
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c: String): Either[String,Program] = pp(prog,c)


  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A],str:String): Either[String,A] =
    parser.parseAll(str) match { //.fold(e=>prettyError(str,e), x=>x)
      case Left(e) => Left(prettyError(str, e))
      case Right(x) => Right(x)
    }
  /** Prettifies an error message */
  def prettyError(str:String,err:Error): String = {
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match {
      case Some((x, y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-"*(y + 1)) + "^\n"}""".stripMargin
      case _ => ""
    }
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset
    };${err.offsets.toList.mkString(",")}"
  }


  /////////////
  // Parsers //
  /////////////

  def prog: P[Program] =
    (sps.with1 *> method.repSep(sps) <* sps).map(lst =>
      Program(lst.toList.toMap,lst.head._1))


//  def main: String =
//    (string("main")~sp)*>id

  def method : P[(String,Method)] =
    (string("method") *> id.sp ~
//      (char('(') *> unqualPin.repSep0(char(',').sp).sp <* char(')')) ~
//      (char('(') *> unqualPin.repSep0(char(',').sp).sp <* char(')')).sp ~
      (char('{') *> declarations.sp <* char('}'))
      )
      .map(m => m._1 -> Method(m._2.activities -- m._2.forks, m._2.start, m._2.stop,
        m._2.forks,
        m._2.src, m._2.snk, m._2.next, m._2.dataflow, m._2.call))
//      .map(x => x._1._1._1 -> (x._2++
//         Method(Map(), Set(), Set(), Set(), x._1._1._2.toSet, x._1._2.toSet, Map(), Map(), Map())))

  def declarations: P0[Method] =
    declaration.repSep0(sps).map(lst => lst.fold(Method.empty)(_++_))

  def declaration: P[Method] =
    activity | start |stop | fork | arrow

  def activity: P[Method] =
    (string("act") *> varName.sp ~ actDesc.?).map(x => {
      val name: String = x._1
      val (desc,isCall): (String,Boolean) = x._2.getOrElse((name,false))
      Method(Map(name->desc), Set(), Set(), Set(), Set(), Set(), Map(), Map(),
        if isCall then Map(name->desc) else Map())
    })
  def actDesc: P[(String,Boolean)] =
    (char('=')~sps *> (string("call")~sps).? ~ id)
      .map(x => (x._2,x._1.nonEmpty))

  def fork: P[Method] =
    (string("fork") ~ sps) *> varName.map(x =>
      Method(Map(), Set(), Set(), Set(x), Set(), Set(), Map(), Map(), Map()))

  def start: P[Method] =
    (string("start")~sps) *>
      (activity.map(x =>
        Method(x.activities, x.activities.keySet, Set(), Set(), Set(), Set(), Map(), Map(), x.call)) |
       fork.map(x =>
        Method(Map(), x.forks.toSet, Set(), x.forks, Set(), Set(), Map(), Map(), x.call)))
//    ((string("start")~sps) *> varName.sp).map(x =>
//      Method(Map(), Set(x), Set(), Set(), Set(), Set(), Map(), Map(), Map()))

  def stop: P[Method] =
    (string("stop")~sps) *>
      (activity.map(x =>
        Method(x.activities, Set(), x.activities.keySet, Set(), Set(), Set(), Map(), Map(), x.call)) |
      fork.map(x =>
        Method(Map(), Set(), x.forks.toSet, x.forks, Set(), Set(), Map(), Map(), x.call)))
//    ((string("stop") ~ sps) *> varName.sp).map(x =>
//      Method(Map(), Set(), Set(x), Set(), Set(), Set(), Map(), Map(), Map()))

  def arrow: P[Method] =
    // if starts with str, it must be a pin name of the method
    ((str <* sps) ~ ((char(':') ~ sps) *> id).? ~ ((string("=>").sp) *> qualPin))
      .map( x => {
        val pin1 = Pin(None,x._1._1,x._1._2)
        val pin2 = x._2
        Method(Map(), Set(), Set(), Set(),
          Set(pin1), Set(pin2), Map(), Map(pin1->Set(pin2)), Map())
      }) |
    (varName ~ (sps *> (flowCont|dataflowCont))).map( x => x._2(x._1))

  def flowCont: P[String=>Method] =
    ((string("->")~sps) *> varName).map( x => (name =>
      Method(Map(name->name,x->x), Set(), Set(), Set(), Set(), Set(), Map(name->Set(x)), Map(), Map())))

  def dataflowCont: P[String => Method] =
    ((qualPinCont.? <* sps).with1 ~ ((string("=>")~sps) *> qualPin)).map(x => (name =>{
      val pin1 = x._1.getOrElse(y=>Pin(None,y,None))(name)
      val pin2 = x._2
      Method(Map(), Set(), Set(), Set(), Set(pin1), Set(pin2), Map(), Map(pin1->Set(pin2)), Map())
  }))

  def qualPin: P[Pin] =
    ((str <* sps) ~ ((char(':') ~ sps) *> id).?)
      .map(x => Pin(None, x._1, x._2)) |
    ((varName<*sps)~qualPinCont.?).map(x =>
      x._2.getOrElse(y=>Pin(None,y,None))(x._1))

  def qualPinCont: P[String => Pin] =
    ((char('.')~sps) *> unqualPin).map( x => ((act:String) =>
      Pin(Some(act),x.name,x.typ))) |
    (char(':')~sps) *> id
      .map(x => ( (act:String) => Pin(None,act,Some(x))))
//    unqualPin.map( x => ((act:String) => Pin(Some(act),x.name,x.typ)))

  def unqualPin: P[Pin] =
    ((id<*sps)~((char(':')~sps) *> id).?)
      .map(x => Pin(None,x._1,x._2))




  ///////////////////
  // base parsers ///
  ///////////////////


  /** Parser for a sequence of spaces or comments */
  //// whitespaces
  val whitespace: P[Unit] = //P.charIn(" \t\r\n").void
                            charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace | comment).rep0.void

  extension[A] (parser:P[A])
    def sp = parser.surroundedBy(sps)
  extension[A] (parser:P0[A])
    def sp = parser.surroundedBy(sps)

  /** letters and digits and _ */
  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  /** variable name (starting with lower cap)  */
  def varName: P[String] =
    ((charIn('a' to 'z')|charIn('A' to 'Z')) ~ alphaDigit.rep0).string
  /** symbols */
//  def symbols: P[String] = // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as sybmols of terms
//    P.not(string("--")).with1 *>
//      oneOf("+-><!%/*=|&".toList.map(char)).rep.string
  def symbols: P[Char] =
    oneOf("+-><!%/*=|&".toList.map(c=>charIn(c)))

  def str:P[String] =
    char('"') *> (alphaDigit|char(' ')|symbols).rep0.string <* char('"')
  def id:P[String] =
    str | varName

  /** real number, e.g., 12 or 34.2 */
  def realP: P[Double] =
    (digits ~ (charIn('.')*>digits.map("."+_)).?)
      .map(x=>(x._1+x._2.getOrElse("")).toDouble)
  /** positive integer */
  def intP: P[Int] = digits.map(_.toInt)

  /** (Recursive) Parser for an linear expression */
  







