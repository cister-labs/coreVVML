package cVVML.backend

import SeqSOS.State
import cVVML.lang.Syntax.Program

object WellBehaved :
  def justControl(p:Program): Either[List[String],String] = try
    val (nodes, edges, completed) = caos.sos.SOS.traverse(SeqSOS, SeqSOS.initial(p))

    var res = List[String]()
    val reached =
      for State(_, as, _, _) <- nodes
          (ma, _) <- as
      yield ma
    val unused =
      for (name, m) <- p.ms.toSet
          (a,_) <- m.activities
          if !reached.contains(name -> a)
      yield s"\"$name\"/\"$a\""
    if !completed then res ::= s"Timeout after traversing ${nodes.size} states and $edges edges."
    if unused.nonEmpty then res ::= s"Unreachable activities ${unused.mkString(", ")}."
    if res.nonEmpty
    then Left(res)
    else Right(s"Ok (${nodes.size} states)")

  catch
    case e:Throwable => Left(e.getMessage.split("\n").toList)

  def checkPP(p:Program) =
    justControl(p).fold(_.mkString("\n"),x=>x)





