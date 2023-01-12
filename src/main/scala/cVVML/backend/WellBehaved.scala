package cVVML.backend

import SeqSOS.State
import cVVML.lang.Syntax.Program

object WellBehaved :
  def justControl(p:Program): Either[List[String],String] = try
    val (nodes, edges, completed) = traverse(SeqSOS.initial(p), max=2000)

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

  /** adaptation from SOS.traverse to check if it is stuck. */
  private def traverse(s:State, max:Int=5000): (Set[State],Int,Boolean) =
    def aux(next:Set[State],done:Set[State],edges:Int, limit:Int): (Set[State],Int,Boolean) =
      if limit <=0 then
        return (done,edges,false)
      next.headOption match
        case None =>
          (done, edges, true)
        case Some(st) if done contains st =>
          aux(next-st,done,edges,limit)
        case Some(st) => //visiting new state
          val more = SeqSOS.next(st)
          if more.isEmpty && !SeqSOS.accepting(st) then
            sys.error(s"Suck at {${(st.as.keySet++st.fs.keySet).map(x=>s"\"${x._1}/${x._2}\"").mkString(",")}} and cannot stop.")
          aux((next-st)++more.map(_._2), done+st, edges+more.size,limit-1)

    aux(Set(s), Set(), 0, max)






