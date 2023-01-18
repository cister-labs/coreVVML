package cVVML.backend

import SeqSOS.State
import cVVML.lang.Syntax.{Program,Method}

object WellBehaved :
  def justControl(p:Program): Either[List[String],String] = try
    val (nodes, edges, completed) = traverse(SeqSOS.initial(p), max=2000)
    if !completed then
      return Left(List(s"Timeout after traversing ${nodes.size} states and $edges edges."))

    var res = List[String]()
    val reached =
      for State(_, as, _, _, _) <- nodes
          (ma, _) <- as
      yield ma
    val unused =
      for (name, m) <- p.ms.toSet
          (a,_) <- m.activities
          if !reached.contains(name -> a)
      yield s"\"$name\"/\"$a\""
    if unused.nonEmpty  then res ::= s"Unreachable activities ${unused.mkString(", ")}."

    try {
      // run reverse program to check if some states cannot reach the end
      val (nodesR, edgesR, completedR) = traverse(SeqSOS.initial(reverse(p)))

      var resR = List[String]()
      val reachedR =
        for State(_, as, _, _, _) <- nodesR
            (ma, _) <- as
        yield ma
      val unusedR =
        for (name, m) <- p.ms.toSet
            (a, _) <- m.activities
            if !reachedR.contains(name -> a)
        yield s"\"$name\"/\"$a\""
      if unusedR.nonEmpty then res ::= s"Activities can never stop ${unusedR.mkString(", ")}."
    } catch
      case _:Throwable =>

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

  // reversed seq arrows
  private def reverse(p:Program): Program =
    Program(p.ms.map(kv=>kv._1->reverse(kv._2)), p.main)
  private def reverse(m:Method): Method =
    Method(
      m.activities,
      m.stop, m.start, // swapped
      m.forks,
      m.src, m.snk,
      reverse(m.next),
      reverse(m.dataflow),
      m.call, m.nodeLbl, m.edgeLbl
    )
  private def reverse[A](m:Map[A,Set[A]]): Map[A,Set[A]] =
    var res = Map[A,Set[A]]().withDefaultValue(Set())
    for (x,ys)<-m; y<-ys do
      res += y -> (res(y)+x)
    res






