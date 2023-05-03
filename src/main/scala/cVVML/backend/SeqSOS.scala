package cVVML.backend

import cVVML.lang.Syntax.{Activity, Program, Fork}
import SeqSOS.State

object SeqSOS extends caos.sos.SOS[String,State]:
  case class State(p:Program,as:ActStates,fs:ForkStates,ret:Option[State],starting:Boolean):
    override def toString: String = pp(this)

  type ActStates = Map[(String,Activity),ActState]
  type ForkStates = Map[(String,Fork),Int] // multiset of forks

  /** Pretty print a state */
  def pp(s:State): String =
    (s.as.map(ppMap) ++ s.fs.map(ppMap)).mkString(", ") +
      (if s.starting then s"Starting ${s.p.main}" else "")

  private def ppMap[A,B,C](l:((A,B),C)): String =
    s"${ppPair(l._1)}->${l._2}"

  private def ppPair[A, B](l: (A, B)): String =
    s"${l._1}/${l._2}"


  enum ActState:
    case Ready
    case Run
    case Done
  import ActState._

  def initial(p:Program):State =
    val m = p.ms(p.main)
    State(p,
//      (for a<-m.start if m.activities.contains(a) yield (p.main,a)->Ready).toMap,
//      (for f<-m.start if m.forks.contains(f)      yield (p.main,f)->1).toMap,
      Map(),Map(),
      None,
      true
    )


  private def startCase(s:State): Set[(String,State)] =
    if !s.starting then return Set()

    val mname = s.p.main
    val m = s.p.ms(mname)
    for nxt <- m.start
    yield
      val dropA = for other<-m.start-nxt if m.activities.contains(other)
                  yield mname->other.asInstanceOf[Activity]
      val dropF = for other <- m.start-nxt if m.forks.contains(other)
                  yield mname->other.asInstanceOf[Fork]
      val newAct = if m.activities.contains(nxt)
                   then (s.as--dropA)+((mname->nxt)->Ready)
                   else (s.as--dropA)
      val newFork = if m.forks.contains(nxt)
                    then (s.fs--dropF)+((mname->nxt)->1)
                    else (s.fs--dropF)
      s"allow-${mname}/$nxt" ->
        State(s.p, newAct, newFork, s.ret, false)


  private def runCase(s:State): Set[(String,State)] =
    for (a,Ready)<-s.as.toSet yield
      s"run-${ppPair(a)}" -> State(s.p,s.as+(a->Run),s.fs, s.ret,s.starting)

  private def stopCase(s:State): Set[(String,State)] =
    var res = Set[(String,State)]()
    for ((mn,a), aSt) <- s.as
        // if activity "a" is ready to stop and...
        if s.p.ms(mn).stop(a) &&
        // ... it is ready to end its execution
           ((aSt==Run && !s.p.ms(mn).call.contains(a)) ||
            aSt==Done)
    do
      val m = s.p.ms(mn)
      val nxts = m.next.getOrElse(a,Set())
      val drop = nxts.map(s => mn->s)+(mn->a)
      val dropped = s.as--drop
      if dropped.nonEmpty then
        sys.error(s"Stopping with running activities (${dropped.mkString(",")}).")
      val nextSt = s.ret.getOrElse(State(s.p, s.as -- drop, s.fs, None, s.starting))
      res += s"stop-${ppPair(mn->a)}" -> nextSt
    res

//  private def stuckCase(s:State): Unit =
//    // first for activities
//    for ((mn,a), aSt) <- s.as do
//      val m = s.p.ms(mn)
//      val nxts = m.next.getOrElse(a,Map())
//      lazy val running = ((for ((`mn`,x),_) <- s.as yield x) ++
//                          (for ((`mn`,x),_) <- s.fs yield x)).toSet - a
//      println(s"checking if act $mn/$a is stuck. ${m.next.getOrElse(a, Map())} / ${running.mkString(",")}")
//      if nxts.isEmpty && running.nonEmpty then
//        sys.error(s"Stuck at \"$a\" before stopping with pending activities {${running.mkString(",")}} [@$mn]")
//
//    // then for forks/mergers
//    for ((mn, f), nf) <- s.fs do
//      val m = s.p.ms(mn)
//      lazy val running = ((for ((`mn`,x),_) <- s.as yield x) ++
//                          (for ((`mn`,x),_) <- s.fs yield x)).toSet - f
//      println(s"checking if fork $mn/$f is stuck. ${m.next.getOrElse(f, Map())} / ${running.mkString(",")}")
//      if m.next.getOrElse(f, Map()).isEmpty && running.nonEmpty then
//        sys.error(s"Stuck at \"$f\" before stopping with pending {${running.mkString(",")}} [@${mn}]")


  private def endCase(s:State): Map[String,State] =
    for (a, aSt) <- s.as
        // if activity "a" is ready to finish running
        if (aSt==Run && !s.p.ms(a._1).call.contains(a._2)) ||
          aSt==Done
        nxts <- s.p.ms(a._1).next.get(a._2).toSet // Option[Set]
        nxt <- nxts // Set[Act|Fork]
    yield
      val m = s.p.ms(a._1)
      val qnxt = a._1 -> nxt // qualified next action
      val drop = nxts.map(s => a._1 -> s) + a
      if m.activities.contains(nxt) // if nxt is an activity
      then (
        if s.as.contains(qnxt) then
          sys.error(s"Trying to enter \"${a._1}/${m(nxt)}\" but state was not idle (${s.as.mkString(",")})")
        else
          s"allow-${ppPair(qnxt)}" ->
            State(s.p, (s.as -- drop) + (qnxt -> Ready), s.fs, s.ret, s.starting))
      else (// nxt is a fork
        if s.as.contains(a._1 -> nxt) then
          sys.error(s"Trying to enter \"${a._1}/${m(nxt)}\" but state was not idle (${s.as.mkString(",")})")
        else
          val x = a._1 -> nxt
          s"allow-${ppPair(qnxt)}" ->
            State(s.p, (s.as -- drop), s.fs + (x -> (s.fs.getOrElse(x, 0) + 1)), s.ret, s.starting))

  private def forkCase(s:State) =
    for ((mn,f), nf) <- s.fs
        if nf == // if f is ready to be fired
            (s.p.ms(mn).next.filter(_._2.contains(f)).size +
             s.p.ms(mn).start(f).compareTo(false))
    yield
      val m = s.p.ms(mn)
      val qf = mn->f // qualified f
      val newReady: Set[((String,Activity),ActState)] = for
          nxt <- m.next.getOrElse(f,Set()) // nxt is a fork or an activity
          if m.activities.contains(nxt) // nxt must be an activity
      yield (mn, nxt) -> Ready
      val newForks: Set[((String,Fork),Int)] = for
          nxt <- m.next.getOrElse(f,Set()) // nxt is a fork or an activity
          if m.forks(nxt) // nxt must be a fork
      yield (mn, nxt) -> (s.fs.getOrElse(f->nxt, 0)+1)
      for (nr,_) <- newReady if s.as.contains(nr) do
        sys.error(s"Trying to enter $nr but state was not idle (${
          s.as.map(ppMap).mkString(",")})") //improve message
      val newState = State(s.p, s.as ++ newReady, (s.fs-qf) ++ newForks, s.ret, s.starting)
      if m.stop(f)
      then s"sync-stop-${ppPair(qf)}" -> s.ret.getOrElse(newState)
      else s"sync-${ppPair(qf)}" -> newState

  private def callCase(s:State) =
    for (a, Run) <- s.as
        mname <- s.p.ms(a._1).call.get(a._2) // if it's a call behaviour
        meth <- s.p.ms.get(mname)
    yield
      val st2 = initial(Program(s.p.ms,mname))
      val cont = Some(State(s.p,s.as+(a->Done),s.fs,s.ret, s.starting))
      s"call-$mname" -> State(st2.p,st2.as,st2.fs,cont,st2.starting)


  /** Calculates the possible next states,
   * potentially raising the following exceptions:
   *  (1) stopping with running avtivities;
   *  (2) entering a non-idle activity.
   */
  def next[A>:String](s: State): Set[(A, State)] =
    // stuck without being last
//    stuckCase(s)
    // start
    startCase(s) ++
    // run
    runCase(s) ++
    // stop
    stopCase(s) ++
    // end+push
    endCase(s) ++
    // sync fork
    forkCase(s) ++
    // call behaviour
    callCase(s)

  override def accepting(s:State): Boolean =
    s.as.isEmpty && s.fs.isEmpty && !s.starting