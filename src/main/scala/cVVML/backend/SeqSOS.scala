package cVVML.backend

import cVVML.lang.Syntax.{Activity, Program, Fork}
import SeqSOS.State

object SeqSOS extends caos.sos.SOS[String,State]:
  case class State(p:Program,as:ActStates,fs:ForkStates)
  type ActStates = Map[(String,Activity),ActState]
  type ForkStates = Map[(String,Fork),Int] // multiset of forks

  def pp(s:State) =
    s.as.mkString(",")+" / "+s.fs.mkString(",")

  enum ActState:
    case Ready
    case Run
    case Done
  import ActState._

  def initial(p:Program):State =
    val m = p.ms(p.main)
    State(p,
      (for a<-m.start if m.activities.contains(a) yield (p.main,a)->Ready).toMap,
      (for f<-m.start if m.forks.contains(f)      yield (p.main,f)->1).toMap,
    )

  def next[A>:String](s: State): Set[(A, State)] =
    var res = Set[(A,State)]()

    // start
    for (a,Ready)<-s.as do
      res += s"start-$a" -> State(s.p,s.as+(a->Run),s.fs)

    // stop
    for (a, Run) <- s.as
        if !s.p.ms(a._1).call.contains(a._2)
    do
      val m = s.p.ms(a._1)
      val nxts = s.p.ms(a._1).next.getOrElse(a._2,Set())
      val drop = nxts.map(a._1->_)+a
      if m.stop(a._2) then
        val dropped = s.as--drop
        if dropped.nonEmpty then
          sys.error(s"Stopping with running activities (${dropped.mkString(",")}).")
        res += s"stop-${a._1}/${a._2}" ->
               State(s.p, s.as -- drop, s.fs)

    // end+push
    for (a, Run) <- s.as
        if !s.p.ms(a._1).call.contains(a._2)
        nxts <- s.p.ms(a._1).next.get(a._2) // Option[Set]
        nxt <- nxts
    do
      val m = s.p.ms(a._1)
      val drop = nxts.map(a._1->_)+a
      if m.activities.contains(nxt) // if nxt is an activity
      then (
        if s.as.contains(a._1->nxt) then
          sys.error(s"Trying to enter \"${a._1}/$nxt\" but state was not idle (${s.as.mkString(",")})")
        else
          res += s"ready-${a._1}/${nxt}" ->
                 State(s.p, (s.as -- drop) + ((a._1->nxt)->Ready), s.fs))
      else ( // nxt is a fork
        if s.as.contains(a._1 -> nxt) then
          sys.error(s"Trying to enter \"${a._1}/$nxt\" but state was not idle (${s.as.mkString(",")})")
        else
          val x = a._1->nxt
          res += s"ready-${a._1}/${nxt}" ->
                 State(s.p, (s.as -- drop), s.fs + (x->(s.fs.getOrElse(x,0)+1))))

    // call - later
    // sync fork
    for ((mn,f), nf) <- s.fs
        if nf== (s.p.ms(mn).next.filter(_._2.contains(f)).size + s.p.ms(mn).start(f).compareTo(false))
    do
      val m = s.p.ms(mn)
      val newReady: Set[((String,Activity),ActState)] = for
          nxt <- m.next.getOrElse(f,Set()) // nxt is a fork or an activity
          if m.activities.contains(nxt) // nxt must be an activity
      yield (mn, nxt) -> Ready
      val newForks: Set[((String,Fork),Int)] = for
          nxt <- m.next.getOrElse(f,Set()) // nxt is a fork or an activity
          if m.forks(nxt) // nxt must be a fork
      yield (mn, nxt) -> (s.fs.getOrElse(f->nxt, 0)+1)
      for (nr,_) <- newReady if s.as.contains(nr) do
        sys.error(s"Trying to enter $nr but state was not idle (${s.as.mkString(",")})")
      res += s"sync-$mn/$f" -> State(s.p, s.as ++ newReady, (s.fs-(mn->f)) ++ newForks)


    res
