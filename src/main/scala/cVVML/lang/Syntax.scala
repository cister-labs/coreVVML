package cVVML.lang

object Syntax:
  case class Program(ms:Map[String,Method], main:String)

  case class Method(activities:Map[Activity,String], // all activities and descriptions
                    start: Set[Activity|Fork], // initial activities
                    stop: Set[Activity|Fork],  // terminal activities
                    forks:Set[Fork], // all forks/merges
                    src: Set[Pin], // source pins of activities and the method
                    snk: Set[Pin], // sink pins of activities and the method
                    next: Map[Activity|Fork, Set[Activity|Fork]], // sequence arrow
                    dataflow: Map[Pin, Set[Pin]], // artifact arrow
                    call: Map[Activity,MethodName]): // call behaviour of an activity
    /** all input pins of an activity or the method */
    def inputs(a:Option[Activity]): Set[Pin] =
      src.filter(_.act==a)
    /** all output pins of an activity or the method */
    def outputs(a: Option[Activity]): Set[Pin] =
      snk.filter(_.act==a)
    /** Join of two methods */
    def ++(m2:Method): Method =
      Method(relStrJoin(activities,m2.activities),
        start++m2.start,       stop++m2.stop,
        forks++m2.forks,
        src++m2.src,           snk++m2.snk,
        relJoin(next,m2.next), relJoin(dataflow,m2.dataflow),
        call++m2.call
      )
    /** Get an activity's name */
    def apply(a:Activity) =
      activities.getOrElse(a,a)
    /** checks if an activity has decisions */
    def isDecisionAct(a: Activity): Boolean =
//      println(s"${m(a)} -> ${m.next.get(a)} is decision? ${m.next.getOrElse(a, Set()).size > 1}")
      (next.getOrElse(a, Set()).size + stop(a).compareTo(false)) > 1


    private def relStrJoin[A](m1:Map[A,String],m2:Map[A,String]):Map[A,String] =
      val upd = for (a,s)<-m2 if s!=a || !m1.contains(a) yield (a,s)
      m1 ++ upd
    private def relJoin[A,B](m1:Map[A,Set[B]],m2:Map[A,Set[B]]):Map[A,Set[B]] =
      m2.foldLeft(m1)(relJoin)
    private def relJoin[A,B](m:Map[A,Set[B]],x:(A,Set[B])):Map[A,Set[B]] =
      m.get(x._1) match
        case Some(bs) => m+(x._1 -> (x._2++bs))
        case None => m + x
    def noData =
      Method(activities,start,stop,forks,src,snk,next,Map(),call)
    def noFlow =
      Method(activities,Set(),Set(),forks,src,snk,Map(),dataflow,call)

  object Method:
    val empty: Method =
      Method(Map(), Set(), Set(), Set(), Set(), Set(), Map(), Map(), Map())


  type Activity = String
  type Fork = String
  type MethodName = String
  case class Pin(act: Option[Activity], name: String, typ: Option[String]):
    override def toString: String =
      s"${if act.nonEmpty then act.get+"." else ""}$name${
          if typ.nonEmpty then s":${typ.get}" else ""}"
    def pp: String = Pin(None,name,typ).toString


  /// EXAMPLES ///
  object Examples:
    val x3i = Pin(Some("x3"),"x3i",Some("Int"))
    val x3o = Pin(Some("x3"),"x3o",None)
    val x4i = Pin(Some("x4"),"x4i",None)
    val min = Pin(None,"min",Some("Int"))
    val mout = Pin(None,"mout",Some("Int"))

    val ex1 = Method(
      activities = Map("x3"->"x3 activity","x4"->"x4 activity"),
      start = Set("1"),
      stop = Set("2"),
      forks = Set("1","2"), // forks -- needed to paint them differently(?)
      src = Set(min, x3o),
      snk = Set(mout, x3i, x4i),
      next = Map("1"->Set("x3","x4"), "x3"->Set("2"), "x4"->Set("2")), // next
      dataflow = Map(min->Set(x3i), x3o->Set(mout,x4i)),
      call = Map()
    )

