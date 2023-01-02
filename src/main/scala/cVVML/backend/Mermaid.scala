package cVVML.backend

import cVVML.lang.Syntax._
import cVVML.backend.SeqSOS.State
object Mermaid:

  def justMethod(m:Method): String =
    apply(Program(Map("method"->m),"method"))

  def fixMeth(str:String) =
    str.replaceAll(" ","_")

  def apply(st:State): String =
    val s = State(Program(st.p.ms.map(kv=>kv._1->kv._2.noData),st.p.main),st.as,st.fs,st.ret)
    def getForkSt(m:String,f:Fork,n:Int) =
      val pred = for
        meth <- s.p.ms.get(m).toSet
        (a,dest) <- meth.next
        if dest.contains(f)
      yield a
      val need = pred.size
      println(s"Need: $need (${pred.mkString(",")}), has: $n")
      if n>=need then "Ready"
      else "NotReady"
    s"""${apply(s.p)}
       |classDef Ready fill:#bbf,stroke-width:3pt,stroke:#228;
       |classDef NotReady fill:#bbf,stroke-width:3pt,stroke:#88b;
       |classDef Run fill:#fc8,stroke-width:4pt,stroke:#964;
       |classDef Done fill:#f88,stroke-width:4pt,stroke:#700;
       |${(for ((m,a)->aSt) <- s.as yield s"${fixMeth(m)}_$a:::$aSt").mkString("\n")}
       |${(for ((m,f)->n) <- s.fs yield s"${fixMeth(m)}_$f:::${getForkSt(m,f,n)}").mkString("\n")}
       |""".stripMargin

  def apply(p:Program): String =
    s"""graph TD
       |${p.ms.map(x=>apply(x._1,x._2)).mkString("\n\n")}
       |${fixMeth(p.main)}:::mn
       |
       |classDef in fill:#2f2,stroke:#ccc;
       |classDef st fill:#000,stroke:#f22,stroke-width:4px;
       |classDef tr fill:#888,stroke-width:0pt;
       |classDef gw fill:#8f8,stroke-width:1pt,stroke:#000;
       |classDef it fill:#f88,stroke-width:1pt,stroke:#822;
       |classDef cl fill:#dff,stroke-width:1pt,stroke:#499;
       |classDef df fill:#ffd,stroke-width:1pt,stroke:#994;
       |classDef mn fill:#ffb,stroke-width:2pt,stroke:#882;""".stripMargin

  def apply(name:String, m: Method): String =
    s"""  subgraph ${name.replaceAll(" ","_")} [$name]
       |${apply(qualify(name,m),name)}
       |""".stripMargin

  private def qualify(name:String,m:Method): Method =
    def upd(s:String) = s"${fixMeth(name)}_$s"
    def updp(p:Pin) = Pin(p.act.map(upd),p.name,p.typ)
    Method(m.activities.map(x=>(upd(x._1)->x._2)),
      m.start.map(upd),m.stop.map(upd),m.forks.map(upd),
      m.src.map(updp),m.snk.map(updp),
      m.next.map(x=>upd(x._1)->x._2.map(upd)),
      m.dataflow.map(x=>updp(x._1)->x._2.map(updp)),
      m.call.map(x=>upd(x._1)->x._2))

  def apply(m: Method, name: String): String =
    val c = new Cache
    import c._
    val nm = name.replaceAll(" ","_")

    def getPin(pin:Pin): String = pin.act match
      case Some(a) => a
      case None => s"p${get(pin.toString)}_$nm[$pin]:::it"

    val s = "    "
    s"""${s}Init_$nm(( )):::in
      |${s}Stop_$nm(( )):::st
      |${s}${(for a<-m.activities
             yield if m.next.getOrElse(a._1,Set()).size +
                      m.stop(a._1).compareTo(false) > 1
               then s"${a._1}{{${a._2}}}:::gw"
             else if m.call.contains(a._1)
               then s"${a._1}([${a._2}]):::cl"
               else s"${a._1}([${a._2}]):::df"
            ).mkString(s"\n$s")}
      |${s}${m.forks.map(x=>s"$x[   ]:::tr").mkString(s"\n$s")}
      |${s}${m.start.map(x=>s"Init_$nm --> $x").mkString(s"\n$s")}
      |${s}${m.stop.map(x=>s"$x --> Stop_$nm").mkString(s"\n$s")}
      |${s}${m.next.map(x=>
          s"${x._1} --> ${x._2.mkString(" & ")}").mkString(s"\n$s")}
      |${s}${(for pin1<-m.src; pin2<-m.dataflow.getOrElse(pin1,Set())
                  if pin1.act.nonEmpty && pin2.act.nonEmpty yield
        s"${pin1.act.get} -.\"${pin1.pp}->${pin2.pp}\".-> ${pin2.act.get}"
      ).mkString(s"\n$s")
      }
      |${s.drop(2)}end
      |${(for pin1<-m.src; pin2<-m.dataflow.getOrElse(pin1,Set())
                  if pin1.act.isEmpty || pin2.act.isEmpty yield
            s"${getPin(pin1)} -.\"${pin1.pp}->${pin2.pp}\".-> ${{getPin(pin2)}}"
      ).mkString(s"\n")
      }
      |""".stripMargin

//${s}${(for (from,tos)<-m.dataflow; to<-tos; a1<-m.src(from); a2<-m.snk(to) yield
//  s"$a1 -.\"${from}->${to}\".-> $a2"
//  ).mkString(s"\n$s")
//${(for (from,tos)<-m.dataflow; to<-tos; a1<-m.src.get(from); a2<-m.snk.get(to)
//       if a1.isEmpty || a2.isEmpty yield
//  s"${getPin(from,a1)} -.\"${from}->${to}\".-> ${{getPin(to,a2)}}"
//  ).mkString(s"\n")


  class Cache:
    var seed = 0
    var cache = Map[String, Int]()

    def get(x: String) = cache.get(x) match
      case Some(v) => v
      case None =>
        cache += x -> seed
        seed += 1
        cache(x)


  val ex =
    """graph TD
      |  subgraph Method
      |    Init2(( )):::in --> f[ ]:::tr
      |    f --> A3([A3])
      |    f --> A4([A4])
      |    A3 --> f2[ ]:::tr
      |    A4 --> f2
      |    f2 --> Stop2(( )):::st
      |
      |  end
      |    mi2:::it -.mi2-ai3.-> A3
      |    A3 -.ao3-mo2.-> mo2:::it
      |
      |classDef in fill:#2f2,stroke:#ccc;
      |classDef st fill:#000,stroke:#f22,stroke-width:4px;
      |classDef tr fill:#888,stroke-width:0pt;
      |classDef gw fill:#8f8,stroke-width:1pt,stroke:#000;
      |classDef it fill:#f88,stroke-width:1pt,stroke:#822;
      |classDef default fill:#ff8,stroke-width:1pt,stroke:#882
      |""".stripMargin
