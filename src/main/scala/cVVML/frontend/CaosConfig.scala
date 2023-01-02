package cVVML.frontend

import cVVML.backend.{SeqSOS, WellBehaved}
import cVVML.lang.Syntax.Program
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS.*
import caos.sos.{BranchBisim, SOS}
import caos.view.*


object CaosConfig extends Configurator[Program]:

  val name = "Core VVML analyser"
  override val languageName = "Core VVML"
  /** Parser for Choreo expressions. */
  val parser: String=>Program = s => cVVML.lang.Parser.parse(s) match
    case Right(p) => p
    case Left(err) => sys.error(err)

  val examples = List(
    "choices"
      -> "method \"M1\" {\n\tstart act init = \"Initialise\"\n  stop act work = \"Go work\"\n\tact pr2 = \"Other Process\"\n\tinit ->  pr2\n  init -> work\n  pr2 -> work\n\tmin => init.\"pin 1\":Spec\n}"
      -> "Simple example with choices.",
    "fork"
      -> "method M1 {\n\tstart act i = \"Initialise\"\n  act g = \"Go work\"\n\tact pr2 = \"Other Process\"\n  fork f\n  stop fork mrg\n\ti ->  f\n  f -> pr2    f -> g\n  pr2 -> mrg  g -> mrg\n\tmin => i.\"pin 1\":Spec\n}"
      -> "Simple example with concurrency.",
    "call"
      -> "method \"M1\" {\n\tstart act init = \"Initialise\"\n  stop act work = \"Go work\"\n\tact pr2 = call \"Other Process\"\n\tinit ->  pr2\n  pr2 -> work\n\tmin => init.\"pin 1\":Spec\n}\nmethod \"Other Process\" {\n start act think\n stop act think\n}"
      -> "Simple example with call behaviour.",
    "loop"
      -> "method abc{\n  start act a1\n  \t= \"Act1<br>abc\"\n  stop act a2\n  \t= \"as\"\n  a1.oa:a => a2.ia\n  a2.oa => \"Out port\":\"My spec\"\n  a1->a2\n  a2->a1\n}"
      -> "Example with loops.",
    "big fork"
      -> "method M1 {\n  start act i = \"Initialise\"\n  fork f\n  stop fork m\n  i ->  f\n  f->a1 f->a2 f->a3 f->a4 f->a5 f->a6 f->a7\n  a1->m a2->m a3->m a4->m a5->m a6->m a7->m\n}"
      -> "Example with an explosion of states",
    "EA"
      -> "method Process {\n\tstart act m = \"Define VVML method\"\n  stop act w = call \"Specify workflow\"\n\n\tm -> w\n\t\n  m.\"\":\"Meth Spec\"  => w.\"\":\"Meth Spec\"\n\tm.\"\":\"Meth Spec\"  =>   \"\":\"Meth Spec\"\n\tw.\"\":\"Workf Spec\" =>   \"\":\"Workf Spec\"\n}"
      -> "Simple example that describes how to write VVML diagrams in Enterprise Architect.",
    "MCF"
      -> "method \"Model Checking Families of Real-Time Specifications\" {\n\tstart fork f\n  fork mg\n  act spec = \"Build annotated<br>RT spec\"\n  act tab = \"Build<br>configuration<br>tables\"\n  act app = \"Apply<br>configurations\"\n  act refs = \"Refine<br>specifications\"\n  act refp = \"Refine<br>param&req\"\n  act ver = \"Verify<br>instances\"\n  act expl = \"State explosion<br>or unexpected res\"\n  stop act fail = \"Failed<br>property\"\n\n  f->spec f->tab\n  spec->mg tab->mg\n  mg->app\n  app->ver\n  ver->expl\n  expl->fail expl->refp\n  fail->refs\n  refs->app refp->app\n  \n  Behaviour:\"Behavioural Model\" => spec.beh:\"Behavioural Model\"\n  \"Real-Time\":\"Real Time Parameters\" => spec.beh:\"Behavioural Model\"\n  \"Real-Time\":\"Real Time Parameters\" => tab.beh:\"Behavioural Model\"\n  \"Test scenarios\":Scenarios => tab.scen:Scenarios\n  spec.\"annotated spec\" => refs.in\n  refs.\"annotated spec\"=> app.spec\n  tab.conf=>app.conf tab.conf=>refp.conf tab.conf=>ConfTab\n  refp.rConf=>app.conf \n  app.inst=>ver.inst app.inst=>SpecInstances\n  ver.rep=>expl.rep ver.rep=>fail.rep ver.rep=>VerRep \n}",


  )

  val widgets = List(
//    "Parsed" -> view[Program](_.toString,Text),
    "Well-formed" -> view[Program](cVVML.backend.WellFormed.checkAllPP,Text),
    "Well-behaved (no data)" -> view(WellBehaved.checkPP, Text),
//    "SeqSOS (text)" -> steps(SeqSOS.initial, SeqSOS, SeqSOS.pp, Text),
    "Diagram" -> view[Program](cVVML.backend.Mermaid.apply, Mermaid).expand,
    "Diagram (just data)" -> view[Program](x =>
      cVVML.backend.Mermaid(Program(x.ms.map(kv => kv._1 -> kv._2.noFlow), x.main)), Mermaid),
    "Run (no data)" -> steps(SeqSOS.initial, SeqSOS, cVVML.backend.Mermaid.apply, Mermaid),
//    "SeqSOS (mermait text)" -> steps(SeqSOS.initial, SeqSOS, cVVML.backend.Mermaid.apply, Text),
//    "LTS-info" -> view(p => {
//      val x = SOS.traverse(SeqSOS, SeqSOS.initial(p),max=2000)
//      s"${if x._3 then "" else "- TIMEOUT -\n"}states: ${x._1.size}\nedges: ${x._2}"},
//      Text),
    "Run All (no data)" -> lts(SeqSOS.initial, SeqSOS, SeqSOS.pp),
//    "Diagram2" -> view (cVVML.backend.Mermaid.apply, Text),
//    "Diagram (just sequence)" -> view[Program](x =>
//      cVVML.backend.Mermaid(Program(x.ms.map(kv=>kv._1->kv._2.noData),x.main)), Mermaid),

  )
