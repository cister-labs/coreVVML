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
    "demo"
      -> "method \"M1\" {\n\tstart act a1\n  stop act more = \"more?\": no\n  stop act a2 = call M2\n\n  a1 ->  more\n  more -> a2: yes\n  \n  mi1=>a1.ai1   a1.ao1 => mo1\n  mi1=>a2.mi2   a2.mo2 => mo1\n}\n\nmethod \"M2\" {\n start fork f1\n stop  fork f2\n f1->a3   a3->f2\n f1->a4   a4->f2\n mi2=>a3.ai3  a3.ao3=>mo2\n}"
      -> "Complete demo example: do a1, then maybe a3 and a4 in parallel.<br/><br/>-- M1=a1;(M2 + 0)  M2=a3|a4 --",
    "demo (expanded)"
      -> "method \"Supermethod M1 - test case creation\" {\n\tstart act a1 = \"Create functional test cases\"\n  stop act more = \"Test if requirements of<br/>test cases is sufficient\": no \n  stop act a2 = call \"Submethod M2 - Refinement test case\"\n\n  a1 ->  more\n  more -> a2: yes\n  \n  Requirements    => a1.Requirements\n  a1.\"Test cases\" => \"Test cases\"\n  \"Software architectural design model\"    => a2.\"Software architectural design model\"\n  a2.\"Test cases\" => \"Test cases\"\n}\n\nmethod \"Submethod M2 - Refinement test case\" {\n start fork f1\n stop  fork f2\n act a3 = \"Create test cases from architectural model\"\n act a4 = \"Analyse architecture\"\n f1->a3   a3->f2\n f1->a4   a4->f2\n \"Software architectural design model\" => a3.\"Design model\"\n a3.\"Test case\" => \"Test cases\"\n\n // Comment the lines below to obtain\n // the original \"demo\" example\n \"Software architectural design model\" => a4.\"Design model\"\n a4.\"Report\" => \"Architectural analysis report\" \n}"
      -> "Corrected variation of the \"demo\" example with more realistic names for activities and artefact types.",
    "demo-v2 (expanded)"
      -> "method \"Supermethod M1 - test case creation\" {\n\tstart act a1 = \"Create functional test cases\"\n  stop act more = \"Test if requirements of<br/>test cases is sufficient\": no \n  stop act a2 = call \"Submethod M2 - Refinement test case\"\n\n  a1 ->  more\n  more -> a2: yes\n  \n  Requirements    => a1.Requirements\n  a1.\"Test cases\" => \"Test cases\"\n  a1.\"Test cases\" => more.\"Test cases\"\n  a1.\"Test cases\" => a2.\"Input test cases\"\n  \"Software architectural design model\"    => a2.\"Software architectural design model\"\n  a2.\"Refined test cases\" => \"Test cases\"\n}\n\nmethod \"Submethod M2 - Refinement test case\" {\n start fork f1\n stop  fork f2\n act a3 = \"Create test cases from architectural model\"\n act a4 = \"Analyse architecture\"\n f1->a3   a3->f2\n f1->a4   a4->f2\n \"Software architectural design model\" => a3.\"Design model\"\n \"Input test cases\" => a3.\"Original test cases\"\n a3.\"Refined test case\" => \"Refined test cases\"\n\n // Comment the lines below to obtain\n // the original \"demo\" example\n \"Software architectural design model\" => a4.\"Design model\"\n a4.\"Report\" => \"Architectural analysis report\" \n}"
      -> "Corrected variation of the \"demo\" example with more realistic names for activities and artefact types.",
    "choices"
      -> "method \"M1\" {\n\tstart act init = \"Initialise\"\n  stop act work = \"Go work\"\n\tact other = \"Other Process\"\n\n  init -> work: just work\n  // comment to become ill-behaved\n\tinit ->  other: pre-process\n  // comment to become ill-formed\n  other -> work\n  \n\tinput => init.\"in pin\"\n  init.out => other.in\n  // comment one of the 2 lines\n  // below to become ill-formed\n  other.out => work.in\n  work.out => output\n}"
      -> "Simple example with choices.<br/><br/>-- init;(other + other;work) --",
    "fork"
      -> "method M1 {\n\tstart act i = \"Initialise\"\n  act w = \"Go work\"\n\tact other = \"Other Process\"\n  fork f\n  stop fork mrg\n\t\n  i ->  f\n  f -> other    f -> w\n  other -> mrg  w -> mrg\n  // uncomment to become ill-behaved\n  // w -> i\n  \n\tinput => i.\"pin 1\"\n}"
      -> "Simple example with concurrency.<br/><br/>-- init; (other | work) --",
    "call"
      -> "method \"M1\" {\n\tstart act init = \"Initialise\"\n  stop act work = \"Go work\"\n\tact other = call \"Other Process\"\n\n  init ->  other\n  other -> work\n\t\n  input => init.\"pin 1\"\n}\nmethod \"Other Process\" {\n start act think\n stop act think\n}"
      -> "Simple example with call behaviour.<br/><br/>-- M1=init;Other;work  Other=think --",
    "loop"
      -> "method m1 {\n  start act a1\n  \t= \"Act 1\"\n  stop act a2\n  \t= \"Act 2\"\n    \n  a1.oa:a => a2.ia\n  a2.oa => \"Output pin\":\"Some result\"\n  \n  a1->a2\n  a2->a1\n}"
      -> "Example with loops.<br/><br/>-- a1;a2;(a1;a2)* --",
    "big fork"
      -> "method M1 {\n  start act i = \"Initialise\"\n  fork f\n  stop fork m\n\n  i ->  f\n  f->a1 f->a2 f->a3 f->a4 f->a5 f->a6\n  a1->m a2->m a3->m a4->m a5->m a6->m\n\n  // uncomment to timeout at 2000 states when checking behaviour\n  // f->a7 a7->m\n}"
      -> "Example with an explosion of states.<br/><br/>-- init;(a1|a2|a3|a4|a5|a6) --",
    "EA"
      -> "method Process {\n\tstart act def = \"Define VVML method\"\n  stop act spec = call \"Specify workflow\"\n\n\tdef -> spec\n\t\n  def.\"\":\"Meth Spec\"  => spec.\"\":\"Meth Spec\"\n\tdef.\"\":\"Meth Spec\"  =>   \"\":\"Meth Spec\"\n\tspec.\"\":\"Workf Spec\" =>   \"\":\"Workf Spec\"\n}"
      -> "Simple example that describes how to write VVML diagrams in Enterprise Architect.<br/><br/>-- def;spec --",
    "stuck-1"
      -> "method M1 {\n\tstart act i = \"Initialise\"\n  act w = \"Go work\"\n\tact other = \"Other Process\"\n  fork f\n  stop fork mrg\n\t\n  i ->  f\n  f -> other    f -> w\n  w -> work1  w -> work2\n  work1 -> mrg work2 -> mrg\n  other -> mrg\n  \n  // uncomment to become ill-behaved\n  // w -> i\n  \n\tinput => i.\"pin 1\":int\n}"
      -> "Merger can never succeed.<br/><br/>-- ± init;(other | go;(w1+w2)) --",
    "stuck-2"
      -> "method M1 {\n\tstart act i = \"Initialise\"\n\t\n  i ->  work1\n  i -> work2\n  work2 -> wait  wait -> work2\n\n\tstop act work1  \n}"
      -> "Worker 2 never finishes (but worker 1 can finish).<br/><br/>-- ± init;(w1 + (w2;wait)*) --",
//    "New Syntax"
//      -> "method \"M1\" {\n\t// labelling\n\tact init: \"Initialise\"\n\tact other: \"Other Process\"\n  \n\n\t// marking start/stop/fork\n\tstart init\n   // can have labels\n  stop work: \"Nothing more to do\"\n  fork f // unused\n  \n  // possibly labelled transitions\n  init -> work: \"just work\"\n\tinit ->  other: \"pre-process\"\n  other -> work\n\n\t// unlabelled and typed artifact flows\n\tinput:Int => init.\"in pin\":Float // Int is a type, not a label\n  init.out => other.in\n  other.out => work.in\n  work.out => output\n}"
//      -> "Not implemented yet (just to experiment)",
    "MCF"
      -> "method \"Model Checking Families of Real-Time Specifications\" {\n\tstart fork f\n  fork mg\n  act spec = \"Build annotated<br>RT spec\"\n  act tab = \"Build<br>configuration<br>tables\"\n  act app = \"Apply<br>configurations\"\n  act refs = \"Refine<br>specifications\"\n  act refp = \"Refine<br>param&req\"\n  act ver = \"Verify<br>instances\"\n  act expl = \"State explosion<br>or unexpected res?\"\n  act fail = \"Failed<br>property?\"\n\n  f->spec f->tab\n  spec->mg tab->mg\n  mg->app\n  app->ver\n  ver->expl\n  expl->fail: no\n  stop act fail: no\n  expl->refp: yes\n  fail->refs: yes\n  refs->app refp->app // missing initially\n  \n  Behaviour:\"Behavioural Model\" => spec.beh:\"Behavioural Model\"\n  \"Real-Time\":\"Real Time Parameters\" => spec.beh:\"Behavioural Model\"\n  \"Real-Time\":\"Real Time Parameters\" => tab.beh:\"Behavioural Model\"\n  \"Test scenarios\":Scenarios => tab.scen:Scenarios\n  spec.\"annotated spec\" => refs.in\n  refs.\"annotated spec\"=> app.spec\n  tab.conf=>app.conf tab.conf=>refp.conf tab.conf=>ConfTab\n  refp.rConf=>app.conf \n  app.inst=>ver.inst app.inst=>SpecInstances\n  ver.rep=>expl.rep ver.rep=>fail.rep ver.rep=>VerRep \n}"
      -> "Larger example imported method from UC10.<br><a href=\"https://repo.valu3s.eu/use-cases/uc10-safe-function-out-of-context/workflow/model-checking-families-of-real-time-specifications\">https://repo.valu3s.eu/use-cases/uc10-safe-function-out-of-context/workflow/model-checking-families-of-real-time-specifications</a><br/><br/>-- (spec|tab) ; X=>app;ver;expl;(ref;X + fail;(ref;X + 0)) --",
    "AINC"
      -> "method \"Assessment of implementation of network communication\" {\n\tstart act impl=\"Implementation\"\n  act static=\"Static Code Analysis\"\n  act sim=\"Simulated fault-injection of a network link\"\n  stop act rep=\"Generate Report\"  \n\n  impl -> static\n  static -> sim\n  sim -> rep\n  \n  \"\":Requirements=>impl.Reqs\n  \"\":SystemDescription=>sim.\"\"\n  impl.Code => static.Code\n  static.\"Test Scenarios\" => sim.\"Test Scenarios\"\n  static.AnalysisResult => rep.AnalysisResult\n  sim.\"Performance report\" => rep.\"Performance report\"\n  rep.\"\" => \"\":Report\n}"
      -> "Example method imported from a Use-Case of VALU3S, over the assessment of implementation of network communication.<br><a href=\"https://repo.valu3s.eu/use-cases/intelligent-traffic-surveillance/workflow/assessment-of-implementation-of-network-communication\">https://repo.valu3s.eu/use-cases/intelligent-traffic-surveillance/workflow/assessment-of-implementation-of-network-communication</a><br/><br/>-- impl; analysis; faultInj; report --",
    "#1"
      -> "method m1 {\n  start act a1 = \"Act 1\"\n  stop act  a2 = \"Act 2\"\n    \n  a1.data => a2.data\n  \n  a1->a2\n}"
      -> "Is it correct? (Didactic example)",
    "#2"
      -> "method m1 {\n  start fork f1\n  stop act A3\n\n  f1->A1 f1->A2\n  A1->A3 A2->A3\n}"
      -> "Is it correct? (Didactic example)",
    "#3"
      -> "method m1 {\n\tstart act ch=\"?\"\n  fork f1\n  stop act A3\n\n\tch->A1 ch->A2\n  A1->f1 A2->f1\n  f1->A3\n}"
      -> "Is it correct? (Didactic example)",
    "#4"
      -> "method m1 {\n  start fork f1\n  fork f2\n  stop fork f3\n\n\tf1->f2 f1->A3\n  f2->A1 f2->A2\n  A1->f3 A2->f3 A3->f3\n}"
      -> "Is it correct? (Didactic example)",
    "#5"
      -> "method m1 {\n  start fork f1\n  act ch = \"?\"\n  stop fork f3\n\n\tf1->ch f1->A3\n  ch->A1 ch->A2\n  A1->f3 A2->f3 A3->f3\n}"
      -> "Is it correct? (Didactic example)",
    "#6"
      -> "method m1 {\n  start act a1 = \"A1\"\n  stop  act a2 = \"A2\"\n    \n  a1.data => a2.data\n  a1.out => out\n  a2.out => out\n  \n  a1->a2\n  a2->a1\n}"
      -> "Is it correct? (Didactic example)",
    "#7"
      -> "method m1 {\n  start act a1\n  fork f1\n  stop act a4\n\t \n \ta1->f1\n  f1->a2 f1->a3\n  a2->a1\n  a2->a4 a3->a4\n}"
      -> "Is it correct? (Didactic example)",
    "#7"
      -> "method m1 {\n  start act a1\n  fork f1\n  stop act a4\n\t \n \ta1->f1\n  f1->a2 f1->a3\n  a2->a1\n  a2->a4 a3->a4\n}"
      -> "Is it correct? (Didactic example)",
    "#8"
      -> "method m1 {\n  start fork f1\n  stop fork f2\n  act ch = \"?\"\n\t \n  f1->a1 f1->a2\n  a1->ch\n  a2->a4\n  ch->a3 ch->a4\n  a3->f2 a4->f2\n}"
      -> "Is it correct? (Didactic example)",
    "#9"
      -> "method m1 {\n  stop act a1\n  start act ch = \"?\"\n\t \n  ch->a1 ch->a2\n  a2->a3 a3->a2\n}"
      -> "Is it correct? (Didactic example)",
    "#10"
      -> "method m1 {\n  stop act a1\n  start act a1\n\t \n  a2->a3 a3->a2\n}"
      -> "Is it correct? (Didactic example)",
  )

  val widgets = List(
//    "Parsed" -> view[Program](_.toString,Text),
    "Well-formed" -> view[Program](cVVML.backend.WellFormed.checkAllPP,Text).expand.moveTo(1),
    "Well-behaved (no data)" -> view[Program](WellBehaved.checkPP, Text).expand.moveTo(1),
//    "SeqSOS (text)" -> steps(SeqSOS.initial, SeqSOS, SeqSOS.pp, Text),
    "Diagram" -> view[Program](cVVML.backend.Mermaid.apply(_), Mermaid).expand,
    "Diagram (just data)" -> view[Program](x =>
      cVVML.backend.Mermaid(Program(x.ms.map(kv => kv._1 -> kv._2.noFlow), x.main), justData=true), Mermaid),
    "Run (no data)" -> steps(SeqSOS.initial, SeqSOS, cVVML.backend.Mermaid.apply, Mermaid),
//    "Run2 (no data)" -> steps(SeqSOS.initial, SeqSOS, cVVML.backend.Mermaid.apply, Text),
//    "Run3 (no data)" -> steps(SeqSOS.initial, SeqSOS, SeqSOS.pp, Text),
//    "SeqSOS (mermait text)" -> steps(SeqSOS.initial, SeqSOS, cVVML.backend.Mermaid.apply, Text),
//    "LTS-info" -> view(p => {
//      val x = SOS.traverse(SeqSOS, SeqSOS.initial(p),max=2000)
//      s"${if x._3 then "" else "- TIMEOUT -\n"}states: ${x._1.size}\nedges: ${x._2}"},
//      Text),
    "Run All (no data)" -> lts(SeqSOS.initial, SeqSOS, _=>" "), //SeqSOS.pp),
//    "Diagram2" -> view (cVVML.backend.Mermaid.apply, Text),
//    "Diagram (just sequence)" -> view[Program](x =>
//      cVVML.backend.Mermaid(Program(x.ms.map(kv=>kv._1->kv._2.noData),x.main)), Mermaid),

  )
