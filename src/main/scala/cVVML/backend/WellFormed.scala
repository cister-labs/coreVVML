package cVVML.backend

import cVVML.lang.Syntax._

object WellFormed:
  /*
  1. All source pins are connected : ∀p∈Sr:∃p′ ∈Sk:p<p′ (not applicable)
  2. All sink pins are connected:∀p∈Sk:∃p′ ∈Sr:p′<p (not applicable)
  3. All activities have successors:∀a∈A\↓:∃x∈(A∪F):a→x
  4. All activities have predecessors:∀a∈A\I:∃x∈(A∪F):x→a
  5. Can start and finish: I!=∅ and ↓ != ∅
  6. Nested activities have matching interfaces: γ(a) = m2 implies
  inputs(a) = inputs(m2) ∧ outputs(a) = outputs(m2)
    (relaxed: only need to use a subset of interfaces)
  --
  7. All activities must have some sink pin
  */

  type Result[A] = Either[String,A]

  /** (3) Check if all activities have successors */
  def actHaveSucc(m:Method,name:String): Result[Unit] =
    for (a,d)<-m.activities if !m.stop(a) && !m.next.contains(a) do
      return Left(s"No successor for activity \"$d\" [@ $name].")
    for f<-m.forks if !m.stop(f) && !m.next.contains(f) do
      return Left(s"No successor for fork/merge node \"$f\" [@ $name].")
    Right(())

  /** (4) Check if all activities have predecessors */
  def actHavePred(m:Method,name:String): Result[Unit] =
    val reach = (for (_,as)<-m.next; a<-as yield a) ++ m.start
    if (m.activities--reach).nonEmpty then
      return Left(s"No predecessor for activity(ies) \"${
        (m.activities--reach).map(x=>x._2).mkString(",")}\" [@ $name].")
    if (m.forks--reach).nonEmpty then
      return Left(s"No predecessor for fork(s) \"${
        (m.forks--reach).mkString(",")}\" [@ $name].")
    Right(())

  /** (5a) Check if a method has some start activity */
  def hasStart(m:Method, name:String): Result[Unit] =
    if m.start.isEmpty then return Left(s"No starting activity found in `$name'.")
    Right(())

  /** (5b) Check if a method has some stop activity */
  def hasStop(m: Method, name:String): Result[Unit] =
    if m.stop.isEmpty then return Left(s"No stopping activity found in `$name'.")
    Right(())

  /** (6) Check if method calls match interfaces */
  def callsOK(m:Method,name:String,p:Program): Result[Unit] =
    for (act,mname)<-m.call do
      val called = p.ms.get(mname) match
        case Some(x)=>x
        case None => return Left(s"No method named `$mname\' found.")
      val minpins = called.inputs(None).map(_.name)
      val moutpins = called.outputs(None).map(_.name)
      val ainpins = m.inputs(Some(act)).map(_.name)
      val aoutpins = m.outputs(Some(act)).map(_.name)
      for pinn<-ainpins  if !moutpins(pinn) do return Left(s"In $name: output pin `$pinn' was not found in method `$mname'.")
      for pinn<-aoutpins if !minpins(pinn)  do return Left(s"In $name: input pin `$pinn' was not found in method `$mname'.")
    Right(())

  /** (7) Checks of all (non-decision) activities have some output pin */
  def allHaveSnkPins(m:Method,name:String): Result[Unit] =
    val sources = m.src.flatMap(p=>p.act)
    for (a,d)<-m.activities if !sources.contains(a) && !m.isDecisionAct(a) do
      return Left(s"Activity `$d' has no output pins [@ $name].")
    Right(())

  /** Checks all properties above for all methods.  */
  def checkAll(p:Program): Result[Unit] =
    var errs=List[String]()
    for (name,m)<-p.ms; Left(err)<-actHaveSucc(m,name) do errs ::= err
    for (name,m)<-p.ms; Left(err)<-actHavePred(m,name) do errs ::= err
    for (name,m)<-p.ms; Left(err)<-hasStop(m,name) do errs ::= err
    for (name,m)<-p.ms; Left(err)<-hasStart(m,name) do errs ::= err
    for (name,m)<-p.ms; Left(err)<-callsOK(m,name,p) do errs ::= err
    for (name,m)<-p.ms; Left(err)<-allHaveSnkPins(m,name) do errs ::= err
    if errs.isEmpty then Right(())
    else Left(errs.reverse.mkString("\n"))

  /** checks all properties above and pretty-prints the result. */
  def checkAllPP(p:Program): String = checkAll(p) match
    case Left(e) => e
    case Right(_) => "Ok"

