/*
    This file is part of soot-scala.

    Soot-scala is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Soot-scala is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with soot-scala.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2015 École Polytechnique de Montréal & Tata Consultancy Services.
 */
package soot.util

import java.io.File
import java.nio.file.Path

import soot.jimple._
import soot.jimple.spark.pag.PAG
import soot.jimple.toolkits.callgraph.{CallGraph, ContextSensitiveCallGraph, ReachableMethods}
import soot.jimple.toolkits.pointer.SideEffectAnalysis
import soot.options.Options
import soot.tagkit._
import soot.toolkits.exceptions.ThrowAnalysis
import soot.{Unit => SootUnit, _}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.util.Try

/**
 * Convenience classes for using Soot with a scala-like API
 * @author Marc-André Laverdière-Papineau
 */
object ScalaWrappers {

  type SootUnit = soot.Unit

  private def ifToOption[T](condition: => Boolean, positiveResult: => T): Option[T] = if (condition) Some(positiveResult) else None

  implicit class RichNumerable(val n: Numberable) extends AnyVal {
    @inline def number: Int = n.getNumber

    @inline def number_=(newNumber: Int) = n.setNumber(newNumber)
  }

  object SSootClass {
    def apply(name: String, modifiers: Int = Modifier.PUBLIC, fields: TraversableOnce[SootField] = Traversable(),
              interfaces: TraversableOnce[SootClass] = Traversable(), superClass: Option[SootClass] = None, methods: Traversable[SootMethod] = Traversable(),
              outerClass: Option[SootClass] = None, annotations: TraversableOnce[AnnotationTag] = Traversable()): SootClass = {
      val sc = new SootClass(name, modifiers)
      superClass.foreach(sc.superclass = _)
      outerClass.foreach(sc.outerClass = _)
      fields.foreach(sc.addField)
      interfaces.foreach(sc.addInterface)
      methods.foreach(sc.addMethod)

      if (annotations.nonEmpty)
        sc.addTag(SVisibilityAnnotationTag(annotations))

      sc
    }

  }

  implicit class RichSootClass(val v: SootClass) extends AnyVal {

    @inline def name: String = v.getName

    @inline def name_=(newName: String) = v.setName(newName)

    @inline def packageName: String = v.getPackageName

    @inline def shortName: String = v.getShortName

    @inline def modifiers: Int = v.getModifiers

    @inline def modifiers_=(mods: Int) = v.setModifiers(mods)

    @inline def fields: Chain[SootField] = v.getFields

    //@inline def fields_+= (newField : SootField) = v.addField(newField)
    @inline def interfaces: Chain[SootClass] = v.getInterfaces

    /**
     * Replaces the list of interfaces for this class.
     * This means that previous interface associations are going to be removed
     * @param ifaces the new list of interfaces
     */
    @inline def interfaces_=(ifaces: Traversable[SootClass]) {
      v.getInterfaces.foreach(v.removeInterface)
      ifaces.foreach(v.addInterface)
    }

    @inline def superclass: SootClass = v.getSuperclass

    @inline def superclass_=(sc: SootClass) = v.setSuperclass(sc)

    //@inline def interfaces_+= (newInterface : SootClass) = v.addInterface(newInterface)
    @inline def superClassOpt: Option[SootClass] = ifToOption(v.hasSuperclass, v.getSuperclass)

    @inline def methods: Traversable[SootMethod] = v.getMethods.asScala

    //@inline def methods_+= (newMethod : SootMethod) = v.addMethod(newMethod)
    @inline def outerClass: SootClass = v.getOuterClass

    @inline def outerClass_=(sc: SootClass) = v.setOuterClass(sc)

    @inline def outerClassOpt: Option[SootClass] = ifToOption(v.hasOuterClass, v.getOuterClass)

    @inline def typ: RefType = v.getType

    @inline def typ_=(newTyp: RefType) = v.setRefType(newTyp)

    @inline def field(subsignature: String): SootField = v.getField(subsignature)

    @inline def fieldOpt(subsignature: String): Option[SootField] = Option(v.getFieldUnsafe(subsignature))

    @inline def field(name: String, typ: Type): SootField = v.getField(name, typ)

    @inline def fieldOpt(name: String, typ: Type): Option[SootField] = Option(v.getFieldUnsafe(name, typ))

    @inline def fieldByName(name: String): SootField = v.getFieldByName(name)

    @inline def fieldByNameOpt(name: String): Option[SootField] = Option(v.getFieldByNameUnsafe(name))

    @inline def fieldsByName(name: String): Traversable[SootField] = fields.filter(_.name == name)

    @inline def methodsByName(name: String): Traversable[SootMethod] = methods.filter(_.name == name)

    @inline def methodByName(name: String): SootMethod = v.getMethodByName(name)

    @inline def methodByNameOpt(name: String): Option[SootMethod] = Option(v.getMethodByNameUnsafe(name))

    @inline def method(subsignature: String): SootMethod = v.getMethod(subsignature)

    @inline def methodOpt(subsignature: String): Option[SootMethod] = Option(v.getMethodUnsafe(subsignature))

    @inline def method(subsignature: NumberedString): SootMethod = v.getMethod(subsignature)

    @inline def methodOpt(subsignature: NumberedString): Option[SootMethod] = Option(v.getMethodUnsafe(subsignature))

    @inline def method(name: String, paramTypes: List[Type]): SootMethod = v.getMethod(name, paramTypes.asJava)

    @inline def methodOpt(name: String, paramTypes: Seq[Type]): Option[SootMethod] = {
      val paramJava = paramTypes.asJava
      ifToOption(v.declaresMethod(name, paramJava), v.getMethod(name, paramJava))
    }

    @inline def method(name: String, paramTypes: List[Type], retType: Type): SootMethod = v.getMethod(name, paramTypes.asJava, retType)

    @inline def methodOpt(name: String, paramTypes: List[Type], retType: Type): Option[SootMethod] = Option(v.getMethodUnsafe(name, paramTypes.asJava, retType))

    //Those have good getter APIs, but annoying setters
    @inline def isInScene_=(flag: Boolean) = v.setInScene(flag)

    @inline def isPhantom_=(flag: Boolean) = v.setPhantom(flag)

    @inline def resolvingLevel_=(lvl: Int) = v.setResolvingLevel(lvl)
  }

  implicit class RichCallGraph(val v: CallGraph) extends AnyVal {
    @inline def callersOf(callee: SootMethod): Traversable[SootMethod] = v.edgesInto(callee).asScala.map(_.getSrc.method()).toTraversable

    @inline def calleesOf(callSite: SootUnit): Traversable[SootMethod] = v.edgesOutOf(callSite).asScala.map(_.getTgt.method()).toTraversable

    @inline def calleesFrom(method: SootMethod): Traversable[SootMethod] = v.edgesOutOf(method).asScala.map(_.getTgt.method()).toTraversable
  }

  implicit class RichRefType(val v: RefType) extends AnyVal {
    @inline def anySubType: AnySubType = v.getAnySubType

    @inline def anySubType_=(ast: AnySubType) = v.setAnySubType(ast)

    @inline def arrayElementType: Type = v.getArrayElementType

    @inline def arrayType: ArrayType = v.getArrayType

    @inline def arrayType_=(at: ArrayType) = v.setArrayType(at)

    @inline def className: String = v.getClassName

    @inline def className_=(cn: String) = v.setClassName(cn)

    @inline def sootClass: SootClass = v.getSootClass

    @inline def sootClass_=(sc: SootClass) = v.setSootClass(sc)

    @inline def number: Int = v.getNumber

    @inline def number_=(n: Int) = v.setNumber(n)
  }

  implicit class RichClassMember(val c: ClassMember) extends AnyVal {
    @inline def declaringClass: SootClass = c.getDeclaringClass

    @inline def modifiers: Int = c.getModifiers

    @inline def modifiers_=(mods: Int) = c.setModifiers(mods)

    @inline def isPhantom_=(flag: Boolean) = c.setPhantom(flag)
  }

  implicit class RichSootField(val v: SootField) extends AnyVal {
    @inline def name: String = v.getName

    @inline def name_=(newName: String) = v.setName(newName)

    @inline def declaringClass: SootClass = v.getDeclaringClass

    @inline def signature: String = v.getSignature

    @inline def subSignature: String = v.getSubSignature

    @inline def declaration: String = v.getDeclaration

    /**
     * @return A field signature that is formatted a bit different than the Soot signature.
     *         It consists of the class' name, followed by '.' and the field name.
     */
    @inline def quasiSignature: String = v.getDeclaringClass.getName + "." + v.getName

    @inline def typ: Type = v.getType
  }

  implicit class RichSootMethod(val v: SootMethod) extends AnyVal {
    @inline def isClinit = v.getName == "<clinit>"

    @inline def name: String = v.getName
    @inline def name_=(newName: String) = v.setName(newName)

    @inline def declared: Boolean = v.isDeclared
    @inline def declared_=(flag: Boolean) = v.setDeclared(true)

    @inline def signature: String = v.getSignature
    @inline def subSignature: String = v.getSubSignature

    @inline def body: Body = v.getActiveBody
    @inline def body_=(body: Body) = v.setActiveBody(body)
    @inline def bodyOpt: Option[Body] = ifToOption(v.hasActiveBody, v.getActiveBody)

    @inline def source: MethodSource = v.getSource
    @inline def source_=(ms: MethodSource) = v.setSource(ms)

    @inline def parameterCount : Int = v.getParameterCount
    @inline def parameterTypes: Seq[Type] = v.getParameterTypes.asScala
    @inline def parameterTypes_=(newPt: Seq[Type]) = v.setParameterTypes(newPt.asJava)

    @inline def exceptions: Seq[SootClass] = v.getExceptions.asScala
    @inline def exceptions_=(newEx: Seq[SootClass]) = v.setExceptions(newEx.asJava)

    @inline def returnType: Type = v.getReturnType
    @inline def returnType_=(typ: Type) = v.setReturnType(typ)

    @inline def declaringClass: SootClass = v.getDeclaringClass
    @inline def declaringClass_=(sc: SootClass): Unit = {
      v.setDeclaringClass(sc)
      v.setDeclared(true)
    }

    @inline def locals: Chain[Local] = if (v.hasActiveBody) v.body.getLocals else new HashChain[Local]()

    @inline def units: Chain[SootUnit] = if (v.hasActiveBody) v.body.getUnits else new HashChain[SootUnit]()

    @inline def statements: Chain[Stmt] =
      if (v.hasActiveBody) v.body.units.asInstanceOf[Chain[Stmt]] else new HashChain[Stmt]()

    @inline def numberedSignature: NumberedString = v.getNumberedSubSignature

    @inline def paramLocals: Seq[Value] = for (i <- 0 until v.getParameterCount; bod <- v.bodyOpt) yield bod.getParameterLocal(i)
  }

  implicit class RichSootMethodRef(val v: SootMethodRef) extends AnyVal {
    @inline def isClinit = v.name == "<clinit>"

    @inline def signature: String = v.getSignature

    @inline def subSignature: String = v.getSubSignature.getString

    @inline def paramTypes: Seq[Type] = v.parameterTypes().asScala
  }


  implicit class RichTrap(val v : Trap) extends AnyVal {
    @inline def beginUnit : SootUnit = v.getBeginUnit
    @inline def beginUnit_=(newU : SootUnit) = v.setBeginUnit(newU)
    @inline def beginStmt : Stmt = v.getBeginUnit.asInstanceOf[Stmt]

    @inline def endUnit : SootUnit = v.getEndUnit
    @inline def endUnit_=(newU : SootUnit) = v.setEndUnit(newU)
    @inline def endStmt : Stmt = v.getEndUnit.asInstanceOf[Stmt]

    @inline def exception : SootClass = v.getException
    @inline def exception_= (sc: SootClass) = v.setException(sc)

    @inline def handlerUnit : SootUnit = v.getHandlerUnit
    @inline def handlerUnit_=(newU : SootUnit) = v.setHandlerUnit(newU)
    @inline def handlerStmt : Stmt = v.getHandlerUnit.asInstanceOf[Stmt]
  }

  implicit class RichBody(val v: Body) extends AnyVal {
    @inline def units: PatchingChain[SootUnit] = v.getUnits

    @inline def statements: PatchingChain[Stmt] = v.getUnits.asInstanceOf[PatchingChain[Stmt]]

    @inline def locals: Chain[Local] = v.getLocals

    @inline def method: SootMethod = v.getMethod

    @inline def thisLocal: Local = v.getThisLocal

    @inline def parameterLocal(i: Int) = v.getParameterLocal(i)

    @inline def traps: Chain[Trap] = v.getTraps

    @inline def parameterLocals: Seq[Local] = v.getParameterLocals.asScala
  }

  object SStmt {
    def unapply(stmt: Stmt): Option[(Option[InvokeExpr], Option[ArrayRef], Option[FieldRef])] =
      Some(stmt.invokeExprOpt, stmt.arrayRefOpt, stmt.fieldRefOpt)
  }

  implicit class RichStmt(val v: Stmt) extends AnyVal {
    @inline def invokeExpr: InvokeExpr = v.getInvokeExpr

    @inline def invokeExprOpt: Option[InvokeExpr] = ifToOption(v.containsInvokeExpr(), v.getInvokeExpr)

    @inline def arrayRef: ArrayRef = v.getArrayRef

    @inline def arrayRefOpt: Option[ArrayRef] = ifToOption(v.containsArrayRef(), v.getArrayRef)

    @inline def fieldRef: FieldRef = v.getFieldRef

    @inline def fieldRefOpt: Option[FieldRef] = ifToOption(v.containsFieldRef(), v.getFieldRef)
  }

  object SIfStmt {
    def unapply(stmt: IfStmt): Option[(Value, Stmt)] = Some(stmt.condition, stmt.target)
  }

  implicit class RichIfStmt(val v: IfStmt) extends AnyVal {
    @inline def condition: Value = v.getCondition

    @inline def target: Stmt = v.getTarget
  }

  object SBinopExpr {
    def unapply(expr: BinopExpr): Option[(Value, Value)] = Some(expr.left, expr.right)
  }

  implicit class RichBinopExpr(val v: BinopExpr) extends AnyVal {
    @inline def left: Value = v.getOp1

    @inline def right: Value = v.getOp2
  }

  object SEqExpr {
    def unapply(exp: EqExpr): Option[(Value, Value)] = Some(exp.left, exp.right)
  }

  implicit class RichFastHierarchy(val v: FastHierarchy) extends AnyVal {
    @inline def abstractDispatch(sm: SootMethod): Set[SootMethod] = v.resolveAbstractDispatch(sm.getDeclaringClass, sm).asScala.toSet

    @inline def interfaceImplementers(sc: SootClass): Set[SootClass] = if (sc.isInterface) v.getAllImplementersOfInterface(sc).asScala.toSet else Set[SootClass]()

    @inline def subClassesOf(sc: SootClass): Set[SootClass] = v.getSubclassesOf(sc).asScala.toSet

    @inline def allSubinterfaces(sc: SootClass): Set[SootClass] = v.getAllSubinterfaces(sc).asScala.toSet
  }

  implicit class RichScene(val v: Scene) extends AnyVal {

    @inline def applicationClasses: Traversable[SootClass] = v.getApplicationClasses.asScala

    @inline def classes: Traversable[SootClass] = v.getClasses.asScala

    @inline def libraryClasses: Traversable[SootClass] = v.getLibraryClasses.asScala

    @inline def phantomClasses: Traversable[SootClass] = v.getPhantomClasses.asScala

    @inline def field(fieldSpec: String): SootField = v.getField(fieldSpec)

    @inline def fieldOpt(fieldSpec: String): Option[SootField] = ifToOption(v.containsField(fieldSpec), v.getField(fieldSpec))

    @inline def fieldRef(fieldSpec: String): SootFieldRef = v.getField(fieldSpec).makeRef()

    @inline def fieldRefOpt(fieldSpec: String): Option[SootFieldRef] = ifToOption(v.containsField(fieldSpec), v.getField(fieldSpec).makeRef())


    @inline def refType(className: String): RefType = v.getRefType(className)

    @inline def refTypeOpt(className: String): Option[RefType] = ifToOption(v.containsType(className), v.getRefType(className))

    @inline def sootClass(className: String): SootClass = v.getSootClass(className)

    //We could consider this one... v.forceResolve(className, SootClass.BODIES)
    @inline def sootClassOpt(className: String): Option[SootClass] = Option(v.getSootClassUnsafe(className))

    @inline def method(sig: String): SootMethod = v.getMethod(sig)

    @inline def methodOpt(sig: String): Option[SootMethod] = ifToOption(v.containsMethod(sig), v.getMethod(sig))

    @inline def methodRef(sig: String): SootMethodRef = v.getMethod(sig).makeRef()

    @inline def methodRefOpt(sig: String): Option[SootMethodRef] = ifToOption(v.containsMethod(sig), v.getMethod(sig).makeRef())

    @inline def objectType: RefType = v.getObjectType

    @inline def objectClass: SootClass = v.getObjectType.getSootClass

    @inline def hierarchy: Hierarchy = v.getActiveHierarchy

    @inline def hierarchy_=(h: Hierarchy) = v.setActiveHierarchy(h)

    @inline def fastHierarchy: FastHierarchy = v.getOrMakeFastHierarchy

    @inline def fastHierarchy_=(fh: FastHierarchy) = v.setFastHierarchy(fh)

    @inline def callGraph: CallGraph = v.getCallGraph

    @inline def callGraph_=(cg: CallGraph) = v.setCallGraph(cg)

    @inline def contextNumberer: Numberer[Context] = v.getContextNumberer

    @inline def contextNumberer_=(cn: Numberer[Context]) = v.setContextNumberer(cn)

    @inline def contextSensitiveCallGraph: ContextSensitiveCallGraph = v.getContextSensitiveCallGraph

    @inline def contextSensitiveCallGraph_=(cscg: ContextSensitiveCallGraph) = v.setContextSensitiveCallGraph(cscg)

    @inline def defaultThrowAnalysis: ThrowAnalysis = v.getDefaultThrowAnalysis

    @inline def defaultThrowAnalysis_=(ta: ThrowAnalysis) = v.setDefaultThrowAnalysis(ta)

    @inline def entryPoints: Seq[SootMethod] = v.getEntryPoints.asScala

    @inline def entryPoints_=(ep: Seq[SootMethod]) = v.setEntryPoints(ep.asJava)

    @inline def mainClass: SootClass = v.getMainClass

    @inline def mainClass_=(sc: SootClass) = v.setMainClass(sc)

    @inline def mainMethod: SootMethod = v.getMainMethod

    @inline def mainMethod_=(sm: SootMethod): Unit = v.setMainClass(sm.declaringClass)

    @inline def phantomRefs: Boolean = v.getPhantomRefs

    @inline def phantomRefs_=(flag: Boolean) = v.setPhantomRefs(flag)

    @inline def pkgList: Seq[String] = v.getPkgList.asScala

    @inline def pkgList_=(pl: Seq[String]) = v.setPkgList(pl.asJava)

    @inline def pta: PointsToAnalysis = v.getPointsToAnalysis

    @inline def pointsToAnalysis: PointsToAnalysis = v.getPointsToAnalysis

    @inline def pointsToAnalysis_=(pta: PointsToAnalysis) = v.setPointsToAnalysis(pta)

    @inline def pag: PAG = v.getPointsToAnalysis.asInstanceOf[PAG]

    @inline def sootClassPath: String = v.getSootClassPath

    @inline def sootClassPath_=(scp: String) = v.setSootClassPath(scp)

    @inline def reachableMethods: ReachableMethods = v.getReachableMethods()

    @inline def reachableMethods_=(rm: ReachableMethods) = v.setReachableMethods(rm)

    @inline def sideEffectAnalysis: SideEffectAnalysis = v.getSideEffectAnalysis

    @inline def sideEffectAnalysis_=(sea: SideEffectAnalysis) = v.setSideEffectAnalysis(sea)

    @inline def reservedNames: Set[String] = v.getReservedNames.asScala.toSet

  }

  implicit class RichChain[E](val v: Chain[E]) extends Traversable[E] {
    def foreach[U](f: (E) => U) {
      v.iterator().asScala.foreach(f)
    }

    def ++=(elems: Seq[E]) = v.addAll(elems.asJava)

    def +=(elem: E) = v.addLast(elem)

  }

  implicit class RichHost(val v: Host) extends AnyVal {
    @inline def tags = v.getTags.asScala

    @inline def tag(aName: String): Tag = v.getTag(aName)

    @inline def tagOpt(aName: String): Option[Tag] = Option(v.getTag(aName))

    //This is dirty, but I can't think of a much better way without defaulting to `tagOpt(String)`
    //http://failex.blogspot.ca/2013/06/fake-theorems-for-free.html
    @inline def tagOpt[T <: Tag](typ: Class[T]): Option[T] = v.tags.find(t => t.getClass eq typ).map(_.asInstanceOf[T])

    /** Returns -1 if the annotation is missing */
    @inline def lineNumber: Int = v.getJavaSourceStartLineNumber

    @inline def lineNumberOpt: Option[Int] = v.getJavaSourceStartLineNumber match {
      case -1 => None
      case any => Some(any)
    }

  }

  // ----------------- VisibilityAnnotationTag
  object SVisibilityAnnotationTag {

    def apply(annotations: AnnotationTag*): VisibilityAnnotationTag = {
      val annotationTag = new VisibilityAnnotationTag(0)
      annotations.foreach(annotationTag.addAnnotation)
      annotationTag
    }

    def apply(annotations: GenTraversableOnce[AnnotationTag]): VisibilityAnnotationTag = {
      val annotationTag = new VisibilityAnnotationTag(0)
      annotations.foreach(annotationTag.addAnnotation)
      annotationTag
    }

    def unapply(vat: VisibilityAnnotationTag): Traversable[AnnotationTag] = vat.annotations
  }

  implicit class RichVisibilityAnnotationTag(val v: VisibilityAnnotationTag) extends AnyVal {
    @inline def annotations: Traversable[AnnotationTag] = v.getAnnotations.asScala
  }

  // ----------------- AnnotationTag
  object SAnnotationTag {
    def apply(name: String, elements: Seq[AnnotationElem] = Seq()): AnnotationTag = new AnnotationTag(name, elements.asJava)

    /**
     * @param at the `AnnotationTag`
     * @return a tuple with (annotation name, information, elements)
     */
    def unapply(at: AnnotationTag): Option[(String, String, Traversable[AnnotationElem])] =
      Some(at.name, at.info, at.elements)
  }

  object SAnnotationStringElem {
    def apply(name: String, value: String) = new AnnotationStringElem(value, 's', name)
  }

  implicit class RichAnnotationTag(val v: AnnotationTag) extends AnyVal {
    @inline def elements: Traversable[AnnotationElem] = v.getElems.asScala

    @inline def info: String = v.getInfo

    @inline def name: String = v.getName
  }

  // ----------------- AnnotationElem
  object SAnnotationElem {
    def unapply(ae: AnnotationElem): Option[(String, Char)] = Some(ae.name, ae.kind)
  }

  implicit class RichAnnotationElement(val v: AnnotationElem) extends AnyVal {
    @inline def kind: Char = v.getKind

    @inline def name: String = v.getName
  }

  implicit class RichValue(val v: Value) extends AnyVal {
    @inline def useBoxes: Seq[ValueBox] = v.getUseBoxes.asScala.toSeq
  }

  // ----------------- InvokeExpr
  object SInvokeExpr {
    /**
     * @param iexpr the expression
     * @return a tuple with (an `Option` to the base variable, the sequence of arguments, the target method)
     */
    def unapply(iexpr: InvokeExpr): Option[(Option[Value], Seq[Value], SootMethod)] = iexpr match {
      case SStaticInvokeExpr(args, method) => Some(None, args, method)
      case SInstanceInvokeExpr(base, args, method) => Some(Some(base), args, method)
      case _ => throw new RuntimeException("Unhandled invoke expression type")
    }
  }

  implicit class RichInvokeExpr(val v: InvokeExpr) extends AnyVal {
    @inline def args: Seq[Value] = v.getArgs.asScala

    @inline def arg(index: Int): Value = v.getArg(index)

    @inline def argCount: Int = v.getArgCount

    @inline def method: SootMethod = v.getMethod

    //In some modes, this getMethod throw an exception. In others, it merely returns false.
    //So we handle both those cases with Try and Option together. The actual exception gets lost
    //in the process, but that's probably not a big deal
    @inline def methodOpt: Option[SootMethod] = Try(Option(v.getMethod)).getOrElse(None)

    @inline def methodRef: SootMethodRef = v.getMethodRef

    @inline def returnType: Type = v.getType
  }

  // ----------------- StaticInvokeExpr
  object SStaticInvokeExpr {
    def apply(args: Seq[Value], target: SootMethod): StaticInvokeExpr = Jimple.v.newStaticInvokeExpr(target.makeRef(), args.asJava)

    /**
     * @param iexpr the expression
     * @return a tuple with (the sequence of arguments, the target method)
     */
    def unapply(iexpr: StaticInvokeExpr): Option[(Seq[Value], SootMethod)] = Some(iexpr.args, iexpr.method)
  }


  // ----------------- InstanceInvokeExpr
  object SInstanceInvokeExpr {
    def apply(base: Local, args: Seq[Value], target: SootMethod): InstanceInvokeExpr = target.declaringClass match {
      case iface if iface.isInterface => Jimple.v.newInterfaceInvokeExpr(base, target.makeRef(), args.asJava)
      case _ => Jimple.v.newVirtualInvokeExpr(base, target.makeRef(), args.asJava)
    }

    /**
     * @param iiexpr the expression
     * @return a tuple with (the base variable, the sequence of arguments, the target method)
     */
    def unapply(iiexpr: InstanceInvokeExpr): Option[(Value, Seq[Value], SootMethod)] = Some(iiexpr.base, iiexpr.args, iiexpr.method)
  }

  implicit class RichInstanceInvokeExpr(val v: InstanceInvokeExpr) extends AnyVal {
    @inline def base: Value = v.getBase
  }

  // ----------------- Local
  object SLocal {
    def unapply(l: Local): Option[(String, Type)] = Some(l.name, l.getType)
  }

  implicit class RichLocal(val v: Local) extends AnyVal {
    @inline def name: String = v.getName

    @inline def name_=(n: String) = v.setName(n)
  }

  // ----------------- ArrayRef
  object SArrayRef {
    def unapply(ar: ArrayRef): Option[(Value, Value)] = Some(ar.base, ar.index)
  }

  implicit class RichArrayRef(val v: ArrayRef) extends AnyVal {
    @inline def base: Value = v.getBase

    @inline def baseBox: ValueBox = v.getBaseBox

    @inline def index: Value = v.getIndex

    @inline def indexBox: ValueBox = v.getIndexBox
  }

  // ----------------- FieldRef
  object SFieldRef {
    def apply(sf: SootField): SootFieldRef = sf.makeRef()

    def unapply(fr: FieldRef): Option[SootField] = Some(fr.getField)
  }

  implicit class RichFieldRef(val v: FieldRef) extends AnyVal {
    @inline def field: SootField = v.getField

    @inline def fieldRef: SootFieldRef = v.getFieldRef

    @inline def fieldRef_=(sfr: SootFieldRef) = v.setFieldRef(sfr)
  }

  // ----------------- StaticFieldRef
  object SStaticFieldRef {
    def apply(sf: SootField): SootFieldRef = sf.makeRef()

    def unapply(fr: StaticFieldRef): Option[SootField] = Some(fr.getField)
  }

  // ----------------- CastExpr
  object SCastExpr {
    def unapply(ce: CastExpr): Option[(Value, Type)] = Some(ce.op, ce.castType)
  }

  implicit class RichCastExpr(val v: CastExpr) extends AnyVal {
    @inline def op: Value = v.getOp

    @inline def op_=(newOp: Value) = v.setOp(newOp)

    @inline def opBox: ValueBox = v.getOpBox

    @inline def castType: Type = v.getCastType

    @inline def castType_=(newCt: Type) = v.setCastType(newCt)
  }


  // ----------------- Definition
  object SDefinitionStmt {
    /**
     * Extractor that gives a tuple with the left op and the right op (in that order)
     * @param ds the assign statement
     * @return (left op, right op)
     */
    def unapply(ds: DefinitionStmt): Option[(Value, Value)] = Some(ds.leftOp, ds.rightOp)
  }

  implicit class RichDefinitionStmt(val v: DefinitionStmt) extends AnyVal {
    @inline def leftOp: Value = v.getLeftOp

    @inline def leftOpBox: ValueBox = v.getLeftOpBox

    @inline def rightObBox: ValueBox = v.getRightOpBox

    @inline def rightOp: Value = v.getRightOp
  }

  object SIdentityStmt {
    /**
     * Extractor that gives a tuple with the left op and the right op (in that order)
     * @param is the identity statement
     * @return (left op, right op)
     */
    def unapply(is: IdentityStmt): Option[(Value, Value)] = Some(is.leftOp, is.rightOp)
  }


  // ----------------- AssignStmt
  object SAssignStmt {
    def apply(left: Value, right: Value): AssignStmt = Jimple.v.newAssignStmt(left, right)

    /**
     * Extractor that gives a tuple with the left op and the right op (in that order)
     * @param as the assign statement
     * @return (left op, right op)
     */
    def unapply(as: AssignStmt): Option[(Value, Value)] = Some(as.leftOp, as.rightOp)
  }

  implicit class RichAssignStmt(val v: AssignStmt) extends AnyVal {
    @inline def rightOp_=(newRo: Value) = v.setRightOp(newRo)

    @inline def leftOp_=(newLo: Value) = v.setLeftOp(newLo)
  }

  // ----------------- ReturnStmt
  object SReturnStmt {
    def unapply(rs: ReturnStmt): Option[Value] = Some(rs.op)
  }

  implicit class RichReturnStmt(val v: ReturnStmt) extends AnyVal {
    @inline def op: Value = v.getOp

    @inline def op_=(newOp: Value) = v.setOp(newOp)
  }


  // ----------------- Options
  implicit class RichOptions(val v: Options) extends AnyVal {
    @inline def classPath = v.soot_classpath()

    @inline def classPath_=(newCp: String) = v.set_soot_classpath(newCp)

    @inline def classPath_=(newCp: Seq[Path]) = v.set_soot_classpath(newCp.mkString(File.pathSeparator))

    @inline def processPath: Seq[String] = v.process_dir().asScala.toSeq

    @inline def processPath_=(newPp: Seq[String]) = v.set_process_dir(newPp.asJava)

    @inline def allowPhantomRefs: Boolean = v.allow_phantom_refs()

    @inline def allowPhantomRefs_=(apr: Boolean): Unit = v.set_allow_phantom_refs(apr)

    @inline def androidJars: String = v.android_jars()

    @inline def androidJars_=(aj: String): Unit = v.set_android_jars(aj)

    @inline def isInAppMode: Boolean = v.app()

    @inline def isInAppMode_=(setting: Boolean): Unit = v.set_app(setting)

    @inline def computeAstMetrics: Boolean = v.ast_metrics()

    @inline def computeAstMetrics_=(setting: Boolean): Unit = v.set_ast_metrics(setting)

    @inline def checkInitThrowAnalysis: Int = v.check_init_throw_analysis()

    @inline def checkInitThrowAnalysis_=(setting: Int): Unit = v.set_check_init_throw_analysis(setting)

    //Doesn't work - 'reassignment to val'
    //@inline def coffi_=(setting : Boolean) : Unit = v.set_coffi(setting)

    @inline def debug_=(setting: Boolean): Unit = v.set_debug(setting)

    @inline def debugResolver: Boolean = v.debug_resolver()

    @inline def debugResolver_=(setting: Boolean): Unit = v.set_debug_resolver(setting)

    @inline def dumpBody: Seq[String] = v.dump_body().asScala.toSeq

    @inline def dumpBody_=(setting: Seq[String]) = v.set_dump_body(setting.asJava)

    @inline def dumpCfg: Seq[String] = v.dump_cfg().asScala.toSeq

    @inline def dumpCfg_=(setting: Seq[String]) = v.set_dump_cfg(setting.asJava)

    @inline def dynamicClass: Seq[String] = v.dynamic_class().asScala.toSeq

    @inline def dynamicClass_=(setting: Seq[String]) = v.set_dynamic_class(setting.asJava)

    @inline def dynamicDir: Seq[String] = v.dynamic_dir().asScala.toSeq

    @inline def dynamicDir_=(setting: Seq[String]) = v.set_dynamic_dir(setting.asJava)

    @inline def dynamicPackage: Seq[String] = v.dynamic_package().asScala.toSeq

    @inline def dynamicPackage_=(setting: Seq[String]) = v.set_dynamic_package(setting.asJava)

    @inline def excludes: Seq[String] = v.exclude().asScala.toSeq

    @inline def excludes_=(setting: Seq[String]) = v.set_exclude(setting.asJava)

    @inline def forceAndroidJar: String = v.force_android_jar()

    @inline def forceAndroidJar_=(setting: String) = v.set_force_android_jar(setting)

    @inline def fullResolver: Boolean = v.full_resolver()

    @inline def fullResolver_=(setting: Boolean): Unit = v.set_full_resolver(setting)

    @inline def phaseHelp(phase: String) = v.getPhaseHelp(phase)

    @inline def phaseHelp: Seq[String] = v.phase_help().asScala.toSeq

    @inline def phaseHelp_=(help: Seq[String]): Unit = v.set_phase_help(help.asJava)

    @inline def phaseList: Boolean = v.phase_list()

    @inline def phaseList_=(setting: Boolean) = v.set_phase_list(setting)

    @inline def gzip_=(setting: Boolean) = v.set_gzip(setting)

    @inline def help_=(setting: Boolean) = v.set_help(setting)

    @inline def ignoreResolutionErrors = v.ignore_resolution_errors()

    @inline def ignoreResolutionErrors_=(setting: Boolean) = v.set_ignore_resolution_errors(setting)

    @inline def includes: Seq[String] = v.include().asScala.toSeq

    @inline def includes_=(setting: Seq[String]) = v.set_include(setting.asJava)

    @inline def includeAll = v.include_all()

    @inline def includeAll_=(setting: Boolean) = v.set_include_all(setting)

    @inline def interactiveMode = v.interactive_mode()

    @inline def interactiveMode_=(setting: Boolean) = v.set_interactive_mode(setting)

    @inline def j2me_=(setting: Boolean) = v.set_j2me(setting)

    @inline def keepLineNumber = v.keep_line_number()

    @inline def keepLineNumber_=(setting: Boolean) = v.set_keep_line_number(true)

    @inline def keepOffset = v.keep_offset()

    @inline def keepOffset_=(setting: Boolean) = v.set_keep_offset(setting)

    @inline def mainClass = v.main_class()

    @inline def mainClass_=(mc: String) = v.set_main_class(mc)

    @inline def noBodiesForExcluded = v.no_bodies_for_excluded()

    @inline def noBodiesForExcluded_=(setting: Boolean) = v.set_no_bodies_for_excluded(setting)

    @inline def noOutputInnerClassesAttribute = v.no_output_inner_classes_attribute()

    @inline def noOutputInnerClassesAttribute_=(setting: Boolean) = v.set_no_output_inner_classes_attribute(setting)

    @inline def noOutputSourceFileAttribute = v.no_output_source_file_attribute()

    @inline def noOutputSourceFileAttribute_=(setting: Boolean) = v.set_no_output_source_file_attribute(setting)

    @inline def ooat_=(setting: Boolean) = v.set_oaat(setting)

    @inline def omitExceptingUnitEdges = v.omit_excepting_unit_edges()

    @inline def omitExceptingUnitEdges_=(setting: Boolean) = v.set_omit_excepting_unit_edges(setting)

    @inline def onTheFly = v.on_the_fly()

    @inline def onTheFly_=(setting: Boolean) = v.set_on_the_fly(setting)

    @inline def outputDir = v.output_dir()

    @inline def outputDir_=(setting: String) = v.set_output_dir(setting)

    @inline def outputDir_=(setting: Path) = v.set_output_dir(setting.toAbsolutePath.toString) //toAbsolutePath because the directory may not exist yet

    @inline def outputFormat = v.output_format()

    @inline def outputFormat_=(setting: Int) = v.set_output_format(setting)

    @inline def outputJar = v.output_jar()

    @inline def outputJar_=(setting: Boolean) = v.set_output_jar(setting)

    @inline def prependClassPath: Boolean = v.prepend_classpath()

    @inline def prependClassPath_=(setting: Boolean) = v.set_prepend_classpath(setting)

    @inline def wholeProgram = v.whole_program()

    @inline def wholeProgram_=(setting: Boolean) = v.set_whole_program(setting)

    @inline def srcPrec = v.src_prec()

    @inline def srcPrec_=(setting: Int) = v.set_src_prec(setting)

    @inline def time_=(setting: Boolean) = v.set_time(setting)

    @inline def noWriteOutBodyReleasing = v.no_writeout_body_releasing()

    @inline def noWriteOutBodyReleasing_=(setting: Boolean) = v.set_no_writeout_body_releasing(setting)

  }

  object SStringConstant {
    def apply(s: String): StringConstant = StringConstant.v(s)
  }

  object SIntConstant {
    def apply(i: Int): IntConstant = IntConstant.v(i)
  }

  object SLongConstant {
    def apply(l: Long): LongConstant = LongConstant.v(l)
  }

  object SDoubleConstant {
    def apply(d: Double): DoubleConstant = DoubleConstant.v(d)
  }

  object SFloatConstant {
    def apply(d: Float): FloatConstant = FloatConstant.v(d)
  }


}
