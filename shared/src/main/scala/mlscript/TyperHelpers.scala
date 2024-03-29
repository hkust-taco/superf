package mlscript

import scala.collection.mutable.{Map => MutMap, Set => MutSet, LinkedHashMap, LinkedHashSet}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.annotation.tailrec
import mlscript.utils._, shorthands._

/** Inessential methods used to help debugging. */
abstract class TyperHelpers { Typer: Typer =>
  
  type CompareRecTypes >: Bool
  
  protected var constrainCalls = 0
  protected var annoyingCalls = 0
  protected var subtypingCalls = 0
  protected var constructedTypes = 0
  def stats: (Int, Int, Int, Int) =
    (constrainCalls, annoyingCalls, subtypingCalls, constructedTypes)
  def resetStats(): Unit = {
    constrainCalls = 0
    annoyingCalls  = 0
    subtypingCalls = 0
    constructedTypes = 0
  }
  
  protected val noPostTrace: Any => String = _ => ""
  
  protected var indent = 0
  def trace[T](pre: => String)(thunk: => T)(post: T => String = noPostTrace): T = {
    println(pre)
    indent += 1
    val res = try thunk finally indent -= 1
    if (post isnt noPostTrace) println(post(res))
    res
  }
  @inline def traceNot[T](pre: => String)(thunk: => T)(post: T => String = noPostTrace): T =
    thunk
  
  def emitDbg(str: String): Unit = scala.Predef.println(str)
  
  // Shadow Predef functions with debugging-flag-enabled ones:
  
  def println(msg: => Any): Unit = if (dbg) emitDbg("| " * indent + msg)
  
  /** A more advanced println version to show where things are printed from. */
  // def println(msg: => Any)(implicit file: sourcecode.FileName, line: sourcecode.Line): Unit =
  //   if (dbg) {
  //     emitDbg((if (showPrintPrefix) {
  //       val prefix = s"[${file.value}:${line.value}]"
  //       prefix + " " * (30 - prefix.length)
  //     } else "") + "| " * indent + msg)
  //   }
  // val showPrintPrefix =
  //   // false
  //   true
  
  def dbg_assert(assertion: => Boolean): Unit = if (dbg) scala.Predef.assert(assertion)
  // def dbg_assert(assertion: Boolean): Unit = scala.Predef.assert(assertion)
  
  
  final def printPol(pol: Bool): Str = pol match {
    case true => "+"
    case false => "-"
  }
  final def printPol(pol: Opt[Bool]): Str = pol match {
    case S(p) => printPol(p)
    case N => "="
  }
  def printPol(pol: PolMap): Str = printPol(pol.base)
  
  def recordIntersection(fs1: Ls[Var -> FieldType], fs2: Ls[Var -> FieldType]): Ls[Var -> FieldType] =
    mergeMap(fs1, fs2)(_ && _).toList
  
  def recordUnion(fs1: Ls[Var -> FieldType], fs2: Ls[Var -> FieldType]): Ls[Var -> FieldType] = {
    val fs2m = fs2.toMap
    fs1.flatMap { case (k, v) => fs2m.get(k).map(v2 => k -> (v || v2)) }
  }
  
  /** Note that this version of `subst` intentionally substitutes unhygienically
    * over the outer polymorphic type, as needed by the class typing infrastructure. */
  def subst(ts: PolymorphicType, map: Map[SimpleType, SimpleType])
        (implicit ctx: Ctx): PolymorphicType = 
    PolymorphicType(ts.polymLevel, subst(ts.body, map))
  
  def subst(st: SimpleType, map: Map[SimpleType, SimpleType], substInMap: Bool = false)
        (implicit ctx: Ctx): SimpleType = {
    val cache: MutMap[TypeVariable, SimpleType] = MutMap.empty
    val subsLvl: Level = map.valuesIterator.map(_.level).reduceOption(_ max _).getOrElse(MinLevel)
    def go(st: SimpleType): SimpleType = {
            // trace(s"subst($st)") {
      map.get(st) match {
        case S(res) => if (substInMap) go(res) else res
        case N =>
          st match {
            case tv @ AssignedVariable(ty) => cache.getOrElse(tv, {
              val v = freshVar(tv.prov, S(tv), tv.nameHint)(tv.level)
              cache += tv -> v
              v.assignedTo = S(go(ty))
              v
            })
            case tv: TypeVariable if tv.lowerBounds.isEmpty && tv.upperBounds.isEmpty =>
              cache += tv -> tv
              tv
            case tv: TypeVariable => cache.getOrElse(tv, {
              val v = freshVar(tv.prov, S(tv), tv.nameHint)(tv.level)
              cache += tv -> v
              v.lowerBounds = tv.lowerBounds.map(go(_))
              v.upperBounds = tv.upperBounds.map(go(_))
              v
            })
            case poly: PolymorphicType if poly.polymLevel < subsLvl =>
              go(poly.raiseLevelTo(subsLvl))
            case _ => st.map(go(_))
          }
      }
      // }(r => s"= $r")
    }
    go(st)
  }
  
  /** Substitutes only at the syntactic level, without updating type variables nor traversing their bounds. */
  def substSyntax(st: SimpleType)(map: PartialFunction[SimpleType, SimpleType]): SimpleType =
    // trace(s"substSyntax $st") {
      map.applyOrElse[ST, ST](st, _.map(substSyntax(_)(map)))
    // }(r => s"=> $r")
  
  def tupleIntersection(fs1: Ls[Opt[Var] -> FieldType], fs2: Ls[Opt[Var] -> FieldType]): Ls[Opt[Var] -> FieldType] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) if n1 =/= n2 => (N, t1 && t2)
      case ((no1, t1), (no2, t2)) => (no1 orElse no2, t1 && t2)
    }
  }
  def tupleUnion(fs1: Ls[Opt[Var] -> FieldType], fs2: Ls[Opt[Var] -> FieldType]): Ls[Opt[Var] -> FieldType] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) => (Option.when(n1 === n2)(n1), t1 || t2)
      case ((no1, t1), (no2, t2)) => (N, t1 || t2)
    }
  }
  
  def factorize(cs: Ls[Conjunct], sort: Bool): Ls[ST] = {
    val factors = MutMap.empty[Factorizable, Int]
    cs.foreach { c =>
      c.vars.foreach { v =>
        factors(v) = factors.getOrElse(v, 0) + 1
      }
      c.nvars.foreach { v =>
        val nv = NegVar(v)
        factors(nv) = factors.getOrElse(nv, 0) + 1
      }
      c.lnf match {
        case LhsTop => ()
        case LhsRefined(_, ttags, _, _) =>
          ttags.foreach { ttg =>
            factors(ttg) = factors.getOrElse(ttg, 0) + 1
          }
      }
      c.rnf match {
        case RhsBot | _: RhsField => ()
        case RhsBases(ps, _, _) =>
          ps.foreach {
            case ttg: AbstractTag =>
              val nt = NegAbsTag(ttg)
              factors(nt) = factors.getOrElse(nt, 0) + 1
            case _ => ()
          }
      }
    }
    factors.maxByOption(_._2) match {
      // case S((fact, n)) =>  // Very strangely, this seems to improve some StressTrait tests slightly...
      case S((fact, n)) if n > 1 =>
        val (factored, rest) = fact match {
          case v: TV =>
            cs.partitionMap(c => if (c.vars(v)) L(c) else R(c))
          case NegVar(v) =>
            cs.partitionMap(c => if (c.nvars(v)) L(c) else R(c))
          case ttg: AbstractTag =>
            cs.partitionMap(c => if (c.lnf.hasTag(ttg)) L(c) else R(c))
          case NegAbsTag(ttg) =>
            cs.partitionMap(c => if (c.rnf.hasTag(ttg)) L(c) else R(c))
        }
        (fact & factorize(factored.map(_ - fact), sort).reduce(_ | _)) :: (
          if (factors.sizeCompare(1) > 0 && factors.exists(f => (f._1 isnt fact) && f._2 > 1))
            factorize(rest, sort)
          else rest.map(_.toType(sort))
        )
      case _ =>
        cs.map(_.toType(sort))
    }
  }
  
  private def cleanupUnion(tys: Ls[ST])(implicit ctx: Ctx): Ls[ST] = {
    var res: Ls[ST] = Nil
    tys.reverseIterator.foreach { ty =>
      if (!res.exists(ty <:< _)) res ::= ty
    }
    res
  }
  def factorize(ctx: Ctx)(ty: ST): ST = {
    cleanupUnion(ty.components(true))(ctx) match {
      case Nil => BotType
      case ty :: Nil => ty
      case cs => factorizeImpl(cs.map(_.components(false)))
    }
  }
  def factorizeImpl(cs: Ls[Ls[ST]]): ST = trace(s"factorize? ${cs.map(_.mkString(" & ")).mkString(" | ")}") {
    def rebuild(cs: Ls[Ls[ST]]): ST =
      cs.iterator.map(_.foldLeft(TopType: ST)(_ & _)).foldLeft(BotType: ST)(_ | _)
    if (cs.sizeCompare(1) <= 0) return rebuild(cs)
    val factors = MutMap.empty[Factorizable, Int]
    cs.foreach { c =>
      c.foreach {
        case tv: TV =>
          factors(tv) = factors.getOrElse(tv, 0) + 1
        case tt: AbstractTag =>
          factors(tt) = factors.getOrElse(tt, 0) + 1
        case nv: NegVar =>
          factors(nv) = factors.getOrElse(nv, 0) + 1
        case nt: NegAbsTag =>
          factors(nt) = factors.getOrElse(nt, 0) + 1
        case _ =>
      }
    }
    println(s"Factors ${factors.mkString(", ")}")
    factors.maxByOption(_._2) match {
      // case S((fact, n)) =>
      case S((fact, n)) if n > 1 =>
        val (factored, rest) =
          cs.partitionMap(c => if (c.contains(fact)) L(c) else R(c))
        println(s"Factor $fact -> ${factored.mkString(", ")}")
        assert(factored.size === n, factored -> n)
        val factoredFactored = fact & factorizeImpl(factored.map(_.filterNot(_ === fact)))
        val restFactored =
          if (factors.sizeCompare(1) > 0 && factors.exists(f => (f._1 isnt fact) && f._2 > 1))
            factorizeImpl(rest)
          else rebuild(rest)
        restFactored | factoredFactored
      case _ => rebuild(cs)
    }
  }(r => s"yes: $r")
  
  
  def mapPol(rt: RecordType, pol: Opt[Bool], smart: Bool)(f: (Opt[Bool], SimpleType) => SimpleType): RecordType =
    RecordType(rt.fields.mapValues(_.update(f(pol.map(!_), _), f(pol, _))))(rt.prov)
  
  def mapPol(bt: BaseType, pol: Opt[Bool], smart: Bool)(f: (Opt[Bool], SimpleType) => SimpleType): BaseType = bt match {
    case FunctionType(lhs, rhs) => FunctionType(f(pol.map(!_), lhs), f(pol, rhs))(bt.prov)
    case ov @ Overload(alts) => ov.mapAltsPol(pol)(f)
    case TupleType(fields) => TupleType(fields.mapValues(_.update(f(pol.map(!_), _), f(pol, _))))(bt.prov)
    case ArrayType(inner) => ArrayType(inner.update(f(pol.map(!_), _), f(pol, _)))(bt.prov)
    case sp @SpliceType(elems) => sp.updateElems(f(pol, _), f(pol.map(!_), _), f(pol, _))
    case wt @ Without(b: ComposedType, ns @ empty()) => Without(b.map(f(pol, _)), ns)(wt.prov) // FIXME very hacky
    case Without(base, names) => Without(f(pol, base), names)(bt.prov)
    case _: ClassTag => bt
  }
  
  
  
  trait SimpleTypeImpl { self: SimpleType =>
    
    def showProvOver(enabled: Bool)(str: Str): Str =
      if (enabled) str + prov.toString
      else str
    
    // Note: we implement hashCode and equals manually because:
    //  1. On one hand, we want a ProvType to compare equal to its underlying type,
    //      which is necessary for recursive types to associate type provenances to
    //      their recursive uses without making the constraint solver diverge; and
    //  2. Additionally, caching hashCode should have performace benefits
    //      — though I'm not sure whether it's best as a `lazy val` or a `val`.
    override lazy val hashCode: Int = this match {
      case tv: TypeVariable => tv.uid
      case ProvType(und) => und.hashCode
      case p: Product => scala.runtime.ScalaRunTime._hashCode(p)
    }
    override def equals(that: Any): Bool = 
        // trace(s"$this == $that") {
        this match {
      case ProvType(und) => (und: Any) === that
      case tv1: TV => that match {
        case tv2: Typer#TV => tv1.uid === tv2.uid
        case ProvType(und) => this === und
        case _ => false
      }
      case p1: Product => that match {
        case that: ST => that match {
          case ProvType(und) => this === und
          case tv: TV => false
          case p2: Product =>
            p1.canEqual(p2) && p1.productArity === p2.productArity && {
              val it1 = p1.productIterator
              val it2 = p2.productIterator
              while(it1.hasNext && it2.hasNext) {
                if (it1.next() =/= it2.next()) return false
              }
              return !it1.hasNext && !it2.hasNext
            }
        }
        case _ => false
      }
    }
    // }(r => s"= $r")
    
    private val initialRun = currentConstrainingRun
    private var shadowRun = initialRun - 1
    private var _shadow: ST = this
    private def computeShadow: ST = this match {
      case tv: TV => tv.original // * Q: no special tratment for assigned TVs?
      case _ => map(_.shadow)
    }
    def shadow: ST =
      if (currentConstrainingRun === shadowRun) _shadow else {
        _shadow = computeShadow
        shadowRun = currentConstrainingRun
        _shadow
      }
    
    def map(f: SimpleType => SimpleType): SimpleType = this match {
      case TypeBounds(lb, ub) => TypeBounds.mkSimple(f(lb), f(ub))
      case FunctionType(lhs, rhs) => FunctionType(f(lhs), f(rhs))(prov)
      case ov @ Overload(as) => ov.mapAltsPol(N)((_, x) => f(x))
      case RecordType(fields) => RecordType(fields.mapValues(_.update(f, f)))(prov)
      case TupleType(fields) => TupleType(fields.mapValues(_.update(f, f)))(prov)
      case sp @ SpliceType(fs) => sp.updateElems(f, f, f)
      case ArrayType(inner) => ArrayType(inner.update(f, f))(prov)
      case ComposedType(pol, lhs, rhs) => ComposedType(pol, f(lhs), f(rhs))(prov)
      case NegType(negated) => NegType(f(negated))(prov)
      case Without(base, names) => Without(f(base), names)(prov)
      case ProvType(underlying) => ProvType(f(underlying))(prov)
      case WithType(bse, rcd) => WithType(f(bse), RecordType(rcd.fields.mapValues(_.update(f, f)))(rcd.prov))(prov)
      case ProxyType(underlying) => f(underlying) // TODO different?
      case TypeRef(defn, targs) => TypeRef(defn, targs.map(f(_)))(prov)
      case PolymorphicType(plvl, und) => PolymorphicType(plvl, f(und))
      case ConstrainedType(cs, bod) =>
        ConstrainedType(cs.map(lu => f(lu._1) -> f(lu._2)), f(bod))
      case _: TypeVariable | _: TypeTag | _: ExtrType => this
    }
    def mapPol(pol: Opt[Bool], smart: Bool = false)(f: (Opt[Bool], SimpleType) => SimpleType)
          (implicit ctx: Ctx): SimpleType = this match {
      case TypeBounds(lb, ub) if smart && pol.isDefined =>
        if (pol.getOrElse(die)) f(S(true), ub) else f(S(false), lb)
      case TypeBounds(lb, ub) => TypeBounds.mkSimple(f(S(false), lb), f(S(true), ub))
      case rt: RecordType => Typer.mapPol(rt, pol, smart)(f)
      case Without(base, names) if smart => f(pol, base).without(names)
      case bt: BaseType => Typer.mapPol(bt, pol, smart)(f)
      case ComposedType(kind, lhs, rhs) if smart =>
        if (kind) f(pol, lhs) | f(pol, rhs)
        else f(pol, lhs) & f(pol, rhs)
      case ComposedType(kind, lhs, rhs) => ComposedType(kind, f(pol, lhs), f(pol, rhs))(prov)
      case NegType(negated) if smart => f(pol.map(!_), negated).neg(prov)
      case NegType(negated) => NegType(f(pol.map(!_), negated))(prov)
      case ProvType(underlying) => ProvType(f(pol, underlying))(prov)
      case WithType(bse, rcd) => WithType(f(pol, bse), RecordType(rcd.fields.mapValues(_.update(f(pol.map(!_), _), f(pol, _))))(rcd.prov))(prov)
      case ProxyType(underlying) => f(pol, underlying) // TODO different?
      case tr @ TypeRef(defn, targs) => TypeRef(defn, tr.mapTargs(pol)(f))(prov)
      case PolymorphicType(plvl, und) =>
        if (smart) PolymorphicType.mk(plvl, f(pol, und)) else PolymorphicType(plvl, f(pol, und))
      case ConstrainedType(cs, bod) =>
        ConstrainedType(cs.map(lu => f(S(true), lu._1) -> f(S(false), lu._2)), f(pol, bod))
      case _: TypeVariable | _: TypeTag | _: ExtrType => this
    }
    
    def toUpper(prov: TypeProvenance): FieldType = FieldType(None, this)(prov)
    def toLower(prov: TypeProvenance): FieldType = FieldType(Some(this), TopType)(prov)
    
    def | (that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType = (this, that) match {
      case (TopType, _) => this
      case (BotType, _) => that
      
      // These were wrong! During constraint solving it's important to keep them!
      // case (_: RecordType, _: PrimType | _: FunctionType) => TopType
      // case (_: FunctionType, _: PrimType | _: RecordType) => TopType
      
      case (_: RecordType, _: FunctionType) => TopType
      case (RecordType(fs1), RecordType(fs2)) =>
        RecordType(recordUnion(fs1, fs2))(prov)
      case (t0 @ TupleType(fs0), t1 @ TupleType(fs1))
        // If the sizes are different, to merge these we'd have to return
        //  the awkward `t0.toArray & t0.toRecord | t1.toArray & t1.toRecord`
      if fs0.sizeCompare(fs1) === 0 =>
        TupleType(tupleUnion(fs0, fs1))(t0.prov)
      case _ if !swapped => that | (this, prov, swapped = true)
      case (`that`, _) => this
      case (NegType(`that`), _) => TopType
      case _ => ComposedType(true, that, this)(prov)
    }
    
    /** This is to intersect two types that occur in negative position,
      * where it may be sound to perform some online simplifications/approximations. */
    def &- (that: SimpleType, prov: TypeProvenance = noProv): SimpleType = (this, that) match {
      // * This one is only correct in negative position because
      // * the existence of first-class polymorphic types makes it unsound in positive positions.
      case (FunctionType(l1, r1), FunctionType(l2, r2)) if approximateNegativeFunction =>
        FunctionType(l1 | l2, r1 &- r2)(prov)
      case _ => this & (that, prov)
    }
    def & (that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType =
        (this, that) match {
      case (TopType | RecordType(Nil), _) => that
      case (BotType, _) => BotType
      
      // * Unnecessary and can complicate constraint solving quite a lot:
      // case (ComposedType(true, l, r), _) => l & that | r & that
      
      // * Tempting, but wrong: this is only valid in positive positions!
      // * (in which case the LBs are irrelevant anyway)
      // * For, instance consider `((A..A) & (B..B)) -> C` in positive position,
      // * which is `(A & B) -> C` and obviously not `(A | B) -> C`.
      // case (TypeBounds(l0, u0), TypeBounds(l1, u1)) =>
      //   TypeBounds(l0 | l1, u0 & u1)(prov)
      
      case (_: ClassTag, _: FunctionType) => BotType
      case (RecordType(fs1), RecordType(fs2)) =>
        RecordType(mergeSortedMap(fs1, fs2)(_ && _).toList)(prov)
      case (t0 @ TupleType(fs0), t1 @ TupleType(fs1)) =>
        if (fs0.sizeCompare(fs1) =/= 0) BotType
        else TupleType(tupleIntersection(fs0, fs1))(t0.prov)
      case _ if !swapped => that & (this, prov, swapped = true)
      case (`that`, _) => this
      case _ if !swapped => that & (this, prov, swapped = true)
      case (NegType(`that`), _) => BotType
      case _ => ComposedType(false, that, this)(prov)
    }
    def neg(prov: TypeProvenance = noProv, force: Bool = false): SimpleType = this match {
      case ExtrType(b) => ExtrType(!b)(noProv)
      case ComposedType(true, l, r) => l.neg() & r.neg()
      case ComposedType(false, l, r) if force => l.neg() | r.neg()
      case NegType(n) => n
      // case _: RecordType => BotType // Only valid in positive positions!
        // Because Top<:{x:S}|{y:T}, any record type negation neg{x:S}<:{y:T} for any y=/=x,
        // meaning negated records are basically bottoms.
      case _ => NegType(this)(prov)
    }
    
    /** This is used to know when two types can be assumed to be mutual subtypes
      * based on a simple equality check. This may not hold when the types involve `TypeBound`s.
      * Indeed, say `type Foo[A] = A -> A`;
      * then `Foo[bot..top]` is an alias for `(bot..top) -> (bot..top)`
      * which IN POSITIVE POSITION is equivalent to `bot -> top`
      * and IN NEGATIVE POSITION is equivalent to `top -> bot`.
      * Therefore, while syntactically `Foo[bot..top] === Foo[bot..top]`,
      * we DO NOT have that `Foo[bot..top] <: Foo[bot..top]`.
      * This check is still a little wrong in that we don't look into type definitions,
      * someone may register type definitions whose bodies involve `TypeBound`s... Though
      * it seems the typer should desugar these bounds when psosible and reject them otherwise.
      * OTOH, note that we truly don't have to look into type variable bounds because these
      * bounds are maintained consistent. */
    lazy val mentionsTypeBounds: Bool = this match {
      case _: TypeBounds => true
      case _ => children(includeBounds = false).exists(_.mentionsTypeBounds)
    }
    
    def >:< (that: SimpleType)(implicit ctx: Ctx, crt: CompareRecTypes = true): Bool =
      this <:< that && that <:< this
    
    // TODO for composed types and negs, should better first normalize the inequation
    def <:< (that: SimpleType)
      (implicit ctx: Ctx, crt: CompareRecTypes = true, cache: MutMap[ST -> ST, Bool] = MutMap.empty): Bool =
    {
    // trace(s"? $this <: $that") {
    // trace(s"? $this <: $that   assuming:  ${
    //     cache.iterator.map(kv => "" + kv._1._1 + (if (kv._2) " <: " else " <? ") + kv._1._2).mkString(" ; ")}") {
      val doCompareRecTypes = (crt === true) && !noRecursiveTypes
      subtypingCalls += 1
      def assume[R](k: MutMap[ST -> ST, Bool] => R): R =
        if (doCompareRecTypes) k(cache.map(kv => kv._1 -> true)) else k(cache)
      if (!mentionsTypeBounds && ((this is that) || this === that)) return true
      (this, that) match {
        case (_, ExtrType(false)) | (ExtrType(true), _) => true
        case (ProxyType(und), _) => und <:< that
        case (_, ProxyType(und)) => this <:< und
        // * Leads to too much simplification in printed types:
        // case (ClassTag(ErrTypeId, _), _) | (_, ClassTag(ErrTypeId, _)) => true
        case (_: TypeTag, _: TypeTag) | (_: TV, _: TV) if this === that => true
        case (ab: ArrayBase, at: ArrayType) => ab.inner <:< at.inner
        case (TupleType(fs1), TupleType(fs2)) =>
          fs1.sizeCompare(fs2) === 0 && fs1.lazyZip(fs2).forall {
            case ((_, ty1), (_, ty2)) => ty1 <:< ty2
          }
        case (RecordType(Nil), _) => TopType <:< that
        case (_, RecordType(Nil)) => this <:< TopType
        case (pt1 @ ClassTag(id1, ps1), pt2 @ ClassTag(id2, ps2)) => (id1 === id2) || pt1.parentsST(id2)
        case (TypeBounds(lb, ub), _) => ub <:< that
        case (_, TypeBounds(lb, ub)) => this <:< lb
        case (FunctionType(l1, r1), FunctionType(l2, r2)) => assume { implicit cache =>
          l2 <:< l1 && r1 <:< r2 
        }
        case (ComposedType(true, l, r), _) => l <:< that && r <:< that
        case (_, ComposedType(false, l, r)) => this <:< l && this <:< r
        case (ComposedType(false, l, r), _) => l <:< that || r <:< that
        case (_, ComposedType(true, l, r)) => this <:< l || this <:< r
        case (RecordType(fs1), RecordType(fs2)) => assume { implicit cache =>
          fs2.forall(f => fs1.find(_._1 === f._1).exists(_._2 <:< f._2))
        }
        
        // * Field removal is special
        // * and can't be compared without taking polarity into account...
        case (_, _: Without) => false
        /* 
        case (Without(bs1, ns1), Without(bs2, ns2)) =>
          ns1.forall(ns2) && bs1 <:< that
        case (_, Without(bs, ns)) =>
          this <:< bs
        */
        
        case (_: TypeVariable, _) | (_, _: TypeVariable) if !doCompareRecTypes => false
        case (_: TypeVariable, _) | (_, _: TypeVariable)
          if cache.contains(this -> that)
          => cache(this -> that)
        case (tv: TypeVariable, _) =>
          cache(this -> that) = false
          val tmp = tv.assignedTo match {
            case S(ty) =>
              ty <:< that
            case N =>
              tv.upperBounds.exists(_ <:< that)
          }
          if (tmp) cache(this -> that) = true
          tmp
        case (_, tv: TypeVariable) =>
          cache(this -> that) = false
          val tmp = tv.assignedTo match {
            case S(ty) =>
              this <:< ty
            case N =>
              tv.lowerBounds.exists(this <:< _)
          }
          if (tmp) cache(this -> that) = true
          tmp
        case (ConstrainedType(Nil, bod), rhs) => bod <:< that
        case (_, ConstrainedType(cs, bod)) => this <:< bod // could assume cs here
        case (_, NegType(und)) => (this & und) <:< BotType
        case (NegType(und), _) => TopType <:< (that | und)
        case (tr: TypeRef, _)
          if (primitiveTypes contains tr.defn.name) => tr.expand <:< that
        case (_, tr: TypeRef)
          if (primitiveTypes contains tr.defn.name) => this <:< tr.expand
        case (tr1: TypeRef, _) =>
          val td1 = ctx.tyDefs(tr1.defn.name)
          that match {
            case tr2: TypeRef if tr2.defn === tr1.defn =>
              val tvv = td1.getVariancesOrDefault
              td1.tparamsargs.unzip._2.lazyZip(tr1.targs).lazyZip(tr2.targs).forall { (tv, targ1, targ2) =>
                val v = tvv(tv)
                (v.isContravariant || targ1 <:< targ2) && (v.isCovariant || targ2 <:< targ1)
              }
            case _ =>
              (td1.kind is Cls) && clsNameToNomTag(td1)(noProv, ctx) <:< that
          }
        case (_, _: TypeRef) =>
          false // TODO try to expand them (this requires populating the cache because of recursive types)
        case (_: PolymorphicType, _) | (_, _: PolymorphicType) => false
        case (_, ov: Overload) => ov.alts.forall(this <:< _)
        case (ov: Overload, _) => ov.alts.exists(_ <:< that)
        case (_: ConstrainedType, _) => false
        case (_: Without, _)
          | (_: ArrayBase, _) | (_, _: ArrayBase)
          | (_: AbstractTag, _) | (_, _: AbstractTag)
          => false // don't even try
        case (_: FunctionType, _) | (_, _: FunctionType) => false
        case (_: RecordType, _: TypeTag) | (_: TypeTag, _: RecordType) => false
        case (_, ExtrType(true)) | (ExtrType(false), _) => false // not sure whether LHS <: Bot (or Top <: RHS)
        // case _ => lastWords(s"TODO $this $that ${getClass} ${that.getClass()}")
      }
    // }(r => s"! $r")
    }
    
    def isTop(implicit ctx: Ctx): Bool = TopType <:< this
    def isBot(implicit ctx: Ctx): Bool = this <:< BotType
    
    // * Sometimes, Without types are temporarily pushed to the RHS of constraints,
    // *  sometimes behind a single negation,
    // *  just for the time of massaging the constraint through a type variable.
    // * So it's important we only push and simplify Without types only in _positive_ position!
    def without(names: SortedSet[Var]): SimpleType = if (names.isEmpty) this else this match {
      case Without(b, ns) => Without(b, ns ++ names)(this.prov)
      case _ => Without(this, names)(noProv)
    }
    def without(name: Var): SimpleType = 
      without(SortedSet.single(name))
    
    def withoutPos(names: SortedSet[Var]): SimpleType = if (names.isEmpty) this else this match {
      case Without(b, ns) => Without(b, ns ++ names)(this.prov)
      case t @ FunctionType(l, r) => t
      case t @ ComposedType(true, l, r) => l.without(names) | r.without(names)
      case t @ ComposedType(false, l, r) => l.without(names) & r.without(names)
      case t @ RecordType(fs) => RecordType(fs.filter(nt => !names(nt._1)))(t.prov)
      case t @ TupleType(fs) =>
        val relevantNames = names.filter(n =>
          n.name.startsWith("_")
            && {
              val t = n.name.tail
              t.forall(_.isDigit) && {
                val n = t.toInt
                1 <= n && n <= fs.length
              }
            })
        if (relevantNames.isEmpty) t
        else {
          val rcd = t.toRecord
          rcd.copy(fields = rcd.fields.filterNot(_._1 |> relevantNames))(rcd.prov)
        }
      case t @ ArrayType(ar) => t
      case t @ SpliceType(fs) => ???  // TODO
      case n @ NegType(_ : ClassTag | _: FunctionType | _: RecordType) => n
      case n @ NegType(nt) if (nt match {
        case _: ComposedType | _: ExtrType | _: NegType => true
        // case c: ComposedType => c.pol
        // case _: ExtrType | _: NegType => true
        case _ => false
      }) => nt.neg(n.prov, force = true).withoutPos(names)
      case e @ ExtrType(_) => e // valid? -> seems we could go both ways, but this way simplifies constraint solving
      // case e @ ExtrType(false) => e
      case p @ ProvType(und) => ProvType(und.withoutPos(names))(p.prov)
      case p @ ProxyType(und) => und.withoutPos(names)
      case p: TypeTag => p
      case TypeBounds(lo, hi) => hi.withoutPos(names)
      case _: TypeVariable | _: NegType | _: TypeRef => Without(this, names)(noProv)
      case PolymorphicType(plvl, bod) => PolymorphicType.mk(plvl, bod.withoutPos(names))
      case ot: Overload => ot
      case ct: ConstrainedType => ct
    }
    def unwrapAll(implicit ctx: Ctx): SimpleType = unwrapProxies match {
      case tr: TypeRef => tr.expand.unwrapAll
      case u => u
    }
    def negNormPos(f: SimpleType => SimpleType, p: TypeProvenance)
          (implicit ctx: Ctx, ptr: PreserveTypeRefs): SimpleType = (if (preserveTypeRefs) this else unwrapAll) match {
      case ExtrType(b) => ExtrType(!b)(noProv)
      case ComposedType(true, l, r) => l.negNormPos(f, p) & r.negNormPos(f, p)
      case ComposedType(false, l, r) => l.negNormPos(f, p) | r.negNormPos(f, p)
      case NegType(n) => f(n).withProv(p)
      case tr: TypeRef if !preserveTypeRefs => tr.expand.negNormPos(f, p)
      case _: RecordType | _: FunctionType => BotType // Only valid in positive positions!
        // Because Top<:{x:S}|{y:T}, any record type negation neg{x:S}<:{y:T} for any y=/=x,
        // meaning negated records are basically bottoms.
      case rw => NegType(f(rw))(p)
    }
    def withProvOf(ty: SimpleType): ST = withProv(ty.prov)
    def withProv(p: TypeProvenance): ST = mkProxy(this, p)
    def pushPosWithout(implicit ctx: Ctx, ptr: PreserveTypeRefs): SimpleType = this match {
      case NegType(n) => n.negNormPos(_.pushPosWithout, prov)
      case Without(b, ns) => if (ns.isEmpty) b.pushPosWithout else (if (preserveTypeRefs) b.unwrapProxies else b.unwrapAll).withoutPos(ns) match {
        case Without(c @ ComposedType(pol, l, r), ns) => ComposedType(pol, l.withoutPos(ns), r.withoutPos(ns))(c.prov)
        case Without(NegType(nt), ns) => nt.negNormPos(_.pushPosWithout, nt.prov).withoutPos(ns) match {
          case rw @ Without(NegType(nt), ns) =>
            nt match {
              case _: TypeVariable | _: ClassTag | _: RecordType => rw
              case _ => if (preserveTypeRefs) rw else lastWords(s"$this  $rw  (${nt.getClass})")
            }
          case rw => rw
        }
        case rw => rw
      }
      case _ => this
    }
    
    def abs(that: SimpleType)(prov: TypeProvenance): SimpleType =
      FunctionType(this, that)(prov)
    
    def unwrapProxies: SimpleType = this match {
      case ProxyType(und) => und.unwrapProxies
      case _ => this
    }
    def unwrapProvs: SimpleType = this match {
      case ProvType(und) => und.unwrapProvs
      case _ => this
    }
    /** Used for error reporting. */
    def typeBase: SimpleType = unwrapProxies match {
      case Without(und, ns) => und.typeBase
      case PolymorphicType(pl, bod) => bod.typeBase
      case TypeBounds(lb, ub) => ub.typeBase
      case _ => this
    }
    
    def components(union: Bool): Ls[ST] = this match {
      case ExtrType(`union`) => Nil
      case ComposedType(`union`, l, r) => l.components(union) ::: r.components(union)
      case NegType(tv: TypeVariable) if !union => NegVar(tv) :: Nil
      case NegType(tt: AbstractTag) if !union => NegAbsTag(tt) :: Nil
      case ProvType(und) => und.components(union)
      case _ => this :: Nil
    }
    
    def childrenPol(pol: Opt[Bool])(implicit ctx: Ctx): List[Opt[Bool] -> SimpleType] = {
      def childrenPolField(fld: FieldType): List[Opt[Bool] -> SimpleType] =
        fld.lb.map(pol.map(!_) -> _).toList ::: pol -> fld.ub :: Nil
      this match {
        case tv @ AssignedVariable(ty) =>
          pol -> ty :: Nil
        case tv: TypeVariable =>
          (if (pol =/= S(false)) tv.lowerBounds.map(S(true) -> _) else Nil) :::
          (if (pol =/= S(true)) tv.upperBounds.map(S(false) -> _) else Nil)
        case FunctionType(l, r) => pol.map(!_) -> l :: pol -> r :: Nil
        case Overload(as) => as.map(pol -> _)
        case ComposedType(_, l, r) => pol -> l :: pol -> r :: Nil
        case RecordType(fs) => fs.unzip._2.flatMap(childrenPolField)
        case TupleType(fs) => fs.unzip._2.flatMap(childrenPolField)
        case ArrayType(fld) => childrenPolField(fld)
        case SpliceType(elems) => elems flatMap {case L(l) => pol -> l :: Nil case R(r) => childrenPolField(r)}
        case NegType(n) => pol.map(!_) -> n :: Nil
        case ExtrType(_) => Nil
        case ProxyType(und) => pol -> und :: Nil
        case SkolemTag(id) => pol -> id :: Nil
        case _: TypeTag => Nil
        case tr: TypeRef => tr.mapTargs(pol)(_ -> _)
        case Without(b, ns) => pol -> b :: Nil
        case TypeBounds(lb, ub) => S(false) -> lb :: S(true) -> ub :: Nil
        case PolymorphicType(_, und) => pol -> und :: Nil
        case ConstrainedType(cs, bod) =>
          cs.flatMap(vbs => S(true) -> vbs._1 :: S(false) -> vbs._2 :: Nil) ::: pol -> bod :: Nil
    }}
    
    def childrenPol(pol: PolMap)(implicit ctx: Ctx): List[PolMap -> SimpleType] = {
      def childrenPolField(fld: FieldType): List[PolMap -> SimpleType] =
        fld.lb.map(pol.contravar -> _).toList ::: pol.covar -> fld.ub :: Nil
      this match {
        case tv @ AssignedVariable(ty) =>
          pol -> ty :: Nil
        case tv: TypeVariable =>
          (if (pol(tv) =/= S(false)) tv.lowerBounds.map(pol.at(tv.level, true) -> _) else Nil) :::
          (if (pol(tv) =/= S(true)) tv.upperBounds.map(pol.at(tv.level, false) -> _) else Nil)
        case FunctionType(l, r) => pol.contravar -> l :: pol.covar -> r :: Nil
        case Overload(as) => as.map(pol -> _)
        case ComposedType(_, l, r) => pol -> l :: pol -> r :: Nil
        case RecordType(fs) => fs.unzip._2.flatMap(childrenPolField)
        case TupleType(fs) => fs.unzip._2.flatMap(childrenPolField)
        case ArrayType(fld) => childrenPolField(fld)
        case SpliceType(elems) => elems flatMap {case L(l) => pol -> l :: Nil case R(r) => childrenPolField(r)}
        case NegType(n) => pol.contravar -> n :: Nil
        case ExtrType(_) => Nil
        case ProxyType(und) => pol -> und :: Nil
        case SkolemTag(id) => pol -> id :: Nil
        case _: TypeTag => Nil
        case tr: TypeRef => tr.mapTargs(pol)(_ -> _)
        case Without(b, ns) => pol -> b :: Nil
        case TypeBounds(lb, ub) => PolMap.neg -> lb :: PolMap.pos -> ub :: Nil
        case PolymorphicType(_, und) => pol -> und :: Nil
        case ConstrainedType(cs, bod) =>
          cs.flatMap(vbs => PolMap.pos -> vbs._1 :: PolMap.posAtNeg -> vbs._2 :: Nil) ::: pol -> bod :: Nil
    }}
    
    /** `ignoreTopLevelOccs` is used to discard immediate occurrences of a variable which
      * would constitute spurious recursive occurrences when traversing the variable's bounds. */
    def getVarsPol(pol: PolMap, ignoreTopLevelOccs: Bool = false, ignoreQuantifiedVars: Bool = false)
          (implicit ctx: Ctx)
          : SortedMap[TypeVariable, Opt[Bool]] = {
      val res = MutMap.empty[TV, Pol]
      val traversed = MutSet.empty[TV -> Bool]
      // * We ignore variables above `upperLvl` when `ignoreQuantifiedVars` is true. 
      def go(pol: PolMap, ignoreTLO: Bool, upperLvl: Level)(ty: ST): Unit = {
        trace(s"getVarsPol[${printPol(pol.base)}] $ty ${pol} $ignoreTLO") {
        ty match {
          case tv: TypeVariable =>
            if (ignoreQuantifiedVars && tv.level > upperLvl) return println(s"Quantified! ${tv}")
            val tvpol = pol(tv.level)
            if (!ignoreTLO) res.updateWith(tv) {
              case S(p) if p =/= tvpol =>
                // println(s"> $tv: $tvpol =/= $p ~> S(N)")
                S(N)
              case _ =>
                // println(s"> $tv: $tvpol")
                S(tvpol)
            }
            val needsTraversing = tvpol match {
              case S(p) =>
                !traversed(tv -> p) && {
                  traversed += tv -> p
                  true
                }
              case N =>
                !(traversed(tv -> true) && traversed(tv -> false)) && {
                  traversed += tv -> true
                  traversed += tv -> false
                  true
                }
            }
            // println(s"$tv ${printPol(tvpol)} $needsTraversing")
            if (needsTraversing)
              tv.childrenPol(pol) // * Note: `childrenPol` deals with `assignedTo`
                .foreach(cp => go(cp._1, ignoreTLO, upperLvl)(cp._2))
          case ProxyType(und) => go(pol, ignoreTLO, upperLvl)(und)
          case Overload(as) => as.foreach(go(pol, ignoreTLO, upperLvl))
          case NegType(n) => go(pol.contravar, ignoreTLO, upperLvl)(n)
          case Without(b, ns) => go(pol, ignoreTLO, upperLvl)(b)
          case TypeBounds(lb, ub) =>
            go(PolMap.neg, ignoreTLO, upperLvl)(lb)
            go(PolMap.pos, ignoreTLO, upperLvl)(ub)
            // * or simply:
            // pol.traverseRange(lb, ub)(go(_, ignoreTLO, upperLvl)(_))
          case ConstrainedType(cs, bod) =>
            cs.foreach { vbs =>
              go(PolMap.pos, false, upperLvl)(vbs._1)
              go(PolMap.posAtNeg, false, upperLvl)(vbs._2)
            }
            go(pol, ignoreTLO, upperLvl)(bod)
          case ComposedType(p, l, r) =>
            go(pol, ignoreTLO, upperLvl)(l)
            go(pol, ignoreTLO, upperLvl)(r)
          case pt: PolymorphicType =>
            go(pol.enter(pt.polymLevel), ignoreTLO, upperLvl min pt.polymLevel)(pt.body)
          case ty =>
            ty.childrenPol(pol).foreach(cp => go(cp._1,
              // * We should have handled above all top-level cases,
              // * so the children here are not supposed to be top level.
              // * Note: We won't get unsoundness if we forgot cases,
              // * just spurious occurs-check failures!
              ignoreTLO = false, upperLvl)(cp._2))
        }
        }()
      }
      go(pol, ignoreTopLevelOccs, MaxLevel)(this)
      res.toSortedMap
    }
    
    def children(includeBounds: Bool): List[SimpleType] = this match {
      case tv @ AssignedVariable(ty) => if (includeBounds) ty :: Nil else Nil
      case tv: TypeVariable => if (includeBounds) tv.lowerBounds ::: tv.upperBounds else Nil
      case FunctionType(l, r) => l :: r :: Nil
      case Overload(as) => as
      case ComposedType(_, l, r) => l :: r :: Nil
      case RecordType(fs) => fs.flatMap(f => f._2.lb.toList ::: f._2.ub :: Nil)
      case TupleType(fs) => fs.flatMap(f => f._2.lb.toList ::: f._2.ub :: Nil)
      case ArrayType(inner) => inner.lb.toList ++ (inner.ub :: Nil)
      case NegType(n) => n :: Nil
      case ExtrType(_) => Nil
      case ProxyType(und) => und :: Nil
      case SkolemTag(id) => id :: Nil
      case _: TypeTag => Nil
      case TypeRef(d, ts) => ts
      case Without(b, ns) => b :: Nil
      case TypeBounds(lb, ub) => lb :: ub :: Nil
      case PolymorphicType(_, und) => und :: Nil
      case ConstrainedType(cs, und) => cs.flatMap(lu => lu._1 :: lu._2 :: Nil) ::: und :: Nil
      case SpliceType(fs) => fs.flatMap{ case L(l) => l :: Nil case R(r) => r.lb.toList ::: r.ub :: Nil}
    }
    
    def getVars: SortedSet[TypeVariable] = {
      val res = MutSet.empty[TypeVariable]
      @tailrec def rec(queue: List[SimpleType]): Unit = queue match {
        case (tv: TypeVariable) :: tys =>
          if (res(tv)) rec(tys)
          else { res += tv; rec(tv.children(includeBounds = true) ::: tys) }
        case ty :: tys => rec(ty.children(includeBounds = true) ::: tys)
        case Nil => ()
      }
      rec(this :: Nil)
      SortedSet.from(res)(Ordering.by(_.uid))
    }
    
    /** (exclusive, inclusive) */
    def varsBetween(lb: Level, ub: Level): Set[TV] = {
      val res = MutSet.empty[TypeVariable]
      val traversed = MutSet.empty[TypeVariable]
      def go(ty: ST, lb: Level, ub: Level): Unit =
          if (lb < ub && ty.level > lb) { // * Don't traverse types with lower levels
        // trace(s"varsBetween($ty, $lb, $ub)") {
        ty match {
          case tv: TypeVariable =>
            if (traversed(tv)) ()
            else {
              traversed += tv
              if (tv.level > lb && tv.level <= ub) {
                // println(s"ADD $tv")
                res += tv
              }
              tv.children(includeBounds = true) // * Note: `children` deals with `assignedTo`
                .foreach(go(_, lb, ub))
            }
          case pt: PolymorphicType =>
            go(pt.body, lb, pt.polymLevel min ub)
          case ty =>
            ty.children(includeBounds = true) // * Q: is `includeBounds` useful here?
              .foreach(go(_, lb, ub))
        }
        // }()
      }
      go(this, lb, ub)
      res.toSet
    }
    
    def showBounds: String =
      getVars.iterator.filter(tv => tv.assignedTo.nonEmpty || (tv.upperBounds ++ tv.lowerBounds).nonEmpty).map {
      case tv @ AssignedVariable(ty) => "\n\t\t" + tv.toString + " := " + ty
      case tv => ("\n\t\t" + tv.toString
          + (if (tv.lowerBounds.isEmpty) "" else " :> " + tv.lowerBounds.mkString(" | "))
          + (if (tv.upperBounds.isEmpty) "" else " <: " + tv.upperBounds.mkString(" & ")))
      }.mkString
    
    def expPos(implicit ctx: Ctx): Type = exp(S(true), this)
    def expNeg(implicit ctx: Ctx): Type = exp(S(false), this)
    def exp(pol: Opt[Bool], ty: ST)(implicit ctx: Ctx): Type = (
      ty
      // |> (_.normalize(false))
      // |> (simplifyType(_, pol, removePolarVars = false, inlineBounds = false))
      // |> (shallowCopy)
      |> (subst(_, Map.empty)) // * Make a copy of the type and its TV graph – although we won't show the TV bounds, we still care about the bounds as they affect class type reconstruction in normalizeTypes_!
      |> (normalizeTypes_!(_, pol)(ctx))
      |> (expandType(_, stopAtTyVars = true))
    )
    
  }
  
  
  def merge(pol: Bool, ts: Ls[ST]): ST =
    if (pol) ts.foldLeft(BotType: ST)(_ | _)
    else ts.foldLeft(TopType: ST)(_ & _)
  
  
  class Traverser(implicit ctx: Ctx) {
    def apply(pol: Opt[Bool])(st: ST): Unit = st match {
      case tv @ AssignedVariable(ty) => apply(pol)(ty)
      case tv: TypeVariable =>
        if (pol =/= S(false)) tv.lowerBounds.foreach(apply(S(true)))
        if (pol =/= S(true)) tv.upperBounds.foreach(apply(S(false)))
      case FunctionType(l, r) => apply(pol.map(!_))(l); apply(pol)(r)
      case Overload(as) => as.foreach(apply(pol))
      case ComposedType(_, l, r) => apply(pol)(l); apply(pol)(r)
      case RecordType(fs) => fs.unzip._2.foreach(applyField(pol))
      case TupleType(fs) => fs.unzip._2.foreach(applyField(pol))
      case ArrayType(fld) => applyField(pol)(fld)
      case SpliceType(elems) => elems foreach {case L(l) => apply(pol)(l) case R(r) => applyField(pol)(r)}
      case NegType(n) => apply(pol.map(!_))(n)
      case ExtrType(_) => ()
      case ProxyType(und) => apply(pol)(und)
      case SkolemTag(id) => apply(pol)(id)
      case _: TypeTag => ()
      case tr: TypeRef => tr.mapTargs(pol)(apply(_)(_)); ()
      case Without(b, ns) => apply(pol)(b)
      case TypeBounds(lb, ub) =>
        if (pol =/= S(true)) apply(S(false))(lb)
        if (pol =/= S(false)) apply(S(true))(ub)
      case PolymorphicType(plvl, und) => apply(pol)(und)
      case ConstrainedType(cs, bod) =>
        cs.foreach {
          case (lo, hi) =>
            apply(S(true))(lo)
            apply(S(false))(hi)
        }
        apply(pol)(bod)
    }
    def applyField(pol: Opt[Bool])(fld: FieldType): Unit = {
      fld.lb.foreach(apply(pol.map(!_)))
      apply(pol)(fld.ub)
    }
  }
  object Traverser {
    trait InvariantFields extends Traverser {
      override def applyField(pol: Opt[Bool])(fld: FieldType): Unit =
        if (fld.lb.exists(_ === fld.ub)) apply(N)(fld.ub)
        else super.applyField(pol)(fld)
    }
  }
  
  class Traverser2(implicit ctx: Ctx) {
    def apply(pol: PolMap)(st: ST): Unit = st match {
      case tv @ AssignedVariable(ty) => apply(pol)(ty)
      case tv: TypeVariable =>
        if (pol(tv) =/= S(false)) tv.lowerBounds.foreach(apply(pol.at(tv.level, true)))
        if (pol(tv) =/= S(true)) tv.upperBounds.foreach(apply(pol.at(tv.level, false)))
      case FunctionType(l, r) => apply(pol.contravar)(l); apply(pol)(r)
      case Overload(as) => as.foreach(apply(pol))
      case ComposedType(_, l, r) => apply(pol)(l); apply(pol)(r)
      case RecordType(fs) => fs.unzip._2.foreach(applyField(pol))
      case TupleType(fs) => fs.unzip._2.foreach(applyField(pol))
      case ArrayType(fld) => applyField(pol)(fld)
      case SpliceType(elems) => elems foreach {case L(l) => apply(pol)(l) case R(r) => applyField(pol)(r)}
      case NegType(n) => apply(pol.contravar)(n)
      case ExtrType(_) => ()
      case ProxyType(und) => apply(pol)(und)
      case SkolemTag(id) => apply(pol)(id)
      case _: TypeTag => ()
      case tr: TypeRef => tr.mapTargs(pol)(apply(_)(_)); ()
      case Without(b, ns) => apply(pol)(b)
      case TypeBounds(lb, ub) => pol.traverseRange(lb, ub)(apply(_)(_))
      case PolymorphicType(plvl, und) => apply(pol.enter(plvl))(und)
      case ConstrainedType(cs, bod) =>
        cs.foreach {
          case (lo, hi) =>
            apply(PolMap.pos)(lo)
            apply(PolMap.posAtNeg)(hi)
        }
        apply(pol)(bod)
    }
    def applyField(pol: PolMap)(fld: FieldType): Unit = {
      fld.lb.foreach(apply(pol.contravar))
      apply(pol)(fld.ub)
    }
  }
  object Traverser2 {
    trait InvariantFields extends Traverser2 {
      override def applyField(pol: PolMap)(fld: FieldType): Unit =
        if (fld.lb.exists(_ === fld.ub)) apply(pol.invar)(fld.ub)
        else super.applyField(pol)(fld)
    }
  }
  
  
  object PolyFunction {
    def unapply(ty: ST)(implicit ctx: Ctx, raise: Raise, shadows: Shadows): Opt[PolymorphicType] = {
      def go(ty: ST, traversed: Set[AnyRef]): Opt[PolymorphicType] = //trace(s"go $ty") {
          if (!distributeForalls) N else ty match {
        case poly @ PolymorphicType(plvl, bod) => S(poly)
        case tr: TypeRef if !traversed.contains(tr.defn) => go(tr.expand, traversed + tr.defn)
        case proxy: ProxyType => go(proxy.underlying, traversed)
        case tv @ AssignedVariable(ty) if !traversed.contains(tv) =>
          go(ty, traversed + tv)
        case ft @ FunctionType(param, funbod) =>
            for { poly @ PolymorphicType(plvl, bod) <- go(funbod, traversed) }
            yield {
              val newRhs = if (param.level > plvl) {
                  val poly2 = poly.raiseLevelTo(param.level)
                  PolymorphicType(poly2.polymLevel, FunctionType(param, poly2.body)(ft.prov))
                } else PolymorphicType(plvl, FunctionType(param, bod)(ft.prov))
              newRhs
            }
        case _ => N
      }
      // }(res => s"= $res")
      go(ty, Set.empty)
    }
  }
  object AliasOf {
    def unapply(ty: ST)(implicit ctx: Ctx): S[ST] = {
      def go(ty: ST, traversedVars: Set[TV]): S[ST] = ty match {
        case tr: TypeRef => go(tr.expand, traversedVars)
        case proxy: ProxyType => go(proxy.underlying, traversedVars)
        case tv @ AssignedVariable(ty) if !traversedVars.contains(tv) =>
          go(ty, traversedVars + tv)
        case _ => S(ty)
      }
      go(ty, Set.empty)
    }
  }
  
  
  protected def showLevel(level: Level): Str =
    (if (level === MaxLevel) "^" else if (level > 5 ) "^" + level else "'" * level)
  
  
  
  /** This class helps keep track of the *relative polarity* of different [olymorphism levels.
    * Polarity is relative because polymorphic types can exist at arbitrary polarities,
    * yet the type variables they quantify have their polarity determined by their local
    * use in the polymorphic type.
    * Things get quite involved once type ranges are involved. */
  abstract class PolMap(val base: Pol) { outer =>
    private val ctx = 0
    private val lvl = 0
    def apply(lvl: Level): Pol
    def quantifPolarity(lvl: Level): PolMap
    final def apply(tv: TV): Pol = apply(tv.level)
    def enter(polymLvl: Level): PolMap =
      new PolMap(base) {
        def apply(lvl: Level): Pol =
          if (lvl > polymLvl) S(true) else outer(lvl)
        def quantifPolarity(lvl: Level): PolMap =
          if (lvl > polymLvl) this else outer.quantifPolarity(lvl)
        def show: Str = s"$outer;Q($lvl)"
      }
    def invar: PolMap =
      new PolMap(N) {
        def apply(lvl: Level): Pol = N
        def quantifPolarity(lvl: Level): PolMap =
          outer.quantifPolarity(lvl)
        def show: Str = s"$outer;="
      }
    def covar: PolMap = this
    def contravar: PolMap =
      new PolMap(base.map(!_)) {
        def apply(lvl: Level): Pol =
          outer(lvl).map(!_)
        def quantifPolarity(lvl: Level): PolMap =
          outer.quantifPolarity(lvl)
        def show: Str = s"$outer;-"
      }
    /** Used to traverse type variable bounds.
      * The tricky part is that when a TV is quantified negatively,
      * then from the POV of lower-level variables,
      * the polarity of its upper bound is actually POSITIVE! */
    def at(atLvl: Level, pol: Bool): PolMap =
      new PolMap(base) {
        val pm = quantifPolarity(atLvl)
        def apply(lvl: Level): Pol =
          // * Everything above or at `atLvl` gets the new polarity pol;
          // * things under it get the new polarity unless atLvl is quantified negatively,
          // * in which case they get the opposite polarity !pol.
          if (lvl >= atLvl) S(pol) else pm(lvl) match {
            case S(true) => S(pol)
            case S(false) => S(!pol)
            case N => N
          }
        def quantifPolarity(lvl: Level): PolMap =
          outer.quantifPolarity(lvl)
        def show: Str = s"$outer;@[${printPol(S(pol))}]($lvl)"
      }
    
    def traverseRange(lb: ST, ub: ST)(k: (PolMap, ST) => Unit): Unit = {
      if (base =/= S(true)) k(PolMap.neg, lb)
      if (base =/= S(false)) k(PolMap.pos, ub)
    }
    // * Note: We used to have this weird impelmentation,
    // * which was not consistent with the other places in the code where
    // * we traversed TypeBound instances...
    // * I am still unsure what's the proper way of traversing them
    // * and it's possible there was some truth in this implementation,
    // * but I no longer remember how it was justified.
    /* 
    def traverseRange(lb: ST, ub: ST)(k: (PolMap, ST) => Unit): Unit = {
      def polma(p: Bool) = new PolMap(S(p)) {
        def apply(lvl: Level): Pol =
          outer.quantifPolarity(lvl).base match {
            case N => N
            case S(true) => S(p)
            case S(false) => S(!p)
          }
        def quantifPolarity(lvl: Level): PolMap =
          outer.quantifPolarity(lvl)
        def show: Str = s"$outer;R${printPol(S(p))}"
      }
      if (base =/= S(true)) k(polma(false), lb)
      if (base =/= S(false)) k(polma(true), ub)
    }
    */
    
    protected def show: Str
    override def toString: String = show
  }
  object PolMap {
    private def mk(init: Pol): PolMap = new PolMap(init) {
      def apply(lvl: Level): Pol = init
      def quantifPolarity(lvl: Level): PolMap = this
      def show: Str = s"${printPol(init)}"
    }
    val pos: PolMap = mk(S(true))
    val neg: PolMap = mk(S(false))
    val posAtNeg: PolMap = pos.at(MinLevel, false)
    val neu: PolMap = mk(N)
    def apply(init: Pol = S(true)): PolMap = init match {
      case S(true) => pos
      case S(false) => neg
      case N => neu
    }
  }
  
  
  
}
