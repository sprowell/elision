/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.context

import ornl.elision.core.Literal
import ornl.elision.core.BasicAtom
import ornl.elision.core.AlgProp
import ornl.elision.core.Bindings
import ornl.elision.core.Apply
import ornl.elision.util.Loc
import ornl.elision.core.AtomSeq
import ornl.elision.core.BindingsAtom
import ornl.elision.core.Lambda
import ornl.elision.core.MapPair
import ornl.elision.core.SpecialForm
import ornl.elision.core.TermVariable
import ornl.elision.core.MetaVariable
import ornl.elision.core.OperatorRef
import ornl.elision.core.IntegerLiteral
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.StringLiteral
import ornl.elision.core.BitStringLiteral
import ornl.elision.core.FloatLiteral
import ornl.elision.util.ElisionException
import ornl.elision.core.Variable
import ornl.elision.core.NamedRootType
import ornl.elision.core.OpApply
import ornl.elision.util.OmitSeq
import ornl.elision.core.GuardStrategy

/**
 * An evaluator adds the logic for doing rewriting and replacement to the
 * atom builder.  As such, it is just an intermediate class and is not
 * necessarily of interest unless you wish to make guarantees about the
 * result of rewriting something - that is, that applying bindings to an
 * atom sequence will always yield an atom sequence.
 * 
 * Apply a binding to an atom, replacing instances of bound variables with
 * their corresponding binding, and evaluating the result.  This class also
 * provides a more general - and dangerous - method to replace one atom with
 * another.
 * 
 * To use this, make an instance and then invoke either
 * the `apply` method to rewrite an atom, or the `replace` method to replace
 * atoms.  If you care about the specific result, you might try one of the
 * type-specific `rewriteAs` methods.  These make guarantees about the return
 * value's type.
 */
abstract class Evaluator extends Builder {
  
  def apply(atom: BasicAtom, binds: Bindings,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    rewrite(atom, binds, strategy)
  }

  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(atom: BasicAtom, binds: Bindings,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    atom match {
      case lit: IntegerLiteral => rewrite(lit, binds, strategy)
      
      case lit: SymbolLiteral => rewrite(lit, binds, strategy)
      
      case lit: StringLiteral => rewrite(lit, binds, strategy)
      
      case lit: BitStringLiteral => rewrite(lit, binds, strategy)
      
      case lit: FloatLiteral => rewrite(lit, binds, strategy)
      
      case ap: AlgProp => rewrite(ap, binds, strategy)
      
      case opapp: OpApply => rewrite(opapp, binds, strategy)

      case app: Apply => rewrite(app, binds, strategy)
        
      case as: AtomSeq => rewrite(as, binds, strategy)

      case ba: BindingsAtom => rewrite(ba, binds, strategy)

      case lam: Lambda => rewrite(lam, binds, strategy)
        
      case map: MapPair => rewrite(map, binds, strategy)
        
      case sf: SpecialForm => rewrite(sf, binds, strategy)
      
      case or: OperatorRef => rewrite(or, binds, strategy)
        
      case vari: TermVariable => rewrite(vari, binds, strategy)
        
      case vari: MetaVariable => rewrite(vari, binds, strategy)
        
      case _ =>
        throw new ElisionException(Loc.internal,
            "Do not know how to rewrite the atom "+atom.toParseString+".")
    }
  }
  
  /**
   * Rewrite the provided atom by applying the replacements defined in the
   * map.  The atom itself may be in the map, in which case its replacement
   * is returned.  This is a "one pass" replacement.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def replace(atom: BasicAtom, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    atom match {
      case lit: IntegerLiteral => _replace(lit, map, strategy)
      
      case lit: SymbolLiteral => _replace(lit, map, strategy)
      
      case lit: StringLiteral => _replace(lit, map, strategy)
      
      case lit: BitStringLiteral => _replace(lit, map, strategy)
      
      case lit: FloatLiteral => _replace(lit, map, strategy)
      
      case ap: AlgProp => _replace(ap, map, strategy)

      case app: Apply => _replace(app, map, strategy)
        
      case as: AtomSeq => _replace(as, map, strategy)

      case ba: BindingsAtom => _replace(ba, map, strategy)

      case lam: Lambda => _replace(lam, map, strategy)
        
      case mp: MapPair => _replace(mp, map, strategy)
        
      case sf: SpecialForm => _replace(sf, map, strategy)
      
      case or: OperatorRef => _replace(or, map, strategy)
        
      case vari: TermVariable => _replace(vari, map, strategy)
        
      case vari: MetaVariable => _replace(vari, map, strategy)
        
      case _ =>
        throw new ElisionException(Loc.internal,
            "Do not know how to rewrite the atom "+atom.toParseString+".")
    }
  }
  
  //======================================================================
  // Rewrite methods.
  //======================================================================
  
  /**
   * Rewrite a sequence of atoms by applying the given bindings to each.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the rewritten sequence of atoms and a flag
   *          that is true if any rewrites succeeded.
   */
  def rewrite(subjects: OmitSeq[BasicAtom], binds: Bindings,
      strategy: GuardStrategy) = {
    var changed = false
    var index = 0
    var newseq = OmitSeq[BasicAtom]()
    while (index < subjects.size) {
      val (newatom, change) = apply(subjects(index), binds, strategy)
      changed |= change
      newseq :+= newatom
      index += 1
    } // Rewrite the subjects.
    if (changed) (newseq, changed) else (subjects, false)
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(nrt: NamedRootType, binds: Bindings,
      strategy: GuardStrategy): (NamedRootType, Boolean) = {
    (nrt, false)
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(ap: AlgProp, binds: Bindings,
      strategy: GuardStrategy): (AlgProp, Boolean) = {
    def _rewrite(opt: Option[BasicAtom]) = {
      opt match {
        case None => 
          (None, false)
          
        case Some(atom) => {
          val newatom = apply(atom, binds, strategy)
          (Some(newatom._1), newatom._2)
        }
      }
    }
    val assoc = _rewrite(ap.associative)
    val commu = _rewrite(ap.commutative)
    val idemp = _rewrite(ap.idempotent)
    val absor = _rewrite(ap.absorber)
    val ident = _rewrite(ap.identity)
    if (assoc._2 || commu._2 || idemp._2 || absor._2 || ident._2) {
      (newAlgProp(Loc.internal, assoc._1, commu._1, idemp._1, absor._1,
          ident._1), true)
    } else {
      (ap, false)
    }
  }
  
  /**
   * Rewrite an operator application.  These are special, since the operator
   * is never rewritten, and there may be cached rewrites.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(opapp: OpApply, binds: Bindings,
      strategy: GuardStrategy) = {
    // TODO Does caching rewrites here actually yield any performance?
    // TODO Should all rewrites be cached?
    
    // We have bindings. Rewrite the operator.
    // See if we have already rewritten this operator with these
    // bindings.
    (binds.rewrites get opapp) match {
      // We have already done this rewrite.
      case Some(rewrite) =>
        rewrite
      
      // We don't have a cached rewrite.
      case None =>
        // Rewrite the argument, but not the operator.  In reality, operators
        // should protect their arguments using De Bruijn indices, but that's
        // not implemented.
        val pair = apply(opapp.arg, binds, strategy)
        if (pair._2) {
          val napply = newApply(Loc.internal, opapp.op, pair._1, strategy)
          binds.rewrites(opapp) = (napply, true) 
          (napply, true) 
        } else {
          binds.rewrites(opapp) = (opapp, false) 
          (this, false)
        }
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(app: Apply, binds: Bindings,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    val (nop, nof) = apply(app.op, binds, strategy)
    val (narg, naf) = apply(app.arg, binds, strategy)
    if (nof || naf) {
      (newApply(Loc.internal, nop, narg, strategy), true)
    } else { 
      (app, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(as: AtomSeq, binds: Bindings,
      strategy: GuardStrategy): (AtomSeq, Boolean) = {
    // Rewrite the properties.
    val (newprop, pchanged) = rewrite(as.props, binds, strategy)
    
    // We must rewrite every child atom, and collect them into a new sequence.
    var schanged = false
    val newseq = as.atoms map {
      atom =>
        val (newatom, changed) = apply(atom, binds, strategy)
        schanged |= changed
        newatom
    }
    
    // If anything changed, make a new sequence.
    if (pchanged || schanged) {
      (newAtomSeq(Loc.internal, newprop, newseq), true)
    } else {
      (as, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(ba: BindingsAtom, binds: Bindings,
      strategy: GuardStrategy): (BindingsAtom, Boolean) = {
    var changed = false
    var newmap = Bindings()
    for ((key, value) <- ba.mybinds) { 
      val (newvalue, valuechanged) = apply(value, binds, strategy)
      changed |= valuechanged
      newmap += (key -> newvalue)
    } // Rewrite all bindings.
    
    if (changed) {
      (newBindingsAtom(Loc.internal, newmap), true) 
    } else {
      (ba, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lam: Lambda, binds: Bindings,
      strategy: GuardStrategy): (Lambda, Boolean) = {
    // We test for a special case here.  If the bindings specify that we
    // should rewrite our own bound De Bruijn index, we explicitly ignore
    // it.
    val newbinds = binds - lam.lvar.name
    apply(lam.body, newbinds, strategy) match {
      case (newatom, changed) if changed => 
        (newLambda(Loc.internal, lam.lvar, newatom, strategy), true)
        
      case _ => 
        (lam, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: IntegerLiteral, binds: Bindings,
      strategy: GuardStrategy): (IntegerLiteral, Boolean) = {
    apply(lit.theType, binds, strategy) match {
      case (newtype, true) =>
        (newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: SymbolLiteral, binds: Bindings,
      strategy: GuardStrategy): (SymbolLiteral, Boolean) = {
    apply(lit.theType, binds, strategy) match {
      case (newtype, true) =>
        (newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: StringLiteral, binds: Bindings,
      strategy: GuardStrategy): (StringLiteral, Boolean) = {
    apply(lit.theType, binds, strategy) match {
      case (newtype, true) =>
        (newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: BitStringLiteral, binds: Bindings,
      strategy: GuardStrategy): (BitStringLiteral, Boolean) = {
    apply(lit.theType, binds, strategy) match {
      case (newtype, true) =>
        (newLiteral(Loc.internal, newtype, lit.bits, lit.len), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: FloatLiteral, binds: Bindings,
      strategy: GuardStrategy): (FloatLiteral, Boolean) = {
    apply(lit.theType, binds, strategy) match {
      case (newtype, true) =>
        (newLiteral(Loc.internal, newtype, lit.significand,
            lit.exponent, lit.radix), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(map: MapPair, binds: Bindings,
      strategy: GuardStrategy): (MapPair, Boolean) = {
    val newleft = apply(map.left, binds, strategy)
    val newright = apply(map.right, binds, strategy)
    if (newleft._2 || newright._2) {
      (newMapPair(Loc.internal, newleft._1, newright._2), true)
    } else {
      (map, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(or: OperatorRef, binds: Bindings,
      strategy: GuardStrategy): (OperatorRef, Boolean) = {
    // Operator references cannot be rewritten... which is actually why they
    // exist!
    (or, false)
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(sf: SpecialForm, binds: Bindings,
      strategy: GuardStrategy): (SpecialForm, Boolean) = {
    val newtag = apply(sf.tag, binds, strategy)
    val newcontent = apply(sf.content, binds, strategy)
    if (newtag._2 || newcontent._2) {
      (newSpecialForm(Loc.internal, newtag._1, newcontent._1, strategy), true)
    } else {
      (sf, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(vari: TermVariable, binds: Bindings,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    // If we have no bindings, don't rewrite the variable.
    if (binds == null) {
      (vari, false)
    } else {
      // If this variable is bound in the provided bindings, replace it with
      // the bound value.
      binds.get(vari.name) match {
        case Some(atom) =>
          (atom, true)
        
        case None => {
          // Though the atom is not bound, its type still might have to be
          // rewritten.
          apply(vari.theType, binds, strategy) match {
            case (newtype, true) =>
              (newTermVariable(Loc.internal, newtype, vari.name,
                  vari.guard, vari.labels, vari.byName), true)
            
            case _ => {
              (vari, false)
            }
          }
        }
      }
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom      The atom to rewrite.
   * @param binds     The bindings.
   * @param strategy  The guard strategy required to create rewrite rules.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(vari: MetaVariable, binds: Bindings,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    // If we have no bindings, don't rewrite the variable.
    if (binds == null) {
      (vari, false)
    } else {
      // If this variable is bound in the provided bindings, replace it with
      // the bound value.
      binds.get(vari.name) match {
        case Some(atom) =>
          (atom, true)
        
        case None => {
          // Though the atom is not bound, its type still might have to be
          // rewritten.
          apply(vari.theType, binds, strategy) match {
            case (newtype, true) =>
              (newMetaVariable(Loc.internal, newtype, vari.name,
                  vari.guard, vari.labels, vari.byName), true)
            
            case _ => {
              (vari, false)
            }
          }
        }
      }
    }
  }
  
  //======================================================================
  // Replace methods.
  //======================================================================
  
  private def _replace(ap: AlgProp, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    def _replace(opt: Option[BasicAtom]) = opt match {
      case None =>
        (None, false)
        
      case Some(atom) =>
        val (newatom, flag) = replace(atom, map, strategy)
        (Some(newatom), flag)
    }
    map.get(ap) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newA, flagA) = _replace(ap.associative)
        val (newC, flagC) = _replace(ap.commutative)
        val (newI, flagI) = _replace(ap.idempotent)
        val (newB, flagB) = _replace(ap.absorber)
        val (newD, flagD) = _replace(ap.identity)
        if (flagA || flagC || flagI || flagB || flagD) {
          (newAlgProp(Loc.internal, newA, newC, newI, newB, newD), true)
        } else {
          (ap, false)
        }
    }
  }
  
  private def _replace(app: Apply, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(app) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newop, flag1) = replace(app.op, map, strategy)
        val (newarg, flag2) = replace(app.arg, map, strategy)
        if (flag1 || flag2) {
          (newApply(Loc.internal, newop, newarg, strategy), true)
        } else {
          (app, false)
        }
    }
  }
  
  private def _replace(as: AtomSeq, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(as) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        var flag1 = false
        val newatoms = as.atoms map {
          atom =>
            val (newatom, changed) = replace(atom, map, strategy)
            flag1 |= changed
            newatom
        }
        // The algebraic properties must rewrite to a valid algebraic
        // properties atom, or we must discard it since we cannot build a
        // legal algebraic properties atom otherwise.
        val (newprops, flag2) = replace(as.props, map, strategy) match {
          case (ap: AlgProp, flag: Boolean) => (ap, flag)
          case _ => (as.props, false)
        }
        if (flag1 || flag2) {
          (newAtomSeq(Loc.internal, newprops, newatoms), true)
        } else {
          (as, false)
        }
    }
  }
  
  private def _replace(ba: BindingsAtom, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(ba) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        var flag = false
        val newbinds = ba.mybinds map {
          bind =>
            val (newbind, changed) = replace(bind._2, map, strategy)
            flag |= changed
            (bind._1, newbind)
        }
        if (flag) {
          (newBindingsAtom(Loc.internal, newbinds), true)
        } else {
          (ba, false)
        }
    }
  }
  
  private def _replace(lam: Lambda, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lam) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newvar, flag) = replace(lam.lvar, map, strategy)
        val (newlvar, flag1) =
          (if (newvar.isInstanceOf[Variable])
            (newvar.asInstanceOf[Variable], flag) else (lam.lvar, false))
        val (newbody, flag2) = replace(lam.body, map, strategy)
        if (flag1 || flag2) {
          (newLambda(Loc.internal, newlvar, newbody, strategy), true)
        } else {
          (lam, false)
        }
    }
  }
  
  private def _replace(lit: IntegerLiteral, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map, strategy)
        if (flag) {
          (newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: SymbolLiteral, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map, strategy)
        if (flag) {
          (newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: StringLiteral, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map, strategy)
        if (flag) {
          (newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: BitStringLiteral, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map, strategy)
        if (flag) {
          (newLiteral(Loc.internal, newtype, lit.bits, lit.len), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: FloatLiteral, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map, strategy)
        if (flag) {
          (newLiteral(Loc.internal, newtype, lit.significand,
              lit.exponent, lit.radix), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(mp: MapPair, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(mp) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newleft, flag1) = replace(mp.left, map, strategy)
        val (newright, flag2) = replace(mp.right, map, strategy)
        if (flag1 || flag2) {
          (newMapPair(Loc.internal, newleft, newright), true)
        } else {
          (mp, false)
        }
    }
  }
  
  private def _replace(or: OperatorRef, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(or) match {
      case None => (or, false)
      case Some(atom) => (atom, true)
    }
  }
  
  private def _replace(sf: SpecialForm, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    map.get(sf) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtag, flag1) = replace(sf.tag, map, strategy)
        val (newcontent, flag2) = replace(sf.content, map, strategy)
        if (flag1 || flag2) {
          (newSpecialForm(Loc.internal, newtag, newcontent, strategy), true)
        } else {
          (sf, false)
        }
    }
  }
  
  private def _replace(vari: TermVariable, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(vari) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtype, flag1) = replace(vari.theType, map, strategy)
        val (newguard, flag2) = replace(vari.guard, map, strategy)
        if (flag1 || flag2) {
          (newTermVariable(Loc.internal, newtype, vari.name, newguard,
              vari.labels, vari.byName), true)
        } else {
          (vari, false)
        }
    }
  }
  
  private def _replace(vari: MetaVariable, map: Map[BasicAtom, BasicAtom],
      strategy: GuardStrategy) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(vari) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtype, flag1) = replace(vari.theType, map, strategy)
        val (newguard, flag2) = replace(vari.guard, map, strategy)
        if (flag1 || flag2) {
          (newMetaVariable(Loc.internal, newtype, vari.name, newguard,
              vari.labels, vari.byName), true)
        } else {
          (vari, false)
        }
    }
  }
}
