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

/**
 * Apply a binding to an atom, replacing instances of bound variables with
 * their corresponding binding, and evaluating the result.  This class also
 * provides a more general - and dangerous - method to replace one atom with
 * another.
 * 
 * To use this, make an instance and provide a builder.  Then invoke either
 * the `apply` method to rewrite an atom, or the `replace` method to replace
 * atoms.  If you care about the specific result, you might try one of the
 * `rewrite` methods.  These make guarantees about the return value's type.
 * 
 * @param builder A builder to construct atoms as needed.
 */
class Binder(builder: Builder) {

  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def apply(atom: BasicAtom, binds: Bindings): (BasicAtom, Boolean) = {
    atom match {
      case lit: IntegerLiteral => rewrite(lit, binds)
      
      case lit: SymbolLiteral => rewrite(lit, binds)
      
      case lit: StringLiteral => rewrite(lit, binds)
      
      case lit: BitStringLiteral => rewrite(lit, binds)
      
      case lit: FloatLiteral => rewrite(lit, binds)
      
      case ap: AlgProp => rewrite(ap, binds)

      case app: Apply => rewrite(app, binds)
        
      case as: AtomSeq => rewrite(as, binds)

      case ba: BindingsAtom => rewrite(ba, binds)

      case lam: Lambda => rewrite(lam, binds)
        
      case map: MapPair => rewrite(map, binds)
        
      case sf: SpecialForm => rewrite(sf, binds)
      
      case or: OperatorRef => rewrite(or, binds)
        
      case vari: TermVariable => rewrite(vari, binds)
        
      case vari: MetaVariable => rewrite(vari, binds)
        
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
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def replace(atom: BasicAtom,
      map: Map[BasicAtom, BasicAtom]): (BasicAtom, Boolean) = {
    atom match {
      case lit: IntegerLiteral => _replace(lit, map)
      
      case lit: SymbolLiteral => _replace(lit, map)
      
      case lit: StringLiteral => _replace(lit, map)
      
      case lit: BitStringLiteral => _replace(lit, map)
      
      case lit: FloatLiteral => _replace(lit, map)
      
      case ap: AlgProp => _replace(ap, map)

      case app: Apply => _replace(app, map)
        
      case as: AtomSeq => _replace(as, map)

      case ba: BindingsAtom => _replace(ba, map)

      case lam: Lambda => _replace(lam, map)
        
      case mp: MapPair => _replace(mp, map)
        
      case sf: SpecialForm => _replace(sf, map)
      
      case or: OperatorRef => _replace(or, map)
        
      case vari: TermVariable => _replace(vari, map)
        
      case vari: MetaVariable => _replace(vari, map)
        
      case _ =>
        throw new ElisionException(Loc.internal,
            "Do not know how to rewrite the atom "+atom.toParseString+".")
    }
  }
  
  //======================================================================
  // Rewrite methods.
  //======================================================================
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(nrt: NamedRootType, binds: Bindings): (NamedRootType, Boolean) = {
    (nrt, false)
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(ap: AlgProp, binds: Bindings): (AlgProp, Boolean) = {
    def _rewrite(opt: Option[BasicAtom]) = {
      opt match {
        case None => 
          (None, false)
          
        case Some(atom) => {
          val newatom = apply(atom, binds)
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
      (builder.newAlgProp(Loc.internal, assoc._1, commu._1, idemp._1, absor._1,
          ident._1), true)
    } else {
      (ap, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(app: Apply, binds: Bindings): (BasicAtom, Boolean) = {
    val (nop, nof) = apply(app.op, binds)
    val (narg, naf) = apply(app.arg, binds)
    if (nof || naf) {
      (builder.newApply(Loc.internal, nop, narg), true)
    } else { 
      (app, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(as: AtomSeq, binds: Bindings): (AtomSeq, Boolean) = {
    // Rewrite the properties.
    val (newprop, pchanged) = rewrite(as.props, binds)
    
    // We must rewrite every child atom, and collect them into a new sequence.
    var schanged = false
    val newseq = as.atoms map {
      atom =>
        val (newatom, changed) = apply(atom, binds)
        schanged |= changed
        newatom
    }
    
    // If anything changed, make a new sequence.
    if (pchanged || schanged) {
      (builder.newAtomSeq(Loc.internal, newprop, newseq), true)
    } else {
      (as, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(ba: BindingsAtom, binds: Bindings): (BindingsAtom, Boolean) = {
    var changed = false
    var newmap = Bindings()
    for ((key, value) <- ba.mybinds) { 
      val (newvalue, valuechanged) = apply(value, binds)
      changed |= valuechanged
      newmap += (key -> newvalue)
    } // Rewrite all bindings.
    
    if (changed) {
      (builder.newBindingsAtom(Loc.internal, newmap), true) 
    } else {
      (ba, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lam: Lambda, binds: Bindings): (Lambda, Boolean) = {
    // We test for a special case here.  If the bindings specify that we
    // should rewrite our own bound De Bruijn index, we explicitly ignore
    // it.
    val newbinds = binds - lam.lvar.name
    apply(lam.body, newbinds) match {
      case (newatom, changed) if changed => 
        (builder.newLambda(Loc.internal, lam.lvar, newatom), true)
        
      case _ => 
        (lam, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: IntegerLiteral,
      binds: Bindings): (IntegerLiteral, Boolean) = {
    apply(lit.theType, binds) match {
      case (newtype, true) =>
        (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: SymbolLiteral, binds: Bindings): (SymbolLiteral, Boolean) = {
    apply(lit.theType, binds) match {
      case (newtype, true) =>
        (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: StringLiteral, binds: Bindings): (StringLiteral, Boolean) = {
    apply(lit.theType, binds) match {
      case (newtype, true) =>
        (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: BitStringLiteral,
      binds: Bindings): (BitStringLiteral, Boolean) = {
    apply(lit.theType, binds) match {
      case (newtype, true) =>
        (builder.newLiteral(Loc.internal, newtype, lit.bits, lit.len), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(lit: FloatLiteral, binds: Bindings): (FloatLiteral, Boolean) = {
    apply(lit.theType, binds) match {
      case (newtype, true) =>
        (builder.newLiteral(Loc.internal, newtype, lit.significand,
            lit.exponent, lit.radix), true)
        
      case _ =>
        (lit, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(map: MapPair, binds: Bindings): (MapPair, Boolean) = {
    val newleft = apply(map.left, binds)
    val newright = apply(map.right, binds)
    if (newleft._2 || newright._2) {
      (builder.newMapPair(Loc.internal, newleft._1, newright._2), true)
    } else {
      (map, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(or: OperatorRef, binds: Bindings): (OperatorRef, Boolean) = {
    // Operator references cannot be rewritten... which is actually why they
    // exist!
    (or, false)
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(sf: SpecialForm, binds: Bindings): (SpecialForm, Boolean) = {
    val newtag = apply(sf.tag, binds)
    val newcontent = apply(sf.content, binds)
    if (newtag._2 || newcontent._2) {
      (builder.newSpecialForm(Loc.internal, newtag._1, newcontent._1), true)
    } else {
      (sf, false)
    }
  }
  
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(vari: TermVariable, binds: Bindings): (BasicAtom, Boolean) = {
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
          apply(vari.theType, binds) match {
            case (newtype, true) =>
              (builder.newTermVariable(Loc.internal, newtype, vari.name,
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
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(vari: MetaVariable, binds: Bindings): (BasicAtom, Boolean) = {
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
          apply(vari.theType, binds) match {
            case (newtype, true) =>
              (builder.newMetaVariable(Loc.internal, newtype, vari.name,
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
  
  private def _replace(ap: AlgProp, map: Map[BasicAtom, BasicAtom]) = {
    def _replace(opt: Option[BasicAtom]) = opt match {
      case None =>
        (None, false)
        
      case Some(atom) =>
        val (newatom, flag) = replace(atom, map)
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
          (builder.newAlgProp(Loc.internal, newA, newC, newI, newB, newD), true)
        } else {
          (ap, false)
        }
    }
  }
  
  private def _replace(app: Apply, map: Map[BasicAtom, BasicAtom]) = {
    map.get(app) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newop, flag1) = replace(app.op, map)
        val (newarg, flag2) = replace(app.arg, map)
        if (flag1 || flag2) {
          (builder.newApply(Loc.internal, newop, newarg), true)
        } else {
          (app, false)
        }
    }
  }
  
  private def _replace(as: AtomSeq, map: Map[BasicAtom, BasicAtom]) = {
    map.get(as) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        var flag1 = false
        val newatoms = as.atoms map {
          atom =>
            val (newatom, changed) = replace(atom, map)
            flag1 |= changed
            newatom
        }
        // The algebraic properties must rewrite to a valid algebraic
        // properties atom, or we must discard it since we cannot build a
        // legal algebraic properties atom otherwise.
        val (newprops, flag2) = replace(as.props, map) match {
          case (ap: AlgProp, flag: Boolean) => (ap, flag)
          case _ => (as.props, false)
        }
        if (flag1 || flag2) {
          (builder.newAtomSeq(Loc.internal, newprops, newatoms), true)
        } else {
          (as, false)
        }
    }
  }
  
  private def _replace(ba: BindingsAtom, map: Map[BasicAtom, BasicAtom]) = {
    map.get(ba) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        var flag = false
        val newbinds = ba.mybinds map {
          bind =>
            val (newbind, changed) = replace(bind._2, map)
            flag |= changed
            (bind._1, newbind)
        }
        if (flag) {
          (builder.newBindingsAtom(Loc.internal, newbinds), true)
        } else {
          (ba, false)
        }
    }
  }
  
  private def _replace(lam: Lambda, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lam) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newvar, flag) = replace(lam.lvar, map)
        val (newlvar, flag1) =
          (if (newvar.isInstanceOf[Variable])
            (newvar.asInstanceOf[Variable], flag) else (lam.lvar, false))
        val (newbody, flag2) = replace(lam.body, map)
        if (flag1 || flag2) {
          (builder.newLambda(Loc.internal, newlvar, newbody), true)
        } else {
          (lam, false)
        }
    }
  }
  
  private def _replace(lit: IntegerLiteral, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map)
        if (flag) {
          (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: SymbolLiteral, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map)
        if (flag) {
          (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: StringLiteral, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map)
        if (flag) {
          (builder.newLiteral(Loc.internal, newtype, lit.value), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: BitStringLiteral, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map)
        if (flag) {
          (builder.newLiteral(Loc.internal, newtype, lit.bits, lit.len), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(lit: FloatLiteral, map: Map[BasicAtom, BasicAtom]) = {
    map.get(lit) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = replace(lit.theType, map)
        if (flag) {
          (builder.newLiteral(Loc.internal, newtype, lit.significand,
              lit.exponent, lit.radix), true)
        } else {
          (lit, false)
        }
    }
  }
  
  private def _replace(mp: MapPair, map: Map[BasicAtom, BasicAtom]) = {
    map.get(mp) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newleft, flag1) = replace(mp.left, map)
        val (newright, flag2) = replace(mp.right, map)
        if (flag1 || flag2) {
          (builder.newMapPair(Loc.internal, newleft, newright), true)
        } else {
          (mp, false)
        }
    }
  }
  
  private def _replace(or: OperatorRef, map: Map[BasicAtom, BasicAtom]) = {
    map.get(or) match {
      case None => (or, false)
      case Some(atom) => (atom, true)
    }
  }
  
  private def _replace(sf: SpecialForm, map: Map[BasicAtom, BasicAtom]) = {
    map.get(sf) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtag, flag1) = replace(sf.tag, map)
        val (newcontent, flag2) = replace(sf.content, map)
        if (flag1 || flag2) {
          (builder.newSpecialForm(Loc.internal, newtag, newcontent), true)
        } else {
          (sf, false)
        }
    }
  }
  
  private def _replace(vari: TermVariable, map: Map[BasicAtom, BasicAtom]) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(vari) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtype, flag1) = replace(vari.theType, map)
        val (newguard, flag2) = replace(vari.guard, map)
        if (flag1 || flag2) {
          (builder.newTermVariable(Loc.internal, newtype, vari.name, newguard,
              vari.labels, vari.byName), true)
        } else {
          (vari, false)
        }
    }
  }
  
  private def _replace(vari: MetaVariable, map: Map[BasicAtom, BasicAtom]) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(vari) match {
      case Some(atom) =>
        (atom, true)
        
      case None =>
        val (newtype, flag1) = replace(vari.theType, map)
        val (newguard, flag2) = replace(vari.guard, map)
        if (flag1 || flag2) {
          (builder.newMetaVariable(Loc.internal, newtype, vari.name, newguard,
              vari.labels, vari.byName), true)
        } else {
          (vari, false)
        }
    }
  }
}
