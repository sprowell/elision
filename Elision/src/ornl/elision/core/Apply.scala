/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ======================================================================*/
package ornl.elision.core

import scala.compat.Platform
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import ornl.elision.util.Loc

/**
 * Provide construction and extraction for an `Apply`.  This is the correct
 * place to come to make an application object.
 */
object Apply {
  
  /**
   * Extract the components of an apply and return them.
   * 
   * @param apply The apply.
   * @return  A pair consisting of the operator and argument, in order.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
}

/**
 * The common root for all application atoms.  This class represents the
 * "applicative dot."
 * 
 * == Purpose ==
 * An ''apply'' takes two atoms and applies the first (as an operator,
 * rewriter, etc.) to the second (as the argument).
 * 
 * In general this just forms a pair, but certain left-hand sides will
 * undergo specialized processing by the system.
 * 
 * == Use ==
 * Use this class via the companion object, so that the correct result is
 * returned.  The result may be any kind of atom.
 * 
 * @param loc   The location of the atom's declaration.
 * @param op		The left-hand element of the apply (operator).
 * @param arg		The right-hand element of the apply (argument).
 */
abstract class Apply(
    loc: Loc,
    val op: BasicAtom,
    val arg: BasicAtom) extends BasicAtom(loc) {
  
  lazy val isConstant = op.isConstant && arg.isConstant
  lazy val isTerm = op.isTerm && arg.isTerm
  lazy val depth = (op.depth max arg.depth) + 1
  lazy val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  lazy val otherHashCode = op.otherHashCode + 8191*arg.otherHashCode
  
  override def equals(other: Any) = (other match {
      case oapp: Apply =>
        feq(oapp, this, (op == oapp.op) && (arg == oapp.arg))
        
      case _ =>
        false
    })
}

/**
 * An ''operator apply''.  This is the common case of applying a known operator
 * to some argument list.
 * 
 * This has some special syntax (operator name juxtaposed with argument list
 * in parentheses) and provides special handling for the type (the type is
 * rewritten using the bindings resulting from matching the arguments against
 * the parameters).
 * 
 * Based on properties and any native handler, this may never be constructed
 * for an operator application.  '''Do not use this directly.'''  Instead,
 * use the methods in the [[ornl.elision.core.Apply]] companion object.
 * 
 * @param loc     Location of the atom's declaration.
 * @param op			The operator.
 * @param arg			The argument list.
 * @param pabinds	The bindings from parameter name to argument.  Note that
 * 								if the operator is associative the parameters may be
 * 								synthetic!
 */
case class OpApply protected[core] (
    loc: Loc,
    override val op: OperatorRef,
    override val arg: AtomSeq,
    val pabinds: Bindings) extends Apply(loc, op, arg) {
  
  /**
   * Compute the type from the type specified by the operator, and the bindings
   * provided during parameter matching.  This allows rewriting otherwise
   * abstract type information to get a proper type.
   */
  lazy val theType = op.operator.typ.rewrite(pabinds)._1
  
  override def rewrite(binds: Bindings) = {
    // If we have no bindings, don't rewrite the operator.
    if (binds == null) {
      (this, false)
    } else {
      // We have bindings. Rewrite the operator.
      // See if we have already rewritten this operator with these
      // bindings.
      (binds.rewrites get this) match {
        // We have already done this rewrite.
        case Some(rewrite) =>
          rewrite
        
        // We don't have a cached rewrite.
        case None =>
          // Rewrite the argument, but not the operator.  In reality, operators
          // should protect their arguments using De Bruijn indices, but that's
          // not implemented just yet.
          val pair = arg.rewrite(binds)
          if (pair._2) {
            val newApply = Apply(op, pair._1)
            binds.rewrites(this) = (newApply, true) 
            (newApply, true) 
          } else {
            binds.rewrites(this) = (this, false) 
            (this, false)
          }
      }
    }  
  }
}

/**
 * A ''simple apply''.  This is the class used if an apply "survives"
 * processing, such as when the right-hand side is not a term.
 * 
 * '''Do not use this directly.''' Instead, use the methods in the
 * [[ornl.elision.core.Apply]] companion object to create an apply using
 * the correct processing.
 * 
 * @param loc   The location of the atom's declaration.
 * @param op		The operator.
 * @param arg		The argument.
 */
case class SimpleApply protected[core] (
    loc: Loc,
    override val op: BasicAtom,
    override val arg: BasicAtom) extends Apply(loc, op, arg) {
  
  /**
   * We take the type from the operator.  This may be an incomplete type, but
   * we cannot rewrite it yet because we don't know the full bindings.  This
   * might cause trouble with matching.
   */
  val theType = op.theType
}
