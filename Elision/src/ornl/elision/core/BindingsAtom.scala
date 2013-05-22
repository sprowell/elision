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
======================================================================
* */
package ornl.elision.core

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import ornl.elision.util.OmitSeq
import ornl.elision.util.other_hashify
import ornl.elision.matcher.SequenceMatcher
import ornl.elision.util.Loc

/**
 * Encapsulate a set of bindings as an atom.
 * 
 * While bindings are a special form, they are also a key ingredient of most
 * special forms, and thus get some specialized handling here.  In particular,
 * they do not extend [[ornl.elision.core.SpecialForm]], though this might
 * change.  This helps avoid a loop in the use hierarchy.
 * 
 * == Purpose ==
 * A bindings atom wraps a set of bindings and allows them to be treated as if
 * they were an atom (matched and rewritten, for instance).  Since this is
 * costly, and since bindings are critical to the operation of the rewriter,
 * this class is typically used "just in time" by an implicit conversion.
 *
 * == Structure and Syntax ==
 * Bindings are a special form, and the general syntax is the tag ''bind''
 * and content equal to a list of map pairs, each of whose left-hand sides
 * must be a symbol.
 * 
 * == Type ==
 * All bindings atoms have the special type BINDING.
 * 
 * == Equality and Matching ==
 * Bindings are equal iff they bind the same symbols to equal values.  They
 * match only if they bind the same symbols, and their respective bindings
 * match.
 * 
 * @param loc           Location of this atom's declaration.
 * @param mybinds       Bindings for this atom.
 */
class BindingsAtom(
    loc: Loc,
    val mybinds: Bindings) extends BasicAtom(loc) with Applicable {
  
  require(mybinds != null, "Bindings are null.")
  
  /**
   * Alternate constructor to wrap a map.
   * 
   * @param loc           Location of this atom's declaration.
   * @param mybinds       Bindings for this atom.
   */
  def this(loc: Loc, mybinds: Map[String, BasicAtom]) = {
    this(loc, Bindings(mybinds))
  }
  
  override val hashCode = mybinds.hashCode
  lazy val otherHashCode = (this.toString).foldLeft(BigInt(0))(other_hashify)+1

  /** The type of a bindings atom is the special bindings type. */
  val theType = ANY
  lazy val isConstant = mybinds.values.forall(_.isConstant)
  lazy val isTerm = mybinds.values.forall(_.isTerm)
  lazy val deBruijnIndex = mybinds.values.foldLeft(0)(_ max _.deBruijnIndex)
  lazy val depth = mybinds.values.foldLeft(0)(_ max _.depth) + 1
    
  override def equals(other: Any) = other match {
    case oba: BindingsAtom =>
      feq(oba, this, (oba.mybinds == mybinds))
      
    case _ =>
      false
  }
}

/**
 * Simplified construction of bindings atoms.
 */
object BindingsAtom {
  
  /** The special form tag. */
  val tag = Literal('binds)
  
  /**
   * Extract the parts of this atom.
   * 
   * @param ba    The bindings atom.
   * @return  The bindings.
   */
  def unapply(ba: BindingsAtom) = Some((ba.mybinds))
}
