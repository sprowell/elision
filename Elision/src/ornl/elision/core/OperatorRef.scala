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
package ornl.elision.core

import ornl.elision.util.Loc

/**
 * Make and match operator references.
 */
object OperatorRef {
  
  /**
   * Extract the operator from the reference.
   *
   * @param ref   The operator reference.
   * @return  The referenced operator.
   */
  def unapply(ref: OperatorRef) = Some(ref.operator)
}

/**
 * Encapsulate a reference to an operator.
 *
 * == Purpose ==
 * Operators are just atoms, so they can be matched and rewritten.  This is
 * not always desirable; we want the operator to remain fixed.  This class
 * provides a level of indirection.
 *
 * @param loc       Location of the atom's declaration.
 * @param operator  The referenced operator.
 */
class OperatorRef(
    loc: Loc,
    val operator: Operator) extends BasicAtom(loc) {
  val depth = 0
  val deBruijnIndex = 0
  val isTerm = true
  val isConstant = true
  val theType = OPREF
  /** The operator name. */
  val name = operator.name
  
    
  /**
   * Apply the operator to the given argument list.
   * 
   * @param args    The argument list.
   */
  def apply(args: IndexedSeq[BasicAtom]): BasicAtom = {
    operator(args)
  }
  
  /**
   * Apply the operator to the given argument list.
   * 
   * @param args    The argument list.
   */
  def apply(args: BasicAtom*): BasicAtom = {
    operator(args:_*)
  }

  /**
   * Operator references are equal iff the referenced operators are equal.
   */
  override def equals(other: Any) = other match {
    case OperatorRef(oop) if (oop == operator) => true
    case _ => false
  }

  override lazy val hashCode = 31 * operator.hashCode
  lazy val otherHashCode = 8191 * operator.otherHashCode
}
