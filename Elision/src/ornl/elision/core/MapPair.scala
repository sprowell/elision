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

import ornl.elision.matcher.SequenceMatcher
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Match
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Many
import ornl.elision.util.Loc

object MapPair {
  
  /**
   * Extract the parts of a map pair.
   * 
   * @param pair  The map pair.
   * @return  The left and right parts.
   */
  def unapply(pair: MapPair) = Some((pair.left, pair.right))
}

/**
 * Provide an ordered pair that also serves as a very simple kind of rewrite
 * rule.
 * 
 * == Purpose ==
 * The map pair can use used to construct a pair, but when applied on the
 * left hand side of the applicative dot, it tries to match the right-hand
 * side against its left-hand side.  If the match succeeds, the result is
 * the rewrite of the map pair's right-hand side and a true flag.
 * 
 * At present there is no way to specify general guards, so the first match
 * of the left hand side is used immediately and unconditionally.  (Variable
 * guards are, of course, honored.)
 * 
 * @param loc     Location of the declaration of the atom.
 * @param left    The left atom.
 * @param right   The right atom.
 */
class MapPair(
    loc: Loc,
    val left: BasicAtom,
    val right: BasicAtom) extends BasicAtom(loc) with YieldsPair {
  
	/** A map pair is actually a strategy. */
  val theType = STRATEGY
  lazy val isConstant = left.isConstant && right.isConstant
  lazy val depth = (left.depth max right.depth) + 1
  lazy val isTerm = left.isTerm && right.isTerm
  lazy val deBruijnIndex = left.deBruijnIndex max right.deBruijnIndex

  override lazy val hashCode = left.hashCode * 31 + right.hashCode
  lazy val otherHashCode = left.otherHashCode + 8191*right.otherHashCode
  
  override def equals(other: Any) = other match {
    case omp: MapPair =>
      feq(omp, this, omp.left == this.left && omp.right == this.right)
      
    case _ =>
      false
  }
}
