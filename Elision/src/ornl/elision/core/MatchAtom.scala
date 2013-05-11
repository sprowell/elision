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

import ornl.elision.util.Loc
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Match
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Many

/**
 * Provide construction and matching.
 */
object MatchAtom {
  
  /** The special form tag. */
  val tag = Literal('match)
  
  /**
   * Extract the pattern atom and any guards.
   * 
   * @param ma  The match atom.
   * @return  The pattern and then guards.
   */
  def unapply(ma: MatchAtom) = Some((ma.pattern, ma.guards))
}

/**
 * Encapsulate a simple atom to perform basic matching.
 * 
 * == Purpose ==
 * This atom holds a single pattern ''P''.  When applied to another atom ''A'',
 * it tries to match ''A'' against pattern ''P''.  If the match fails, then
 * `NONE` is returned.  The first match that satisfies the guards (if any) is
 * returned.
 * 
 * == Rules ==
 * This is almost a rewrite rule.  Consider the following rule.
 * 
 * `{ rule ``$``x:INTEGER -> 0 #if equal(``$``$``x,5) }`
 * 
 * We can ''almost'' write this as follows, provided we are very careful with
 * metavariables.
 * 
 * `\``$``$``x.(({match ``$``$``x:INTEGER #if equal(``$``$``x,5)}.``$``$``x).0)`
 * 
 * The reason the above two are not the same is that in the second case
 * we might get `NONE` as the result from the match and this will not
 * get "digested" by the applicative dot.
 * 
 * @param loc       Location of the atom's declaration.
 * @param content   Special form content, lazily constructed.
 * @param pattern   The pattern to match.
 * @param guards    Any guards.
 * @param typ       Type of this match atom.
 */
class MatchAtom(
    loc: Loc,
    content: => BasicAtom,
    val pattern: BasicAtom,
    val guards: AtomSeq,
    typ: BasicAtom)
    extends SpecialForm(loc, MatchAtom.tag, content) with Applicable {
  
  /** The type of this atom. */
  override val theType = typ
  
  override def equals(other: Any) = other match {
    case oma: MatchAtom =>
      feq(oma, this, oma.pattern == pattern && oma.guards == guards)
      
    case _ =>
      false
  }
}
