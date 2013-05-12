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

import ornl.elision.context.Context
import ornl.elision.util.OmitSeq
import ornl.elision.util.Debugger
import ornl.elision.util.Loc
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Match
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Many

/**
 * Easier construction and pattern matching of rewrite rules.
 */
object RewriteRule {
  
  /** Tag for this special form. */
  val tag = new SymbolLiteral(Loc.internal, 'rule)

  /**
   * Break a rewrite rule into its parts.  The synthetic flag is not returned.
   * 
   * @param rule	The rewrite rule.
   * @return	The pattern, rewrite, guards, rulesets, and whether the rule is
   *          synthetic.
   */
  def unapply(rule: RewriteRule) = Some((rule.pattern, rule.rewrite,
      rule.guards, rule.rulesets, rule.name, rule.description, rule.detail,
      rule.synthetic))
}

/**
 * Encapsulate a rewrite rule.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param loc         Location of the atom's declaration.
 * @param content     The content of the special form, lazily evaluated.
 * @param pattern			The pattern to match.
 * @param rewrite			The rewrite to apply on match.
 * @param guards			Guards that must be true to accept a match.
 * @param rulesets		The rulesets that contain this rule.
 * @param name        Optional rule name.
 * @param description Optional rule description.
 * @param detail      Optional detailed rule description.
 * @param synthetic		If true, this is a synthetic rule.
 */
class RewriteRule protected[elision] (
    loc: Loc,
    content: => BasicAtom,
    val pattern: BasicAtom,
    val rewrite: BasicAtom,
    val guards: Seq[BasicAtom],
    val rulesets: Set[String],
    val name: Option[String] = None,
    val description: String = "",
    val detail: String = "",
    val synthetic: Boolean = false)
    extends SpecialForm(loc, RewriteRule.tag, content) with Strategy {
  
  // Rewrite rules all have type strategy.
  override val theType = STRATEGY
}
