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
  val tag = Literal('rule)
  
  /**
   * Create a rewrite rule.  The rule is not synthetic.
   * 
   * @param loc         Location of the rewrite rule definition.
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
   */
  def apply(loc: Loc, pattern: BasicAtom, rewrite: BasicAtom,
      guards: Seq[BasicAtom], rulesets: Set[String]) = {
    val sfh = _makeSpecialFormHolder(loc, pattern, rewrite, guards, rulesets,
        None, "", "")
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, None)
  }
  
  /**
   * Create a rewrite rule.
   * 
   * @param loc         Location of the rewrite rule definition.
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
	 * @param synthetic		If true, this is a synthetic rule.
   */
  def apply(loc: Loc, pattern: BasicAtom, rewrite: BasicAtom,
      guards: Seq[BasicAtom], rulesets: Set[String], synthetic: Boolean) = {
    val sfh = _makeSpecialFormHolder(loc, pattern, rewrite, guards, rulesets,
        None, "", "")
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, None, "", "",
        synthetic)
  }
  
  /**
   * Create a rewrite rule.
   * 
   * @param loc         Location of the rule definition.
   * @param pattern     The pattern to match.
   * @param rewrite     The rewrite to apply on match.
   * @param guards      Guards that must be true to accept a match.
   * @param rulesets    The rulesets that contain this rule.
   * @param name        Optional rule name.
   * @param description Optional rule description.
   * @param detail      Optional detailed rule description.
   * @param synthetic   If true, this is a synthetic rule.
   */
  def apply(loc: Loc, pattern: BasicAtom, rewrite: BasicAtom,
      guards: Seq[BasicAtom], rulesets: Set[String], name: Option[String],
      description: String, detail: String, synthetic: Boolean) = {
    val sfh = _makeSpecialFormHolder(loc, pattern, rewrite, guards, rulesets,
        name, description, detail)
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, name, description,
        detail, synthetic)
  }
  
  private def _makeSpecialFormHolder(loc: Loc, pattern: BasicAtom,
      rewrite: BasicAtom, guards: Seq[BasicAtom], rulesets: Set[String],
      name: Option[String], description: String, detail: String) = {
    // Make the map pair.
    val mappair = MapPair(pattern, rewrite)
    // Make the binding.
    var binds:Bindings = Bindings() + (""->AtomSeq(NoProps, mappair))
    if (guards.length > 0) {
      binds += ("if" -> AtomSeq(NoProps, guards.toIndexedSeq))
    }
    if (rulesets.size > 0) {
      binds += ("rulesets" ->
          AtomSeq(NoProps, rulesets.map {
            str => Literal(Symbol(str))
          }.toIndexedSeq))
    }
    name match {
      case None =>
      case Some(value) => binds += ("name" -> Literal(Symbol(value)))
    }
    if (description != "") {
      binds += ("description" -> Literal(description))
    }
    if (detail != "") {
      binds += ("detail" -> Literal(detail))
    }
    new SpecialFormHolder(loc, tag, BindingsAtom(binds))
  }

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
      
  def apply(sfh: SpecialFormHolder): RewriteRule = {
    // A rewrite rule must be given with a binding.
    val bh = sfh.requireBindings
    // Check the content.
    bh.check(Map(""->true, "if"->false, "ruleset"->false, "rulesets"->false,
        "name"->false, "description"->false, "detail"->false))
    // Get the map pair.
    val mappair = bh.fetchAs[AtomSeq]("")
    if (mappair.length < 1)
      throw new SpecialFormException(sfh.loc,
          "Rewrite rule does not contain a map pair.")
    if (mappair.length > 1)
      throw new SpecialFormException(sfh.loc,
          "Too many items at the top level of a rewrite rule; did you forget a marker?")
    val pair = mappair(0) match {
      case mp:MapPair => mp
      case x =>
        throw new SpecialFormException(sfh.loc,
            "Top-level item in rewrite rule is not a map pair: " + x.toParseString)
    }
    // Get the guards.
    val guards = bh.fetchAs[AtomSeq]("if", Some(EmptySeq))
    // Get the rulesets.  We combine two different possibilities here.
    val rseq = bh.fetchAs[AtomSeq]("ruleset", Some(EmptySeq)) ++
      bh.fetchAs[AtomSeq]("rulesets", Some(EmptySeq))
    val rulesets = rseq map {
      rs => rs match {
        case SymbolLiteral(_, name) => name.name
        case _ =>
          throw new SpecialFormException(rs.loc,
              "Ruleset specification is not a symbol: " + rs.toParseString)
      }
    }
    // Get the (optional) name.
    val name = (if (bh.has("name"))
      Some(bh.fetchAs[SymbolLiteral]("name").value.name) else None)
    // Get the description and detail.
    val description = bh.fetchAs[StringLiteral]("description",
        Some(Literal(""))).value
    val detail = bh.fetchAs[StringLiteral]("detail", Some(Literal(""))).value
    // Build the rule.
    new RewriteRule(sfh, pair.left, pair.right, guards.atoms, rulesets.toSet,
        name, description, detail)
  }
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
 * @param sfh					The special form holder.
 * @param pattern			The pattern to match.
 * @param rewrite			The rewrite to apply on match.
 * @param guards			Guards that must be true to accept a match.
 * @param rulesets		The rulesets that contain this rule.
 * @param name        Optional rule name.
 * @param description Optional rule description.
 * @param detail      Optional detailed rule description.
 * @param synthetic		If true, this is a synthetic rule.
 */
class RewriteRule private (
    sfh: SpecialFormHolder,
    val pattern: BasicAtom,
    val rewrite: BasicAtom,
    val guards: Seq[BasicAtom],
    val rulesets: Set[String],
    val name: Option[String] = None,
    val description: String = "",
    val detail: String = "",
    val synthetic: Boolean = false)
    extends SpecialForm(sfh.loc, sfh.tag, sfh.content) with Strategy {
  override val theType = STRATEGY
}
