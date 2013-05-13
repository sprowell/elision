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

import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.RewriteRule
import ornl.elision.util.Debugger
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Many
import ornl.elision.matcher.Match
import ornl.elision.matcher.Fail
import ornl.elision.core.wrapBindingsAtom

/**
 * Apply a rule to another atom.
 */
object RuleApplyHandler {

  /**
   * Apply the given rule to the provided atom.  A set of initial bindings
   * to honor is included, along with a hint to provide during matching. 
   * Finally, a builder is provided to allow atoms to be created.
   * 
   * @param rule      The rewrite rule.
   * @param atom      The atom.
   * @param binds     The bindings.
   * @param hint      The optional hint.
   * @param builder   The builder.
   * @param strategy  The strategy to use to rewrite guards.
   * @return  The result of applying the rule to the atom, packaged as a pair
   *          whose first element is the final atom, and whose second element
   *          is true iff the rule was successfully applied.
   */
  def apply(rule: RewriteRule, atom: BasicAtom, binds: Bindings,
      hint: Option[Any], builder: Builder) = {
    // Try to apply the rewrite rule.  Whatever we get back is the result.
    _tryRewrite(rule, atom, binds, hint, builder)
  }
  
  /**
   * Attempt to apply a rule to a given atom.
   * 
   * To successfully apply a rule, the subject must match the pattern and
   * given the provided bindings (if any).  The complete set of bindings is
   * then used to rewrite every guard, and these are checked to see if they
   * are the literal true.  At present no further rewriting of guards is
   * performed.
   * 
   * If all guards are true, then the bindings are applied to the rewrite and
   * the result is returned.
   * 
   * @param subject   The subject to test.
   * @param binds     Bindings to honor.
   * @param hint      An optional hint to pass along during matching.
   * @return  A pair consisting of an atom and a boolean.  The boolean is
   *          true if the rewrite yielded a new atom, and is false otherwise.
   */
  private def _tryRewrite(
      rule: RewriteRule,
      subject: BasicAtom,
      binds: Bindings,
      hint: Option[Any],
      builder: Builder): (BasicAtom, Boolean) = {
    // Local function to check the guards.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- rule.guards) {
        val (newguard, _) = builder.rewrite(guard, candidate)
        val (newguard1, _) = builder.guardStrategy(newguard)
        if (!newguard1.isTrue) return false
      }
      true
    }
    
    // Local function to perform the rewrite if the rule fires.  We return
    // true in the pair no matter what, since the rule fired.
    def doRuleRewrite(candidate: Bindings) = {
      Debugger("rewrite", "Applied rule: " + rule.toParseString +
          " to: " + candidate.toParseString + "")
      (builder.rewrite(rule.rewrite, candidate)._1, true)
    }
    
    // First we try to match the given atom against the pattern.
    Debugger("rulematch", "Trying rule: " + rule.toParseString)
    Matcher(rule.pattern, subject, builder, binds, hint) match {
      case fail:Fail =>
        Debugger("rulematch", "Rule does not match subject: " + fail)
        return (subject, false)

      case Match(newbinds) =>
        // We got a match.  Check the guards.
        Debugger("rulematch", "Potential match (1): " + newbinds.toParseString)
        if (checkGuards(newbinds)) {
          Debugger("rulematch", "Rule matched with: "+newbinds)
          return doRuleRewrite(newbinds)
        } else {
          Debugger("rulematch", "Guard failed.")
          return (subject, false)
        }
        
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          Debugger("rulematch", "Potential match (*): " + newbinds.toParseString)
          if (checkGuards(newbinds)) {
            Debugger("rulematch", "Rule matched with "+newbinds.toParseString)
            return doRuleRewrite(newbinds)
          }
          Debugger("rulematch", "Guard failed.")
        }
        return (subject, false)
    }
  }
}