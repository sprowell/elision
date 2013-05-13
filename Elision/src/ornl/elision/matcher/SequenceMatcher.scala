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
package ornl.elision.matcher

import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.giveMkParseString
import ornl.elision.core.wrapBindingsAtom
import ornl.elision.util.Debugger
import ornl.elision.util.OmitSeq
import ornl.elision.util.OmitSeq.fromIndexedSeq
import ornl.elision.context.Builder

/**
 * Match two sequences of atoms.
 * 
 * == Purpose ==
 * The goal here is to make matching simple(r).  For complex objects, you can
 * have several items to match, any of which can return a match iterator, etc.
 * It is best to concentrate that complexity here, and create a more general
 * solution (or so the logic goes).
 * 
 * == Use ==
 * To use this, provide two sequences of atoms.  The sequences must be the
 * same length, or the whole match is immediately rejected.  This matcher
 * tries to match the respective atom pairs in order, from the first to the
 * last.  Matches are returned iff the entire sequence matches.
 */
object SequenceMatcher {

  /**
   * Match two sequences of atoms, in order.
   * 
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param builder   The builder needed to create atoms.
   * @param binds			Bindings to honor in any match.  This can be omitted.
   * @return	The result of the match.
   */
  def tryMatch(patterns: OmitSeq[BasicAtom], subjects: OmitSeq[BasicAtom],
      builder: Builder, binds: Bindings = Bindings()): Outcome = {
    Debugger("matching") {
      Debugger("matching", "Sequence Matcher called: ")
      Debugger("matching", "    Patterns: " + patterns.mkParseString("",",",""))
      Debugger("matching", "    Subjects: " + subjects.mkParseString("",",",""))
      Debugger("matching", "    Bindings: " + binds.toParseString)
    }
    if (patterns.length != subjects.length) {
      Fail("Sequences are not the same length.")
    } else {
      _tryMatch(patterns, subjects, builder, binds, 0)
    }
  }
  
  /**
   * Match two sequences of atoms, in order.
   * 
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor in any match.
   * @param position	Index into the list.  This is only used to generate
   * 									a failure index to return to the caller.
   * @return	The result of the match.
   */
  private def _tryMatch(patterns: OmitSeq[BasicAtom],
      subjects: OmitSeq[BasicAtom], builder: Builder,
      binds: Bindings, position: Int): Outcome = {
    // Watch for the basis case.  If the patterns list is empty, we are done
    // and return a successful match.
    if (patterns.isEmpty) return Match(binds)
    
    // Try to match the heads of the list.  This generates one of three
    // possible outcomes, and we figure out what to do based on the particular
    // outcome.
    Matcher(patterns.head, subjects.head, builder, binds, None) match {
      case fail:Fail =>
        // The atoms do not match.  There is nothing further to try; the
        // entire match can be rejected at this point, so return failure.
        Fail("Elements do not match.",
             patterns.head, subjects.head, Some(fail), position)
      case Match(newbinds) =>
        // We found exactly one way in which the pattern could match, given the
        // bindings we received.  This is easy; proceed to the next element.
        _tryMatch(patterns.tail, subjects.tail, builder, (newbinds++binds),
            position+1)
      case Many(miter) =>
        // Matching returned a match iterator.  This is the nontrivial case.
        // Now we have to be careful, and build another match iterator that
        // performs the rest of the matching work.  We do that now.
        // NOTE  It may be more efficient to replace this recursion.
        val (pt, st) = (patterns.tail, subjects.tail)
          Many(MatchIterator(
              (newbinds: Bindings) =>
                _tryMatch(pt, st, builder, (newbinds++binds), position+1),
                miter))
    }
  }
}