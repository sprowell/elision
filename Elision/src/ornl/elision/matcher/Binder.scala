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
package ornl.elision.matcher

import ornl.elision.core.Variable
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.context.Builder

/**
 * Generate bindings by attempting to bind a variable to an atom.  The
 * variable's guards are checked to assure that the variable may be bound
 * to the given atom.
 */
object Binder {

  /**
   * Attempt to bind the given variable to the provided atom.  If this can be
   * done, a match is returned with the new binding.  This checks properties
   * and guards for the variable, but does _not_ check the variable type.
   * That is, this is invoked when a binding or a variable to an atom is
   * proposed.
   * 
   * @param binds     The initial bindings.
   * @param variable  The variable to bind.
   * @param atom      The atom to which to bind the variable.
   * @param builder   A builder to construct atoms.
   * @return  The outcome of the binding attempt, which may be either a match
   *          or a failure.
   */
  def bind(binds: Bindings, variable: Variable, atom: BasicAtom,
      builder: Builder): Outcome = {
    // If this is a by-name variable, reject immediately if the subject is not
    // a variable of the same name.
    if (variable.byName) {
      atom match {
        case Variable(_, nm, _, _, _) if nm == variable.name =>
          // Continue processing, so that a guard can be checked.
          
        case _ =>
          return Fail("By-name variable does not match.")
      }
    }
    
    // Check any guard.
    if (variable.guard.isTrue) {
      return Match(binds + (variable.name -> atom))
    } else {
      // Compute the bindings and check the guard.
      val newbinds = binds + (variable.name -> atom)
      val newterm = builder.rewrite(variable.guard, newbinds)._1
      if (newterm.isTrue) {
        return Match(newbinds)
      } else {
        return Fail("Variable guard failed.  Is now: " +newterm.toParseString)
      }
    }
  }
}
