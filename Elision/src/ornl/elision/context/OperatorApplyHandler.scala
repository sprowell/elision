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
import ornl.elision.core.Operator
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.CaseOperator
import ornl.elision.util.Loc
import ornl.elision.util.ElisionException
import ornl.elision.core.AtomSeq
import ornl.elision.core.Bindings
import ornl.elision.repl.Processor
import ornl.elision.core.Apply
import ornl.elision.core.Literal
import ornl.elision.core.SimpleApply
import ornl.elision.core.OperatorRef
import ornl.elision.core.toESymbol
import ornl.elision.core.OpApply
import ornl.elision.core.ANY
import ornl.elision.core.Applicable
import ornl.elision.core.NONE
import ornl.elision.util.Console
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Match
import ornl.elision.matcher.Many
import ornl.elision.matcher.SequenceMatcher
import ornl.elision.core.ArgumentListException

/**
 * Applying an operator to an argument (or argument list) is a complicated
 * operation.  This is the base class for objects that handle that process.
 */
abstract class OperatorApplyHandler {

  /**
   * Apply an operator to an argument.
   * 
   * @param op      The operator.
   * @param arg     The argument.
   * @param builder The builder to create atoms.
   * @param bypass  If true, bypass the operator's native handler, if any.
   * @return  The constructed atom.
   */
  def apply(op: Operator, arg: BasicAtom, builder: Builder,
      bypass: Boolean = false): BasicAtom 
}
