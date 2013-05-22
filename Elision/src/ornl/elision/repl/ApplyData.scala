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
package ornl.elision.repl

import ornl.elision.core.SymbolicOperator.AbstractApplyData
import ornl.elision.util.Console
import ornl.elision.context.Context
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.AtomSeq
import ornl.elision.core.Bindings
import ornl.elision.core.Literal

/**
 * Constants available to native operators during execution.
 */
object ApplyData {
  
  /** A special literal that we never show, or save as a binding. */
  val _no_show = Literal(Symbol(" NO SHOW "))
}

/**
 * Provide a concrete interface to native handlers that allows them access to
 * the top-level processor.  These instances can only be created from a
 * processor, and this is done by providing the context (and thus other
 * subordinate classes) with a closure that creates these instances.
 * 
 * @author Stacy Prowell (sprowell@gmail.com)
 * 
 * @param op    The operator.
 * @param args  The arugments.
 * @param binds Binding of parameters to arguments.
 */
abstract class ApplyData(
    val op: SymbolicOperator,
    val args: AtomSeq,
    val binds: Bindings) extends AbstractApplyData {

  /**
   * The top-level processor that created this instance.
   */
  val processor: Processor
  
  /**
   * The context providing named semantics.
   */
  val context: Context
  
  /**
   * A console for output.
   */
  val console: Console

  /**
   * Just preserve the apply as it is.  The loc of the operator is used as the
   * loc of the generated apply.
   */
  def as_is =
    context.builder.newApply(op.loc, op, args, context.guardstrategy, true)
}