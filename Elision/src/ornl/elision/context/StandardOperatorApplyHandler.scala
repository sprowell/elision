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

import ornl.elision.core.Operator
import ornl.elision.core.BasicAtom
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.CaseOperator
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.core.AtomSeq
import ornl.elision.core.OpApply
import ornl.elision.core.Bindings
import ornl.elision.core.Literal
import ornl.elision.core.ArgumentListException
import ornl.elision.core.toESymbol
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Many
import ornl.elision.matcher.Match
import ornl.elision.matcher.SequenceMatcher
import ornl.elision.core.SimpleApply
import ornl.elision.core.Strategy
import ornl.elision.core.Applicable
import ornl.elision.util.Console
import ornl.elision.core.OperatorRef
import ornl.elision.core.ANY

/**
 * Applying an operator to an argument (or argument list) is a complicated
 * operation.  This object manages the details of that.
 * 
 * @param context   A context that will be passed to the native handlers.
 */
class StandardOperatorApplyHandler(context: Context)
extends OperatorApplyHandler {
  
  /**
   * Apply an operator to an argument.
   * 
   * @param op      The operator.
   * @param arg     The argument.
   * @param builder The builder to make atoms.
   * @param bypass  If true, bypass the operator's native handler, if any.
   * @return  The constructed atom.
   */
  def apply(op: Operator, arg: BasicAtom, builder: Builder,
      bypass: Boolean = false): BasicAtom = {
    // Operator applications can be handled in two quite different ways.
    // A symbolic operator is processed differently from a case operator.
    // We make that distinction now.
    op match {
      case sop: SymbolicOperator =>
        _opApply(sop, arg, context, bypass)
        
      case cop: CaseOperator =>
        _opApply(cop, arg, context, bypass)
        
      case c =>
        // We come here if we find an unsupported operator class.
        throw new ElisionException(Loc.internal,
            "Internal error.  Unrecognized operator subclass: " + c.getClass)
    }
  }
  
  /**
   * Apply a symbolic operator to an argument.  If a native handler is to
   * be invoked, it is compiled first, if necessary.  This provides for
   * "just in time" compilation or loading (from the cache) of native
   * handlers.
   * 
   * @param op      The operator.
   * @param arg     The argument.
   * @param context The necessary context to build atoms.
   * @param bypass  If true, bypass the operator's native handler, if any.
   * @return  The constructed atom.
   */
  private def _opApply(op: SymbolicOperator, arg: BasicAtom, context: Context,
      bypass: Boolean): BasicAtom = {
    arg match {
      case args: AtomSeq =>
        // Things have to happen in the correct order here.  First increase
        // the argument list by flattening associative applications.  Second
        // we reduce by looking for identities, etc.  Third we check for an
        // empty argument list.

        // Save the properties for fast access.
        val props = op.params.props
        val assoc = props.isA(false)
        val commu = props.isC(false)
        val idemp = props.isI(false)
        val absor = props.absorber.getOrElse(null)
        val ident = props.identity.getOrElse(null)
        
        // Run through the arguments and watch for the absorber, omit
        // identities, and flatten associative lists.
        var newseq = args.atoms
        var index = 0
        // While loops are significantly faster than for comprehensions.
        while (index < newseq.size) {
          val atom = newseq(index)
          if (absor == atom) {
            // Found the absorber.  Nothing else to do.
            return absor
          }
          
          // Omit identities and check for associative lists to flatten.  If
          // we remove an identity, do not increment the index.  If we insert
          // items, we should not increment the index.  If we don't change the
          // item at the current index, then we can advance the index pointer.
          if (ident == atom) {
            newseq = newseq.omit(index)
          } else if (assoc) atom match {
            case OpApply(_, _, opref, opargs, binds) if (opref.operator == this) =>
              // Add the arguments directly to this list.  We can assume the
              // sub-list has already been processed, so no deeper checking
              // is needed.  This flattens associative lists, as required.
              newseq = newseq.omit(index)
              newseq = newseq.insert(index, opargs)
              
            case _ =>
              // Nothing to do except increment the pointer.
              index += 1
          } else {
            // Since nothing at this position changed, increment the pointer.
            index += 1
          }
        } // Run through all arguments.

        // Handle actual operator application.
        def handleApply(binds: Bindings): BasicAtom = {
          // Re-package the arguments with the correct properties.
          val newargs =
            context.builder.newAtomSeq(Loc.internal, op.params.props, newseq)
          // See if we are bypassing the native handler.
          if (!bypass) {
            // Do we have a native handler?
            if (op.handler.isEmpty && op.handlertxt.isDefined) {
              // Compile the native handler and cache it.
              op.handler = compileHandler(op, op.handlertxt, context)
            }
            if (op.handler.isDefined) {
              val ad = context.applyDataBuilder(op, newargs, binds)
              return op.handler.get(ad)
            }
          }
          // No native handler.  In this case the type of the apply must be
          // obtained by rewriting the operator's type using the bindings.
          // The "fully applied type" (the co-domain) must be used, and not
          // the type for a typed symbolic operator (which is a MAP).
          val appliedtype = context.builder.rewrite(op.typ, binds,
              context.guardstrategy)._1
          return new OpApply(Loc.internal, appliedtype,
              context.builder.newOperatorRef(Loc.internal, op), newargs, binds)
        }
        
        // Check the argument length versus the parameter length.
        if (!assoc) {
          // The number of arguments must exactly match the number of
          // parameters.
          if (newseq.length > op.params.length) {
            throw new ArgumentListException(arg.loc,
                "Too many arguments for non-associative operator " +
                toESymbol(op.name) + ".  Expected " + op.params.length +
                " but got " + newseq.length + ".")
          } else if (newseq.length < op.params.length) {
            throw new ArgumentListException(arg.loc,
                "Too few arguments for non-associative operator " +
                toESymbol(op.name) + ".  Expected " + op.params.length +
                " but got " + newseq.length + ".")
          }
        } else {
          // There are special cases to handle here.  First, if the argument
          // list is empty, but there is an identity, return it.  Second, if
          // the argument list is empty, but there is no identity, apply the
          // operator to the empty list.
          if (newseq.length == 0) {
            if (ident == null) {
              return handleApply(Bindings())
            } else {
              return ident
            }
          }
        }

        // If the argument list is associative, we have an identity, and we
        // have a single element, then that element must match the type of
        // the operator, and we return it.  Why is this the rule?  We want
        // to use associative operators to mimic "var args", but don't want
        // them to "collapse" when there is just one argument.  That is, we
        // don't want f(x)->x when we just want a var args f.  But if we give
        // f an identity, it is probably a mathematical operator of some kind,
        // and we probably do want f(x)->x.  So, for now, that's the rule.
        // For greater control, you have to use a case operator.
        if (newseq.length == 1) {
          if (assoc && ident != null) {
            // Get the atom.
            val atom = newseq(0)
            // Match the type of the atom against the type of the parameters.
            val param = op.params(0)
            Matcher(param, atom, context.builder, context.guardstrategy) match {
              case Fail(reason, index) =>
                // The argument is invalid.  Reject!
                throw new ArgumentListException(atom.loc,
                    "Incorrect argument for operator " + toESymbol(op.name) +
                    " at position 0: " + atom.toParseString + ".  " + reason())

              case mat: Match =>
                // The argument matches.
                return atom

              case many: Many => {
                // The argument matches.
                return atom
              }
            }
          }
        }

        // Is the current operator associative?
        if (assoc) {
          // Handle type checking of an associative operator. All
          // formal parameters of an associative operator must have
          // the same type, so type checking of an associative
          // operator will be performed by checking:
          //
          // 1. That all arguments of the operator we are trying to
          //    create have the same type.
          // 2. That the type of 1 of the arguments matches the type
          //    of 1 of the formal parameters of the associative
          //    operator.

          // Check to see if all arguments have the same type.
          val anArg = newseq(0)
          val aParam = op.params.atoms(0)
          while (index < newseq.length) {
            // Does the current argument have the same type as the
            // other arguments?
            if (newseq(index).theType != anArg.theType) {
              // No, bomb out.
              throw new ArgumentListException(anArg.loc,
                  "Incorrect argument for operator " + toESymbol(op.name) +
                  " at position " + index + ": " + newseq(index).toParseString +
                  ".  All arguments must have the same type (" + 
                  newseq(index).theType.toParseString + " != " +
                  anArg.theType.toParseString + ").")
            }
          }

          // All arguments have the same type. Now try to match the
          // parameter type with the argument type. Note that the
          // bindings returned by the match are only used for
          // inferring the value of type variables. Since all
          // arguments/formal parameters have the same type, matching
          // 1 formal parameter with 1 argument gives us all the
          // binding information needed to do type inference.
          Matcher(aParam, anArg, context.builder, context.guardstrategy) match {
            case Fail(reason, index) =>
              throw new ArgumentListException(anArg.loc,
                  "Incorrect argument for operator " + toESymbol(op.name) +
                  " at position " + index + ": " + newseq(index).toParseString +
                  ".  " + reason())
            case Match(binds1) => {
              // The argument matches.
              return handleApply(binds1)
            }
            case Many(iter) => {
              // The argument matches.
              return handleApply(iter.next)
            }
          }
        } else {
          // We've run out of special cases to handle.  Now just try to match
          // the arguments against the parameters.
          val newparams = op.params.atoms
          SequenceMatcher.tryMatch(newparams, newseq, context.builder,
              context.guardstrategy) match {
            case Fail(reason, index) =>
              throw new ArgumentListException(newseq(index).loc,
                  "Incorrect argument for operator " + toESymbol(op.name) +
                  " at position " + index + ": " + newseq(index).toParseString +
                  ".  " + reason())
            case Match(binds1) => {
              // The argument list matches.
              return handleApply(binds1)
            }
            case Many(iter) => {
              // The argument list matches.
              return handleApply(iter.next)
            }
          }
        }

      case _ => {
        return new SimpleApply(Loc.internal, op, arg)
      }
    }
  }

  /**
   * Apply a case operator to an argument.
   * 
   * @param op      The operator.
   * @param arg     The argument.
   * @param context The necessary context to build atoms.
   * @param bypass  If true, bypass the operator's native handler, if any.
   * @return  The constructed atom.
   */
  private def _opApply(op: CaseOperator, arg: BasicAtom, context: Context,
      bypass: Boolean) = {
    // Traverse the list of cases and try to find a case that the arguments
    // match.  Every case should be a rewritable, an applicable, or an atom.
    // If a rewritable, apply it and if it succeeds, choose the result.
    // If an applicable, apply it.  If any other atom, choose that atom.
    var result: Option[BasicAtom] = None
    val done = op.cases.exists {
      _ match {
        case rew: Strategy =>
          val pair = context.applybuilder.test(rew, arg, context.builder,
              context.guardstrategy)
          result = Some(pair._1)
          pair._2
          
        case app: Applicable =>
          result = Some(context.applybuilder(app, arg, context.builder,
              context.guardstrategy, bypass))
          true
          
        case atom =>
          result = Some(atom)
          true
      }
    }
    
    // If nothing worked, then we need to generate an error since the operator
    // was incorrectly applied.
    if (!done)
      throw new ArgumentListException(arg.loc, "Applied the operator " +
        toESymbol(op.name) + " to an incorrect argument list: " +
        arg.toParseString)
    
    // If the result turned out to be ANY, then just construct a simple
    // apply for this operator.
    result.get match {
      case ANY => arg match {
        case as: AtomSeq =>
          new OpApply(Loc.internal, op.typ,
              context.builder.newOperatorRef(Loc.internal, op), as, Bindings())
          
        case _ =>
          new SimpleApply(Loc.internal,
              context.builder.newOperatorRef(Loc.internal, op), arg)
      }
      case other =>
        // We have to do one more thing.  We need to bind $__ to this operator,
        // and $_ to the original argument list, and then rewrite the result.
        val binds = Bindings("_"->arg, "__"->op)
        context.builder.rewrite(other, binds, context.guardstrategy)._1
    }
  }
  
  // Time the compilation of native handlers.
  import ornl.elision.util.Timeable
  private val _timer = new Timeable {
    timing = true
    def reportElapsed() = {}
  }
  
  /**
   * Get the time taken to compile native handlers, in milliseconds.
   * 
   * @return  The time used.
   */
  def getTime() {
    _timer.getCumulativeTimeMillis
  }

  /**
   * Print out the time spent compiling native handlers.
   * 
   * @param console The console to get the message.
   */
  def reportTime(console: Console) {
    console.emit("Time Compiling Native Handlers: ")
    console.emitln(Timeable.asTimeString(_timer.getCumulativeTimeMillis))
  }
  
  // Preserve the last context and associated compiler so we don't have to
  // create a new compiler instance every time (unless the context is changed).
  private var lastContext: Context = _
  private var lastCompiler: NativeCompiler = _

  /**
   * Compile Scala code to a native handler.
   * 
   * @param op            The operator to get this native handler.
   * @param handlertxt    The optional text to compile.
   * @param context       The context needed to build atoms.
   * @return  The optional handler result.
   */
  private def compileHandler(op: SymbolicOperator, code: Option[String],
      context: Context): Option[SymbolicOperator.AbstractApplyData => BasicAtom] = {
    // Fetch the handler text.
    if (code.isDefined) {
      var handlertxt = code.get
      if (handlertxt.length > 0 && handlertxt(0) == '|')
        handlertxt = handlertxt.stripMargin('|')
        
      // Compile the handler, if we were given one.
      if (handlertxt != "") {
        this synchronized {
          if (context != lastContext) {
            lastCompiler = new NativeCompiler(context)
            lastContext = context
          }
          return Some(_timer.time {
            lastCompiler.compile(op.loc, op.name, handlertxt)
          })
        }
      }
    } // Handler has text.
    
    // If we get here then no handler code was provided, or the code was
    // empty.
    return None
  }
}
