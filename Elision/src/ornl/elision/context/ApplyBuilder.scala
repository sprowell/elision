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
import ornl.elision.core.AlgProp
import ornl.elision.core.AtomSeq
import ornl.elision.core.SimpleApply
import ornl.elision.core.BindingsAtom
import ornl.elision.core.Lambda
import ornl.elision.matcher.Matcher
import ornl.elision.core.Bindings
import ornl.elision.matcher.Fail
import ornl.elision.util.Loc
import ornl.elision.util.ElisionException
import ornl.elision.matcher.Many
import ornl.elision.matcher.Match
import ornl.elision.core.MapPair
import ornl.elision.core.MatchAtom
import ornl.elision.core.NONE
import ornl.elision.core.Operator
import ornl.elision.core.RewriteRule
import ornl.elision.core.OperatorRef
import ornl.elision.core.Literal
import ornl.elision.core.Strategy
import ornl.elision.core.StringLiteral
import ornl.elision.core.GuardStrategy
import ornl.elision.core.SimpleApply

/**
 * A lambda variable does not match the argument.
 * 
 * @param loc   Location of the bad lambda body.
 * @param msg   Human-readable message.
 */
class LambdaVariableMismatchException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * A lambda application results in unbounded recursion.
 * 
 * @param loc   Location of the bad lambda argument.
 * @param msg   Human-readable message.
*  */
class LambdaUnboundedRecursionException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Handle the "application" of one atom to another.  The first (left hand)
 * atom is the _operator_, and the second (right hand) atom is the _argument_,
 * in an informal sense.
 * 
 * This object provides the rules to interpret the application based on the
 * choice of atoms.
 * 
 * @param ophandler     A class that understands how to handle an operator
 *                      application.  These are special since they may involve
 *                      native dispatch.
 */
class ApplyBuilder(ophandler: OperatorApplyHandler) {
  
  /**
   * Turn a pair into a binding to be returned from a strategy.  This turns
   * a pair of the form `(a, f)` into a binding of the form
   * `{binds atom->a flag->f}`.
   * 
   * @param pair    The pair of atom and flag.
   * @return  The binding of atom and flag.
   */
  private def pair2bind(pair: (BasicAtom, Boolean)): Bindings = {
    Bindings("atom" -> pair._1, "flag" -> Literal(pair._2))
  }
  
  /**
   * Apply a strategy to the provided argument atom.  This method allows for
   * testing (hence its name) whether the given strategy applies.
   * 
   * @param strat     The strategy to apply.
   * @param arg       The argument.
   * @param builder   The builder needed to build atoms.
   * @param strategy  The guard strategy to use for new rules.
   * @return  The pair of result atom and flag.
   */
  def test(strat: Strategy, arg: BasicAtom, builder: Builder,
      strategy: GuardStrategy): (BasicAtom, Boolean) = {
    strat match {
      case op_mappair: MapPair =>
        // Handle the case of a map pair.  A map pair is a primitive sort of
        // rewrite rule that consists of a pattern and a rewrite, and nothing
        // else.
        Matcher(op_mappair.left, arg, builder, strategy, Bindings(), None) match {
          case file:Fail => 
            (arg, false)
            
          case Match(binds) =>
            (builder.rewrite(op_mappair.right, binds, strategy)._1, true)
            
          case Many(iter) =>
            (builder.rewrite(op_mappair.right, iter.next, strategy)._1, true)
        }

      case op_rule: RewriteRule =>
        // A rewrite rule generalizes both the match atom and the map pair
        // as a package (a special form) to perform controlled rewriting.
        RuleApplyHandler(op_rule, arg, Bindings(), None, builder)
        
      case c =>
        // We come here if we find an unsupported strategy class.
        throw new ElisionException(Loc.internal,
            "Internal error.  Unrecognized strategy class: " + c.getClass)
    }
  }

  /**
   * Apply one atom (the _operator_) to another (the _argument_). 
   * 
   * @param op        The operator.
   * @param arg       The argument.
   * @param builder   The builder necessary to create atoms.
   * @param strategy  The strategy to use to rewrite guards in rules.
   * @param bypass    If true, bypass native handlers.
   * @return  The result of applying the operator to the argument.
   */
  def apply(op: BasicAtom, arg: BasicAtom, builder: Builder,
      strategy: GuardStrategy, bypass: Boolean = false): BasicAtom = {
    op match {
      case StringLiteral(loc, typ, str) if arg.isInstanceOf[StringLiteral] =>
        // If the argument is also a string literal, then we want to simply
        // concatenate them.
        new StringLiteral(Loc.internal, typ,
            str + arg.asInstanceOf[StringLiteral].value)
          
      case op_ap: AlgProp =>
        // Handle the case of algebraic properties being applied.  These can
        // operate as a function to modify other algebraic property specs.
        arg match {
          /* A Note to Maintainers
           * Remember that for the "and" method the properties of the second
           * override those of the first.
           */
          case ap: AlgProp =>
            (ap and op_ap)
            
          case as: AtomSeq => 
            builder.newAtomSeq(Loc.internal, as.props and op_ap, as.atoms)
            
          case _ => 
            new SimpleApply(Loc.internal, op_ap, arg)
        }

      case op_bind: BindingsAtom =>
        // Handle the case of applying a binding to another atom.  Bindings
        // operate as an operator that rewrites the argument.  In prior versions
        // a single symbol could be used to extract a bound value from the
        // binding.  That no longer applies.
        //
        // Try to rewrite the argument using the bindings and whatever we get
        // back is the result.
        builder.rewrite(arg, op_bind.mybinds, strategy)._1

      case op_lambda: Lambda =>
        // Handle the case of applying a lambda to an atom.  A lambda is a
        // (unnamed) operator that matches its argument against its parameter,
        // and uses the resuting bindings to rewrite its body.  Do that now.
        //
        // Lambdas are very general; their application can lead to a stack
        // overflow because it is possible to model unbounded recursion.  Catch
        // the stack overflow here, and bail out.
        try {
          // Make it possible to check types by matching the variable against the
          // argument instead of just binding.  For pure binding without checking
          // types, use a bind.
          Matcher(op_lambda.lvar, arg, builder, strategy, Bindings(), None) match {
            case fail:Fail =>
              throw new LambdaVariableMismatchException(arg.loc,
                  "Lambda argument does not match parameter: " + fail.theReason)
            case Match(binds) =>
              // Great!  Now rewrite the body with the bindings.
              builder.rewrite(op_lambda.body, binds, strategy)._1
            case Many(iter) =>
              builder.rewrite(op_lambda.body, iter.next, strategy)._1
          }
        } catch {
          case ex:java.lang.StackOverflowError =>
            // Trapped unbounded recursion.
            throw new LambdaUnboundedRecursionException(arg.loc,
                "Lambda application results in unbounded recursion: (" +
                op_lambda.toParseString + ").(" + arg.toParseString + ")")
        }
        
      case op_strat: Strategy =>
        // Handle a strategy.
        pair2bind(test(op_strat, arg, builder, strategy))
        
      case op_opref: OperatorRef =>
        // An operator reference holds an operator as a "closure."  We need to
        // extract the operator and then apply it.
        apply(op_opref.operator, arg, builder, strategy, bypass)
        
      case op_op: Operator =>
        // An operator can be applied to another atom, potentially resulting
        // in a new atom being instantiated.
        try {
          ophandler(op_op, arg, builder, bypass)
        } catch {
          case ex: StackOverflowError =>
            // Trapped unbounded recursion... probably.
            throw new LambdaUnboundedRecursionException(arg.loc,
                "Application results in unbounded recursion: (" +
                op.toParseString + ").(" + arg.toParseString + ")")
        }
        
      case c =>
        // Nothing special must be done.  Just make the appropriate simple
        // apply instance.
        new SimpleApply(Loc.internal, op, arg)
    }
  }
}
