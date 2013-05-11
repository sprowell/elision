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
======================================================================*/
package ornl.elision.core

import scala.math.BigInt.int2bigInt
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.util.OmitSeq.fromIndexedSeq
import ornl.elision.util.Timeable

/**
 * An incorrect argument list was supplied to an operator.
 *
 * @param loc The location of the bad application.
 * @param msg The human-readable message describing the problem.
 */
class ArgumentListException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Provide construction and matching for operators.
 */
object Operator {
  
  /** Tag for this special form. */
  val tag = new SymbolLiteral(Loc.internal, 'operator)

  /**
   * Extract the parts of an operator.
   *
   * @param op    The operator.
   * @return  The triple of name, type, and definition.
   */
  def unapply(op: Operator) = op match {
    case so: SymbolicOperator => Some((so.name, so.theType, so.params))
    case co: CaseOperator => Some((co.name, co.theType, co.cases))
  }
}

/**
 * Encapsulate an operator.
 *
 * This is the common base class for all operator classes.
 *
 * == Purpose ==
 * An operator, by itself, is simply a function that maps from some
 * domain to some codomain.
 *
 * == Use ==
 * The companion object provides methods to create operators.
 *
 * @param loc         The location.
 * @param content     The content.  This is lazily evaluated.
 * @param name				The operator name.
 * @param typ					The type of the fully-applied operator.
 * @param definition	A definition for the operator.
 * @param description	An optional short description for the operator.
 * @param detail			Optional detailed help for the operator.
 * @param evenMeta		Apply this operator even to meta-terms.  This is false
 * 										by default, and you should probably leave it alone.
 */
abstract class Operator(
    val loc: Loc,
    content: => BasicAtom,
    val name: String,
    val typ: BasicAtom,
    definition: AtomSeq,
    val description: String,
    val detail: String,
    override val evenMeta: Boolean = false)
    extends SpecialForm(loc, Operator.tag, content) with Applicable {
  
  /**
   * Apply the operator to the given argument list.
   * 
   * @param args    The argument list.
   */
  def apply(args: IndexedSeq[BasicAtom]): BasicAtom
  
  /**
   * Apply the operator to the given argument list.
   * 
   * @param args    The argument list.
   */
  def apply(args: BasicAtom*): BasicAtom = {
    apply(args.toIndexedSeq)
  }
}

/**
 * Construction and matching of macros (case operators).
 */
object CaseOperator {

  /**
   * Extract the parts of a case operator.
   *
   * @param co	The case operator.
   * @return	A triple of the name, type, and cases.
   */
  def unapply(co: CaseOperator) = Some((co.name, co.theType, co.cases,
      co.description, co.detail))
}

/**
 * Encapsulate a case operator.
 *
 * == Purpose ==
 * A case operator is actually a kind of macro.  Its definition consists of
 * a sequence of atoms.  When applied to some atom ''A'', it proceeds as
 * follows, considering each atom in its definition, in order.
 *  - If the atom is a rewriter, apply it and, if the success flag is true,
 *    the value is the result.  Otherwise continue.
 *  - If the atom is an applicable, apply it.  The value is the result of
 *    the application.
 *  - If the atom is neither a rewriter nor an applicable, just return that
 *    atom as the result.
 * If the end of the list is reached and no value is determined, then an
 * error is generated (an `ArgumentListException`).
 *
 * @param loc           The location.
 * @param content       The content.  This is lazily evaluated.
 * @param name					The operator name.
 * @param typ						The operator type.
 * @param cases					The definition.
 * @param description		An optional short description for the operator.
 * @param detail				Optional detailed help for the operator.
 * @param evenMeta			Apply this operator even when the arguments contain
 * 											meta-terms.  This is not advisable, and you should
 * 											probably leave this with the default value of false.
 */
abstract class CaseOperator protected[elision] (
    loc: Loc,
    content: => BasicAtom,
    name: String,
    typ: BasicAtom,
    val cases: AtomSeq,
    description: String,
    detail: String,
    evenMeta: Boolean) extends Operator(loc, content, name, typ, cases,
        description, detail, evenMeta) {
  
  /**
   * Alternate constructor for an operator omitting the special form content
   * binding.
   * 
   * @param loc           The location.
   * @param name          The operator name.
   * @param typ           The operator type.
   * @param cases         The definition.
   * @param description   An optional short description for the operator.
   * @param detail        Optional detailed help for the operator.
   * @param evenMeta      Apply this operator even when the arguments contain
   *                      meta-terms.  This is not advisable, and you should
   *                      probably leave this with the default value of false.
   */
  def this(
      loc: Loc,
      name: String,
      typ: BasicAtom,
      cases: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean) = {
    this(loc, new BindingsAtom(loc, Bindings {
      "name" -> new SymbolLiteral(Loc.internal, Symbol(name))
      "type" -> typ
      "cases" -> cases
      "description" -> new StringLiteral(Loc.internal, description)
      "detail" -> new StringLiteral(Loc.internal, detail)
      "evenmeta" -> new BooleanLiteral(Loc.internal, evenMeta)
    }), name, typ, cases, description, detail, evenMeta)
  }

  /** The type of the operator is the provided type. */
  override val theType = typ
}

/**
 * Construction and matching of typed symbolic operators.
 */
object TypedSymbolicOperator {

  /**
   * Extract the parts of a typed symbolic operator.
   *
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: TypedSymbolicOperator) =
    Some((so.name, so.typ, so.params, so.description, so.detail,
        so.evenMeta, so.handlertxt))
}

/**
 * Encapsulate a typed symbolic operator.
 *
 * == Purpose ==
 * A ''typed'' symbolic operator computes its type based on the types of its
 * parameters and the provided "fully applied" type.  The result has the form
 * of a mapping from a domain to a co-domain.
 *
 * @param loc           The location.
 * @param tag           The tag.
 * @param content       The content.  This is lazily evaluated.
 * @param name          The operator name.
 * @param typ           The type of the fully-applied operator (codomain).
 * @param optype        The type of the operator.
 * @param params        The operator parameters.
 * @param description   An optional short description for the operator.
 * @param detail        Optional detailed help for the operator.
 * @param evenMeta      Apply this operator even when the arguments contain
 *                      meta-terms.  This is not advisable, and you should
 *                      probably leave this with the default value of false.
 * @param handlertxt    The text for an optional native handler.
 */
abstract class TypedSymbolicOperator protected[elision] (
    loc: Loc,
    content: => BasicAtom,
    name: String,
    typ: BasicAtom,
    optype: BasicAtom,
    params: AtomSeq,
    description: String,
    detail: String,
    evenMeta: Boolean,
    handlertxt: Option[String]) extends SymbolicOperator(loc, content, name,
        optype, params, description, detail, evenMeta, handlertxt) {
  
  /**
   * Alternate constructor for an operator omitting the special form content
   * binding.
   * 
   * @param loc           The location.
   * @param name          The operator name.
   * @param typ           The type of the fully-applied operator (codomain).
   * @param optype        The type of the operator.
   * @param params        The operator parameters.
   * @param description   An optional short description for the operator.
   * @param detail        Optional detailed help for the operator.
   * @param evenMeta      Apply this operator even when the arguments contain
   *                      meta-terms.  This is not advisable, and you should
   *                      probably leave this with the default value of false.
   * @param handlertxt    The text for an optional native handler.
   */
  def this(
      loc: Loc,
      name: String,
      typ: BasicAtom,
      optype: BasicAtom,
      params: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean,
      handlertxt: Option[String]) = {
    this(loc, new BindingsAtom(loc, Bindings {
      "name" -> new SymbolLiteral(Loc.internal, Symbol(name))
      "type" -> typ
      "params" -> params
      "description" -> new StringLiteral(Loc.internal, description)
      "detail" -> new StringLiteral(Loc.internal, detail)
      "evenmeta" -> new BooleanLiteral(Loc.internal, evenMeta)
    }), name, typ, optype, params, description, detail, evenMeta, handlertxt)
  }
  
  /**
   * The type of an operator is a mapping from the operator domain to the
   * operator codomain.
   */
  override val theType = optype
}

/**
 * Construction and matching of symbolic operators.
 */
object SymbolicOperator {
  
  /**
   * Base class for data to pass to a native handler.  This must be
   * concretized elsewhere.
   */
  protected[elision] class AbstractApplyData

  /**
   * Extract the parts of a symbolic operator.
   *
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: SymbolicOperator) = Some((so.name, so.theType, so.params))
}

/**
 * Encapsulate a symbolic operator.
 *
 * == Purpose ==
 * An (untyped) symbolic operator is a rudimentary form of operator used only
 * for special "primitive" operators that are themselves used to specify the
 * types of operators.
 *
 * @param loc           The location.
 * @param tag           The tag.
 * @param content       The content.  This is lazily evaluated.
 * @param name          The operator name.
 * @param typ           The type of the fully-applied operator.
 * @param params        The operator parameters.
 * @param description   An optional short description for the operator.
 * @param detail        Optional detailed help for the operator.
 * @param evenMeta      Apply this operator even when the arguments contain
 *                      meta-terms.  This is not advisable, and you should
 *                      probably leave this with the default value of false.
 * @param handlertxt    Optional text for a native handler.  None by default.
 */
class SymbolicOperator protected[elision] (
    loc: Loc,
    content: => BasicAtom,
    name: String,
    typ: BasicAtom,
    val params: AtomSeq,
    description: String = "no description",
    detail: String = "no detail",
    evenMeta: Boolean = false,
    val handlertxt: Option[String] = None) extends Operator(loc, content, name,
        typ, params, description, detail, evenMeta) {
    
  /**
   * Alternate constructor for an operator omitting the special form content
   * binding.
   * 
   * @param loc           The location.
   * @param name          The operator name.
   * @param typ           The type of the fully-applied operator.
   * @param params        The operator parameters.
   * @param description   An optional short description for the operator.
   * @param detail        Optional detailed help for the operator.
   * @param evenMeta      Apply this operator even when the arguments contain
   *                      meta-terms.  This is not advisable, and you should
   *                      probably leave this with the default value of false.
   * @param handlertxt    The text for an optional native handler.
   */
  def this(
      loc: Loc,
      name: String,
      typ: BasicAtom,
      params: AtomSeq,
      description: String = "no description",
      detail: String = "no detail",
      evenMeta: Boolean = false,
      handlertxt: Option[String] = None) = {
    this(loc, new BindingsAtom(loc, Bindings {
      "name" -> new SymbolLiteral(Loc.internal, Symbol(name))
      "type" -> typ
      "params" -> params
      "description" -> new StringLiteral(Loc.internal, description)
      "detail" -> new StringLiteral(Loc.internal, detail)
      "evenmeta" -> new BooleanLiteral(Loc.internal, evenMeta)
    }), name, typ, params, description, detail, evenMeta, handlertxt)
  }

  // Save the type.  Symbolic operators can't construct their type like the
  // typed symbolic operators can (thus the names).
  override val theType: BasicAtom = ANY
  
  /**
   * Hold a native handler, if any.  Other components are responsible for
   * compiling and caching the native handler here.
   * 
   * This is used internally by the Elision native compilation system.  Do
   * not modify this or tamper with it in any way!
   */
  protected[elision] var
  handler: Option[(SymbolicOperator.AbstractApplyData => BasicAtom)] = None

  // Check the properties.
  _check()

  /**
   * Check the parameters against the properties.  If any problems are detected,
   * then an exception is thrown (`ArgumentListException`).
   */
  private def _check() {
    /**
     * Define a little method to require that all parameters have the same
     * type.
     *
     * @return	True if all parameters have the same type, and false if not.
     */
    def paramTypeCheck = {
      val aType = params(0).theType
      params.forall(_.theType == aType)
    }

    // Check the properties and make sure everything is in accordance with
    // them.
    if (params.props.isA(false)) {
      // There must be exactly two parameters.
      if (params.length != 2) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but does not have exactly two " +
          "parameters, as required: " + params.toParseString)
      }
      // All parameter types must be the same.
      if (!paramTypeCheck) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but all parameters do not have the " +
          "same type, as required: " + params.toParseString)
      }
      // The fully-applied type must be the same as the parameter type.
      if (params(0).theType != typ) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but the parameter type (" +
          params(0).theType.toParseString +
          ") is not the same as the fully-applied type (" +
          typ.toParseString + ").")
      }
    } else {
      // The operator is not associative, so it must not have an identity,
      // absorber, or be idempotent.
      if (params.props.isI(false)) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as idempotent, but it not marked as associative, as" +
          " required.")
      }
      if (params.props.identity.isDefined) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is declared to have an identity, but it not marked as " +
          "associative, as required.")
      }
      if (params.props.absorber.isDefined) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is declared to have an absorber, but it not marked as " +
          "associative, as required.")
      }
    }
    if (params.props.isC(false)) {
      // There must be at least two parameters.
      if (params.length < 2) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as commutative, but does not have at least two " +
          "parameters, as required: " + params.toParseString)
      }
      // All parameter types must be the same.
      if (!paramTypeCheck) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as commutative, but all parameters do not have the " +
          "same type, as required: " + params.toParseString)
      }
    }
    // Any identity must have the same type as the parameters.
    if (params.props.identity.isDefined) {
      if (! (params(0).theType equals params.props.identity.get.theType)) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " has an identity whose type (" +
          params.props.identity.get.theType.toParseString +
          ") does not match the parameter type (" +
          params(0).theType.toParseString + ").")
      }
    }
    // Any absorber must have the same type as the parameters.
    if (params.props.absorber.isDefined) {
      if (! (params(0).theType equals params.props.absorber.get.theType)) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " has an absorber whose type (" +
          params.props.absorber.get.theType.toParseString +
          ") does not match the parameter type (" +
          params(0).theType.toParseString + ").")
      }
    }
  }
}
