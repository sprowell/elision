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
  val tag = Literal('operator)

  /**
   * Construct an operator from the provided special form data.
   *
   * @param sfh   The parsed special form data.
   * @return  An operator.
   */
  def apply(sfh: SpecialFormHolder): Operator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "cases" -> false, "params" -> false,
        "type" -> false, "description" -> false, "detail" -> false,
        "evenmeta" -> false, "handler" -> false))
    if (bh.either("cases", "params") == "cases") {
      CaseOperator(sfh)
    } else {
      TypedSymbolicOperator(sfh)
    }
  }

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
 * @param name				The operator name.
 * @param typ					The type of the fully-applied operator.
 * @param definition	A definition for the operator.
 * @param description	An optional short description for the operator.
 * @param detail			Optional detailed help for the operator.
 * @param evenMeta		Apply this operator even to meta-terms.  This is false
 * 										by default, and you should probably leave it alone.
 */
abstract class Operator(
  sfh: SpecialFormHolder,
  val name: String,
  val typ: BasicAtom,
  definition: AtomSeq,
  val description: String,
  val detail: String,
  override val evenMeta: Boolean = false)
  extends SpecialForm(sfh.loc, sfh.tag, sfh.content) with Applicable {
}

/**
 * Construction and matching of macros (case operators).
 */
object CaseOperator {
  /**
   * Make a case operator from the given special form data.
   *
   * @param sfh	The special form data.
   * @return	The case operator.
   */
  def apply(sfh: SpecialFormHolder): CaseOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "cases" -> true, "type" -> false,
      "description" -> false, "detail" -> false, "evenmeta" -> false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val cases = bh.fetchAs[AtomSeq]("cases")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    var description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    if (description.value(0) == '|') description = description.value.stripMargin('|')
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    val evenMeta = bh.fetchAs[BooleanLiteral]("evenmeta", Some(false)).value
    return new CaseOperator(sfh, name, typ, cases, description, detail, evenMeta)
  }

  /**
   * Make a case operator from the components.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					Operator name.
   * @param typ						The operator type (may be `ANY`).
   * @param cases					The cases, as a sequence of atoms.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @return	The new case operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, cases: AtomSeq,
    description: String, detail: String,
    evenMeta: Boolean = false): CaseOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name" -> nameS) + ("cases" -> cases) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail))
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)

    return new CaseOperator(sfh, name, typ, cases, description, detail, evenMeta)
  }

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
 * @param sfh						Special form data.
 * @param name					The operator name.
 * @param typ						The operator type.
 * @param cases					The definition.
 * @param description		An optional short description for the operator.
 * @param detail				Optional detailed help for the operator.
 * @param evenMeta			Apply this operator even when the arguments contain
 * 											meta-terms.  This is not advisable, and you should
 * 											probably leave this with the default value of false.
 */
class CaseOperator private (sfh: SpecialFormHolder,
  name: String, typ: BasicAtom, val cases: AtomSeq,
  description: String, detail: String, evenMeta: Boolean)
  extends Operator(sfh, name, typ, cases, description, detail, evenMeta) {
  /** The type of the operator is the provided type. */
  override val theType = typ
}

/**
 * Construction and matching of typed symbolic operators.
 */
object TypedSymbolicOperator {
  /**
   * Make a typed symbolic operator from the provided special form data.
   *
   * @param sfh		The parsed special form data.
   * @return	The typed symbolic operator.
   */
  def apply(sfh: SpecialFormHolder): TypedSymbolicOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "params" -> true, "type" -> false,
      "description" -> false, "detail" -> false, "evenmeta" -> false,
      "handler" -> false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val params = bh.fetchAs[AtomSeq]("params")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    var description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    if (description.length > 0 && description.value(0) == '|')
      description = description.value.stripMargin('|')
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    val evenMeta = bh.fetchAs[BooleanLiteral]("evenmeta", Some(false)).value

    // Fetch the handler text, if any was provided.
    val handlertxt = bh.fetchAs[StringLiteral]("handler", Some(null)) match {
      case null => None
      case x: StringLiteral => Some(x.value)
    }

    // Now create the operator.
    new TypedSymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handlertxt)
  }
  
  /**
   * Make a typed symbolic operator from the provided parts.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @param handler       Optional native handler code.  Default is `None`.
   * @return	The typed symbolic operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, params: AtomSeq,
    description: String, ddetail: String, evenMeta: Boolean = false,
    handler: Option[String] = None): TypedSymbolicOperator = {
    val detail = ddetail
    val nameS = Literal(Symbol(name))
    var binds = Bindings() + ("name" -> nameS) + ("params" -> params) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail)) + ("evenmeta" -> Literal(evenMeta))
    handler match {
      case None =>
      case Some(text) => binds += ("handler" -> Literal(text))
    }
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)
    return new TypedSymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handler)
  }

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
 * @param sfh         The parsed special form data.
 * @param name        The operator name.
 * @param typ         The type of the fully-applied operator.
 * @param params      The operator parameters.
 * @param evenMeta    Apply this operator even when the arguments contain
 *                    meta-terms.  This is not advisable, and you should
 *                    probably leave this with the default value of false.
 * @param handlertxt  The text for an optional native handler.
 */
class TypedSymbolicOperator private (sfh: SpecialFormHolder,
  name: String, typ: BasicAtom, params: AtomSeq,
  description: String, detail: String, evenMeta: Boolean,
  handlertxt: Option[String])
  extends SymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handlertxt) {
  /**
   * The type of an operator is a mapping from the operator domain to the
   * operator codomain.
   */
  override val theType = SymbolicOperator.makeOperatorType(params, typ)
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
   * Make a symbolic operator from the provided parts.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @param handler       The text for an optional native handler.
   * @return	The typed symbolic operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, params: AtomSeq,
    description: String, ddetail: String, evenMeta: Boolean = false,
    handler: Option[String] = None): SymbolicOperator = {
    val detail = ddetail
    val nameS = Literal(Symbol(name))
    var binds = Bindings() + ("name" -> nameS) + ("params" -> params) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail)) + ("evenmeta" -> Literal(evenMeta))
    handler match {
      case None =>
      case Some(text) => binds += ("handler" -> Literal(text))
    }
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)
    return new SymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handler)
  }

  /**
   * Extract the parts of a symbolic operator.
   *
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: SymbolicOperator) = Some((so.name, so.theType, so.params))

  /**
   * The well-known MAP operator.  This is needed to define the types of
   * operators, but is not used to define its own type.  The type of the MAP
   * operator is ^TYPE, indicating that it is a root type.  We could, with
   * great justice, use xx (the cross product) for this operator, but don't.
   * This makes the types of operators look more natural when viewed.
   */
  val MAP = OperatorRef(
    SymbolicOperator(Loc.internal, "MAP", TypeUniverse, AtomSeq(NoProps,
        'domain, 'codomain),
      "Mapping constructor.",
      "This operator is used to construct types for operators.  It " +
      "indicates a mapping from one type (the domain) to another type " +
      "(the codomain)."))
  /**
   * The well-known cross product operator.  This is needed to define the
   * types of operators, but is not used to define its own type.  The type
   * of the cross product is ANY.  Note that it must be ANY, since it is
   * associative.
   */
  val xx = OperatorRef(
    SymbolicOperator(Loc.internal, "xx", ANY, AtomSeq(Associative(true), 'x, 'y),
      "Cross product.",
      "This operator is used to construct types for operators.  It " +
      "indicates the cross product of two atoms (typically types).  " +
      "These originate from the types of the parameters of an operator."))
  /**
   * The well-known list operator.  This is used to define the type of lists
   * such as the atom sequence.  It has type ^TYPE, indicating that it is a
   * root type.
   */
  val LIST = OperatorRef(
    SymbolicOperator(Loc.internal, "LIST", TypeUniverse, AtomSeq(NoProps, 'type),
      "List type constructor.",
      "This operator is used to indicate the type of a list.  It takes a " +
      "single argument that is the type of the atoms in the list.  For " +
      "heterogeneous lists this will be ANY."))

  /**
   * Compute an operator type.
   *
   * @param params	The parameters.
   * @param typ			The type of the fully-applied operator.
   * @return	The type for the operator.
   */
  def makeOperatorType(params: AtomSeq, typ: BasicAtom) =
    params.length match {
      case 0 => MAP(NONE, typ)
      case 1 => MAP(params(0).theType, typ)
      case _ => MAP(xx(params.map(_.theType): _*), typ)
    }
}

/**
 * Encapsulate a symbolic operator.
 *
 * == Purpose ==
 * An (untyped) symbolic operator is a rudimentary form of operator used only
 * for special "primitive" operators that are themselves used to specify the
 * types of operators.
 *
 * @param sfh         The parsed special form data.
 * @param name        The operator name.
 * @param typ         The type of the fully-applied operator.
 * @param params		  The operator parameters.
 * @param evenMeta    Apply this operator even when the arguments contain
 *                    meta-terms.  This is not advisable, and you should
 *                    probably leave this with the default value of false.
 * @param handlertxt  The text for an optional native handler.
 */
protected class SymbolicOperator protected (sfh: SpecialFormHolder,
  name: String, typ: BasicAtom, val params: AtomSeq,
  description: String, detail: String, evenMeta: Boolean,
  val handlertxt: Option[String])
  extends Operator(sfh, name, typ, params, description, detail, evenMeta) {
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
