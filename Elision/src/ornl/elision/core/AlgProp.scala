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
 * ======================================================================*/
package ornl.elision.core

import ornl.elision.util.ElisionException
import ornl.elision.util.hashify
import ornl.elision.util.other_hashify
import ornl.elision.util.Loc

/**
 * Indicate a properties specification is illegal.  This typically indicates a
 * bad value for a property (setting commutativity to a non Boolean value, for
 * instance, such as "5") or using properties incorrectly (such as
 * specifying idempotency but not associativity).
 * 
 * @param loc Location of the bad property specification.
 * @param msg	Human readable message.
 */
class IllegalPropertiesSpecification(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Simplified creation and matching for algebraic properties objects.
 */
object AlgProp {
  
  /**
   * Pull apart an algebraic properties object.
   * 
   * @param ap  The algebraic properties object.
   * @return  Associativity, commutativity, idempotency, absorber, and identity.
   */
  def unapply(ap: AlgProp) = Some((ap.associative, ap.commutative,
      ap.idempotent, ap.absorber, ap.identity))
}

/**
 * Encapsulate the algebraic properties ascribed to some object.
 * 
 * == Properties ==
 * The following properties are supported.
 *  - ''Associativity'' implies that children can be arbitrarily grouped.
 *    For example, string concatenation is associative.
 *  - ''Commutativity'' implies that children can be arbitrarily ordered.
 *    For example, multiplication is commutative.
 *  - ''Idempotency'' implies that repeated children are ignored.  For
 *    example, Boolean '''or''' is idempotent.
 *  - An ''absorber'' is a special atom ''A'' that, when added to the children,
 *    causes the result to evaluate to simply ''A''.  Zero is a multiplicative
 *    absorber.
 *  - An ''identity'' is a special atom ''I'' that can be introduced or
 *    omitted from the child list without changing the value.  Zero is the
 *    additive identity.
 *    
 * == Restrictions ==
 * Some properties require others.  At present idempotency requires
 * associativity.  If an absorber or identity is present, associativity is
 * also required.
 * 
 * == Use ==
 * To use this, make an instance and specify the properties.  There are several
 * child classes that may make this easier.
 *  - [[ornl.elision.core.NoProps]]
 *  - [[ornl.elision.core.Associative]]
 *  - [[ornl.elision.core.Commutative]]
 *  - [[ornl.elision.core.Idempotent]]
 *  - [[ornl.elision.core.Absorber]]
 *  - [[ornl.elision.core.Identity]]
 * These can be combined with `and` and the Boolean-valued properties negated
 * with `!`.  Thus one can write `Associative and !Commutative`.
 * 
 * Properties can be specified, or left unspecified.  The entire properties
 * object can be matched and rewritten.
 * 
 * == Application ==
 * Instances are applicable; applied to a typed list of atoms, they "overwrite"
 * the lists properties.
 * 
 * @param loc           Location of this specification.
 * @param associative		Optional associativity.  Default is none.
 * @param commutative		Optional commutativity.  Default is none.
 * @param idempotent		Optional idempotency.  Default is none.
 * @param absorber			Optional absorber.  Default is none.
 * @param identity			Optional identity.  Default is none.
 */
class AlgProp protected[elision] (
    loc: Loc,
    val associative: Option[BasicAtom] = None,
    val commutative: Option[BasicAtom] = None,
    val idempotent: Option[BasicAtom] = None,
    val absorber: Option[BasicAtom] = None,
    val identity: Option[BasicAtom] = None)
    extends BasicAtom(loc) with Applicable {
  
  // Put all the properties in a list for quick access.
  private val _plist =
    List(associative, commutative, idempotent, absorber, identity)
  
  lazy val otherHashCode =
    (this.toString).foldLeft(BigInt(0))(other_hashify)+1

  override lazy val hashCode = _plist.foldLeft(0)(hashify)
  
  // Type check the Boolean properties.
  private def _isNotBool(opt: Option[BasicAtom]) = opt match {
    case Some(ANY) => false
    case Some(atom) =>
      atom.theType match {
        case ANY => false
        case BOOLEAN => false
        case _ => true
      }
    case None => false
  }
  if (_isNotBool(associative))
    throw new IllegalPropertiesSpecification(loc,
        "Associativity value must be a Boolean, but the provided value was: " +
        associative.get.toParseString)
  if (_isNotBool(commutative))
    throw new IllegalPropertiesSpecification(loc,
        "Commutativity value must be a Boolean, but the provided value was: " +
        commutative.get.toParseString)
  if (_isNotBool(idempotent))
    throw new IllegalPropertiesSpecification(loc,
        "Idempotency value must be a Boolean, but the provided value was: " +
        idempotent.get.toParseString)
  
  // Without associativity, there cannot be idempotency, identities, or
  // absorbers.
  if (!isA(true)) {
    if (isI(false))
      throw new IllegalPropertiesSpecification(loc,
          "Idempotency requires associativity.")
    if (getB(null) != null)
      throw new IllegalPropertiesSpecification(loc,
          "An absorber requires associativity.")
    if (getD(null) != null)
      throw new IllegalPropertiesSpecification(loc,
          "An identity requires associativity.")
  }
  
  // All algebraic properties have type ^TYPE.
  val theType = TypeUniverse
  
  lazy val depth = _plist.foldLeft(0) {
    (dbi: Int, opt: Option[BasicAtom]) => dbi max (opt match {
      case None => 0
      case Some(atom) => atom.depth
    })
  } + 1
  
  lazy val isTerm =
    _plist.foldLeft(true)(_ && _.getOrElse(Literal.TRUE).isTerm)
  
  lazy val isConstant =
    _plist.foldLeft(true)(_ && _.getOrElse(Literal.TRUE).isConstant)
  
  lazy val deBruijnIndex =
    _plist.foldLeft(0)(_ max _.getOrElse(Literal.TRUE).deBruijnIndex)
  
  /**
   * Fast check for associativity.
   * 
   * @param default	What to return if unspecified.
   */
  def isA(default: Boolean) = associative match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for commutativity.
   * 
   * @param default	What to return if unspecified.
   */
  def isC(default: Boolean) = commutative match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for idempotency.
   * 
   * @param default	What to return if unspecified.
   */
  def isI(default: Boolean) = idempotent match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for absorber.
   * 
   * @param default	What to return if unspecified.
   */
  def getB(default: BasicAtom) = absorber match {
    case Some(atom) => atom
    case _ => default
  }
  
  /**
   * Fast check for identity.
   * 
   * @param default	What to return if unspecified.
   */
  def getD(default: BasicAtom) = absorber match {
    case Some(atom) => atom
    case _ => default
  }
  
  /**
   * Join two optional atoms together.  The second, if specified, overrides
   * the first.
   * 
   * @param a1	The first atom.
   * @param a2	The second atom.
   * @return	The result.
   */
  private def joinatoms(a1: Option[BasicAtom],
      a2: Option[BasicAtom]) = (a1, a2) match {
    case (_, None) => a1
    case (_, Some(atom)) => a2
  }

  /**
   * Combine this with another property and yield the resulting property.
   * Properties in the second override properties in the first, if they
   * are specified.
   *
   * @param other	Another properties list to consider.
   * @return	A new algebraic properties list.
   */
  def and(other: AlgProp) = {
    new AlgProp(loc,
      joinatoms(associative, other.associative),
      joinatoms(commutative, other.commutative),
      joinatoms(idempotent, other.idempotent),
      joinatoms(absorber, other.absorber),
      joinatoms(identity, other.identity))
  }

  /**
   * Invert a single Boolean option.
   * 
   * @param opt		An optional atom.
   * @return	The atom, with its sense inverted.
   */
  private def invert(opt: Option[BasicAtom]) = opt match {
    case Some(Literal.TRUE) => Some(Literal.FALSE)
    case Some(Literal.FALSE) => Some(Literal.TRUE)
    case _ => opt
  }

  /**
   * Invert the sense of the specified properties.
   *
   * @return	The new algebraic properties list.
   */
  def unary_! = {
    new AlgProp(loc, invert(associative), invert(commutative),
        invert(idempotent))
  }
  
  /**
   * Determine if this atom is equal to another atom.
   * 
   * @param other	The other atom.
   * @return	True iff the atoms are equal.
   */
  override def equals(other: Any) = other match {
    case ap:AlgProp =>
      associative == ap.associative &&
      commutative == ap.commutative &&
      idempotent == ap.idempotent &&
      absorber == ap.absorber &&
      identity == ap.identity
      
    case _ => false
  }

  /**
   * Generate a descriptive string.
   * 
   * @return	The string.
   */
  def toHumaneString = {
    var list = List[String]()
    associative match {
	    case Some(Literal.TRUE) => list :+= "associative"
	    case Some(Literal.FALSE) => list :+= "not associative"
	    case Some(atom) => list :+= "associative=[" + atom.toParseString + "]"
	    case _ =>
	  }
    commutative match {
	    case Some(Literal.TRUE) => list :+= "commutative"
	    case Some(Literal.FALSE) => list :+= "not commutative"
	    case Some(atom) => list :+= "commutative=[" + atom.toParseString + "]"
	    case _ =>
	  }
    idempotent match {
	    case Some(Literal.TRUE) => list :+= "idempotent"
	    case Some(Literal.FALSE) => list :+= "not idempotent"
	    case Some(atom) => list :+= "idempotent=[" + atom.toParseString + "]"
	    case _ =>
	  }
    absorber match {
	    case None =>
	    case Some(atom) => list :+= "absorber=[" + atom.toParseString + "]"
	  }
    identity match {
	    case None =>
	    case Some(atom) => list :+= "identity=[" + atom.toParseString + "]"
	  }
    if (list.length == 0) "no properties"
    else list.mkString(" and ")
  }
}

/** No properties. */
case object NoProps extends AlgProp(Loc.internal)

/** The associative property. */
case class Associative(atom: BasicAtom)
extends AlgProp(atom.loc, associative = Some(atom))

/** The commutative property */
case class Commutative(atom: BasicAtom)
extends AlgProp(atom.loc, commutative = Some(atom))

/** The idempotent property. */
case class Idempotent(atom: BasicAtom)
extends AlgProp(atom.loc, idempotent = Some(atom))

/**
 * An absorber.
 * 
 * @param atom	The absorber atom.
 */
case class Absorber(atom: BasicAtom)
extends AlgProp(atom.loc, absorber = Some(atom))

/**
 * An identity.
 * 
 * @param atom	The identity atom.
 */
case class Identity(atom: BasicAtom)
extends AlgProp(atom.loc, identity = Some(atom))
