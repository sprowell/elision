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
package ornl.elision.core

import scala.collection.mutable.BitSet
import scala.collection.mutable.{HashSet => MutableHashSet}
import ornl.elision.util.Debugger
import ornl.elision.util.HasOtherHash
import ornl.elision.util.Loc
import ornl.elision.dialects.Dialect
import scala.language.existentials

/**
 * This marker trait is used to frighten developers and strike fear into
 * anyone trying to parallelize something in the library.  If you see it,
 * '''BEWARE'''!  The associated class contains some form of mutable data!
 */
trait Mutable

/**
 * This marker trait is used to frighten developers and strike fear into
 * anyone trying to get some work done.  If you see it, '''BEWARE'''!  The
 * associated class is going to change dramatically, disappear, or explode
 * violently.
 */
trait Fickle

/**
 * Marker trait for atoms that can be applied to other atoms.
 */
trait Applicable

/**
 * Marker trait for atoms that implement a strategy.  These atoms are expected
 * to yield a binding when applied in the operator position to another atom.
 * The binding must have the form `atom->`_atom_ `flag->`_boolean_, where the
 * _atom_ is a potentially different atom, and the _boolean_ is a literal.
 */ 
trait Strategy extends Applicable

/**
 * The root of all atoms manipulated by the rewriter.
 *
 * To use this, extend the class as a new `case class`.  Then implement the
 * abstract methods and fields, and override any methods or fields you need to.
 * 
 * The following list is a short guide to some of the things you should do when
 * you implement `BasicAtom`.
 * 
 *  - Specify the type of the object.  To do this add `val theType = `(a basic
 *    atom).
 *    
 *  - Visit [[ornl.elision.context.AtomWalker]] and add code to traverse the atom.
 *  
 *  - Visit [[ornl.elision.context.Builder]] and add code or a method to make
 *    the atom.  If specialized processing is required then add that in an
 *    appropriate subclass, such as [[ornl.elision.context.StandardBuilder]].
 *  
 *  - Visit [[ornl.elision.dialects.ElisionGenerator]] and add code to create a
 *    string from the new atom.  This must return a string that is parseable by
 *    [[ornl.elision.parse.ElisionParser]] to re-create the atom.
 *    
 *  - Visit [[ornl.elision.dialects.ScalaGenerator]] and add code to create a
 *    string from the new atom.  This must return a string that is parseable by
 *    Scala to re-create the atom.  In many cases making the class into a
 *    `case class` will be sufficient, but if there are arguments that are
 *    primitive types, such as strings, whose toString method does not produce
 *    a parseable result, this must be adjusted.  Be sure to see the implicitly
 *    added `mkParseString` method found in the package object
 *    [[ornl.elision.core.package]], as this can help.
 *    
 *  - Write code to specify the De Bruijn index of the instance and add
 *    `val deBruijnIndex =` to set the index.  This can be computed as follows.
 *    - Instances with no children have De Bruijn index of zero.
 *    - Instances other than lambdas with children have index equal to the
 *      maximum index of their children.
 *    - Lambdas have index one greater than the index of their body, and the
 *      body must also be rewritten to replace the variable with a De Bruijn
 *      index variable.  The implementation of this is left to classes that
 *      implement lambdas.
 *    {{{
 *    // Common implementation with children.
 *    lazy val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
 *    }}}
 *      
 *  - Write code to compute the depth of the instance.  This can be computed
 *    as follows.
 *    - Instances that do not have children have depth of zero.
 *    - Instances with other atoms as children have depth equal to the maximum
 *      depth of their children, plus one.
 *    {{{
 *    // Common implementation with children.
 *    lazy val depth = children.foldLeft(0)(_ max _.depth) + 1
 *    }}}
 *      
 *  - Write code to determine if the instance is a constant.  A constant can be
 *    arbitrarily complex, but cannot contain variables.  This can be computed
 *    as follows.
 *    - Variables are not constants.
 *    - Literals are constants.
 *    - Instances with children are constant iff all their children are
 *      constant.
 *    {{{
 *    // Common implementation with children.
 *    lazy val isConstant = children.forall(_.isConstant)
 *    }}}
 *    
 *  - Specify whether this atom represents a term, or a metaterm.  If a term,
 *    then set `isTerm` to `true`.  Otherwise, set it to `false`.  An atom is
 *    a metaterm if it simply ''is'', or if it contains a metaterm.  In general
 *    the following will work.
 *    {{{
 *    // Common implementation of isTerm with children.
 *    lazy val isTerm = children.forall(_.isTerm)
 *    }}}
 * 
 * @param loc The location where this atom originated.
 */
abstract class BasicAtom(val loc: Loc = Loc.internal) extends HasOtherHash {
  import scala.collection.mutable.{Map => MMap}

  /**
   * The rulesets with respect to which this atom is clean. If this is
   * not None the atom has already been rewritten with some set of
   * rulesets. If None, the atom has never been rewritten.
   */
  var cleanRulesets = new BitSet()

  /** The type for the atom. */
  val theType: BasicAtom
  
  /** The De Bruijn index. */
  val deBruijnIndex: Int

  /**
   * An alternate hash code for a BasicAtom. An
   * alternate hash code is used in some cases to provide 2 different
   * hash codes for an Elision object. These 2 hash codes are used to
   * lower the chances of a hash collision (both different hash codes
   * will need to collide for a hash collision to occur).
   */
  val otherHashCode: BigInt

  /**
   * If true then this atom can be bound.  Only variables should be bound, so
   * override this for variables; it is `false` by default.
   */
  val isBindable: Boolean = false

  /**
   * If true, this atom represents false.  Override this for an atom that
   * represents false.
   */
  val isFalse: Boolean = false

  /**
   * If true, this atom represents true.  Override this for an atom that
   * represents true.
   */
  val isTrue: Boolean = false

  /**
   * Iff true, this is a De Bruijn index.  A De Bruijn index is a special kind
   * of variable that is used during alpha conversion of lambdas.  If you are
   * not writing code for a lambda, you can ignore this.  Otherwise you need to
   * convert variables bound by the lambda into De Bruijn indices.  This flag
   * is used to protect De Bruijn indices against further rewriting.
   */
  val isDeBruijnIndex = false
  
  /** If true then this atom denotes a constant (it contains no variables). */
  val isConstant: Boolean
  
  /** If true then this atom denotes a term.  If false, a metaterm. */
  val isTerm: Boolean
  
  /**
   * The depth of the atom.  An atom's depth is equal to the maximum depth
   * of its children, plus one.  An atom with no children has depth zero.
   */
  val depth: Int
  
  /**
   * Whether this atom should be applied, even on meta-terms.  Do not mess
   * with this; unless you are modifying the Elision language you want to
   * leave this alone.  Only exceptional operators that must operate
   * handlers to terms that will contain meta-terms need to change this.
   * For nearly every operator the default handling of meta-terms is
   * appropriate.
   */
  val evenMeta = false
  
  /**
   * Construct and cache the spouse of this object.
   */
  lazy val spouse = BasicAtom.buildSpouse(this)

  /**
   * Generate a parseable string from this atom.  The returned string should
   * be able to "round trip," that is, [[ornl.elision.parse.ElisionParser]] must
   * be able to parse it and return an atom equal to this one.
   * 
   * @return	The string.
   */
  def toParseString =
    Dialect.serialize('elision, new StringBuffer(), this).toString
  
  /**
   * Generate a parseable string from this atom.  The string is immediately
   * written ("streamed") to the given appendable.
   * 
   * @param app   The appendable to get the string.
   * @param limit A limit on the depth of the returned string.  By default this
   *              is negative one, for no limit.  Note that if the limit is
   *              set, and is exceeded, the string will not be parseable.
   * @return  The appendable.
   */
  def toParseString(app: Appendable, limit: Int = -1) =
    Dialect.serialize('elision, new StringBuffer(), this, limit=limit)
    
  /**
   * Generate a parseable string from this atom.
   * 
   * @param limit A limit on the depth of the returned string.  By default this
   *              is negative one, for no limit.  Note that if the limit is
   *              set, and is exceeded, the string will not be parseable.
   * @return  The string.
   */
  def toParseString(limit: Int) =
    Dialect.serialize('elision, new StringBuffer(), this, limit=limit).toString
  
  /**
   * Make a string that can be used to re-generate this atom.
   * 
   * @return  The string.
   */
  override def toString =
    Dialect.serialize('scala, new StringBuffer(), this).toString
}

/**
 * Mutable controls affecting all atoms and matching go here.
 * 
 * In addition you can find the helper method `buildConstantPool` to
 * compute the constant pool for an atom.
 */
object BasicAtom {

  /** Whether or not to provide type information in toParseString(). */
  var printTypeInfo = false

  /**
   * Every basic atom may have a "spouse" that is a different object.
   * This field specifies a closure to create the spouse object.  The
   * basic atom constructor calls this closure, passing the basic atom
   * itself, and caching the returned object.
   * 
   * The default implementation of this simply returns the object itself.
   */
  var buildSpouse: (BasicAtom) => T forSome {type T <: AnyRef} =
    ((obj: BasicAtom) => obj)
}
