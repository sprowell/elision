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

import scala.collection.mutable.HashSet
import scala.collection.mutable.OpenHashMap
import ornl.elision.util.other_hashify
import ornl.elision.matcher.Outcome
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Match
import ornl.elision.util.Loc

/**
 * Companion object to match variables.
 */
object Variable {
  
  /**
   * Extract the parts of a variable.
   * 
   * @param vx  The variable.
   * @return  The type, name, guard, labels, and by-name status.
   */
  def unapply(vx: Variable) = Some((vx.theType, vx.name, vx.guard,
      vx.labels, vx.byName))
}

/**
 * Common base class for both term- and meta-variables.
 * 
 * @param loc     Location of the atom's declaration.
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard		The variable's guard.  Default is true.
 * @param labels	Labels for this variable.  Default is none.
 * @param byName  If true, this is a "by name" variable.  Default is false.
 */
abstract class Variable(
    loc: Loc,
    typ: BasicAtom,
    val name: String,
    val guard: BasicAtom = Literal.TRUE,
    val labels: Set[String] = Set[String](),
    val byName: Boolean = false) extends BasicAtom(loc) {
  
  /** The prefix for this variable. */
  val prefix: String

  /** The variable's type must be given. */
  val theType = typ
  
  /** The de Bruijn index is zero, unless overridden. */
  val deBruijnIndex = 0
  
  /** Variables are not constant. */
  val isConstant = false
  
  /** The depth is zero, regardless of the type. */
  val depth = 0
  
  /** By default, variables can be bound. */
  override val isBindable = true
  
  /**
   * Generate a term variable with the same properties as this variable,
   * including type, guards, and labels.  If this is already a term variable,
   * then it is simply returned as-is.
   * 
   * @return  A term variable.
   */
  def asTermVariable: TermVariable
  
  /**
   * Generate a metavariable with the same properties as this variable,
   * including type, guards, and labels.  If this is already a metavariable,
   * then it is simply returned as-is.
   * 
   * @return  A metavariable.
   */
  def asMetaVariable: MetaVariable
  
  override def equals(varx: Any) = varx match {
    case ovar:Variable =>
      feq(ovar, this,
          ovar.theType == theType &&
          ovar.name == name &&
          ovar.guard == guard &&
          ovar.labels == labels &&
          ovar.isTerm == isTerm)
    		
    case _ =>
      false
  }
}

/**
 * Companion object to match metavariables.
 */
object TermVariable {
  
  /**
   * Extract the parts of a term variable.
   * 
   * @param vx  The variable.
   * @return  The type, name, guard, labels, and by-name status.
   */
  def unapply(vx: TermVariable) = Some((vx.theType, vx.name, vx.guard,
      vx.labels, vx.byName))
}

/**
 * Represent a variable.
 * 
 * == Structure and Syntax ==
 * A term variable is indicated with a leading dollar sign (`$`) followed by a
 * valid symbol.  So the following are valid variables:
 * - `$``x`
 * - `$``Fred51_2`
 * - <code>$`1`</code>
 * 
 * It is also possible to construct a "by name" variable.  This is a variable
 * that has an implicit guard (in addition to any other guards it may have)
 * that restricts the variable to only matching itself.  That is, the by-name
 * variable FOO matches only the variable FOO, and not anything else.  This
 * is denoted by enclosing the variable name in quotation marks.
 * - `$``"``FOO``"`
 * By-name variables otherwise behave as synonyms for the variable itself, so
 * if you directly bind `$``"FOO"` to 17, you have actually bound `$``FOO`
 * to 17.
 * 
 * == Guards ==
 * A variable is allowed to have a guard.  The guard is substituted before the
 * variable is bound, and must evaluate to `true` to allow the binding to take
 * place.
 * 
 * In fact, the variable "guard" is more.  The guard is specified as an atom
 * in curly braces after the variable name and before any type information.
 * For proposed binding of variable `$``x` to value `v`, with guard `g`, we do
 * the following.
 * 
 * - If `g` is a [[ornl.elision.core.Rewritable]], then `g.a` is computed and
 *   if the flag is true, `$``x` is bound to the resulting atom.
 * - If `g` is a [[ornl.elision.core.Applicable]], then `g.a` is computed and
 *   `$``x` is bound the result.
 * - Otherwise `g` is assumed to be a predicate, and is rewritten with the
 *   potential bindings.  If the result is true, then `$``x` is bound to `v`.
 * 
 * In all other cases the binding attempt is rejected.
 * 
 * See the previous section for information on the implicit guard created for
 * a "by name" variable.
 * 
 * == Type ==
 * Every variable must have a type, and the type can be `ANY`.
 * 
 * == Equality and Matching ==
 * Variables are equal iff their name, type, guard, and labels are all equal.
 * 
 * Variables can be bound, so that gets checked during matching.  A variable
 * pattern matches a subject iff it is already bound to that subject, or if it
 * is unbound and the types match.  Guards are not matched; this would be
 * problematic, as the guards are used to determine whether a match succeeds!
 * Labels are also not matched, as they serve a different semantic purpose.
 * 
 * @param loc     Location of the atom's declaration.
 * @param typ     The variable type.
 * @param name    The variable name.
 * @param guard   The variable's guard.  Default is true.
 * @param labels  Labels for this variable.  Default is none.
 * @param byName  If true, this is a "by name" variable.  Default is false.
 */
class TermVariable protected[elision] (
    loc: Loc,
    typ: BasicAtom,
    name: String,
    guard: BasicAtom = Literal.TRUE,
    labels: Set[String] = Set[String](),
    byName: Boolean = false)
    extends Variable(loc, typ, name, guard, labels) {
  
  // This variable is a term.
  override val isTerm = true
  
  // Use the term variable prefix.
  override val prefix = "$"
    
  // Hash codes are based on the type and name.
  override lazy val hashCode = typ.hashCode * 31 + name.hashCode
  override lazy val otherHashCode = typ.otherHashCode +
    8191*(name.toString).foldLeft(BigInt(0))(other_hashify)+1
    
  /**
   * Make a non-meta version of this metavariable.
   * @return  The new variable.
   */
  override def asVariable = this
  
  /**
   * Make a meta version of this metavariable.  I.e., do nothing.
   * @return  This metavariable.
   */
  override def asMetaVariable =
    new MetaVariable(loc, typ, name, guard, labels, byName)
}

/**
 * Companion object to match metavariables.
 */
object MetaVariable {
  
  /**
   * Extract the parts of a metavariable.
   * 
   * @param vx  The variable.
   * @return  The type, name, guard, labels, and by-name status.
   */
  def unapply(vx: MetaVariable) = Some((vx.theType, vx.name, vx.guard,
      vx.labels, vx.byName))
}

/**
 * Define a metavariable.
 * 
 * == Purpose ==
 * A metavariable is just like an ordinary variable, with the exception that
 * any metavariables in an atom make that atom a meta atom, and meta atoms
 * block evaluation in an apply.
 * 
 * For example, consider `is_bindable(``$``x)`.  This will immediately
 * evaluate to `true` since `$``x` is bindable.  If we wanted to use this
 * as a guard for a variable, however, this won't work.  Instead we write
 * `is_bindable($``$``x)` using the metavariable, and evaluation is
 * deferred until the atom is rewritten.
 * 
 * @param loc     Location of the atom's declaration.
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard   The variable's guard.  Default is true.
 * @param labels  Labels for this variable.  Default is none.
 * @param byName  If true, this is a "by name" variable.  Default is false.
 */
class MetaVariable protected[elision] (
    loc: Loc,
    typ: BasicAtom,
    name: String,
    guard: BasicAtom = Literal.TRUE,
    labels: Set[String] = Set[String](),
    byName: Boolean = false)
    extends Variable(loc, typ, name, guard, labels) {
  
  // This variable is not a term.
  override val isTerm = false
  
  // Use the metavariable prefix.
  override val prefix = "$$"
    
  // Hash codes are based on the type and name.
  override lazy val hashCode = typ.hashCode * 37 + name.hashCode
  override lazy val otherHashCode = typ.otherHashCode +
    8193*(name.toString).foldLeft(BigInt(0))(other_hashify)+1
    
  /**
   * Make a non-meta version of this metavariable.
   * @return  The new variable.
   */
  override def asVariable =
    new TermVariable(loc, typ, name, guard, labels, byName)
  
  /**
   * Make a meta version of this metavariable.  I.e., do nothing.
   * @return  This metavariable.
   */
  override def asMetaVariable = this
}
