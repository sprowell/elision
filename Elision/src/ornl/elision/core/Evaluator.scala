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
package ornl.elision.core

import ornl.elision.util.Loc

/**
 * An evaluator is responsible for evaluating atoms just prior to their
 * construction.  This avoids creating an atom and then immediately
 * discarding it because of its immediate evaluation.  To use this extend
 * it and override any methods that need evaluation.
 */
abstract class Evaluator {

  /**
   * Make a new algebraic property specification.
   * 
   * @param loc           Location of this specification.
   * @param associative   Optional associativity.  Default is none.
   * @param commutative   Optional commutativity.  Default is none.
   * @param idempotent    Optional idempotency.  Default is none.
   * @param absorber      Optional absorber.  Default is none.
   * @param identity      Optional identity.  Default is none.
   * @return  The new algebraic properties specification.
   */
  def newAlgProp(loc: Loc, associative: Option[BasicAtom],
      commutative: Option[BasicAtom], idempotent: Option[BasicAtom],
      absorber: Option[BasicAtom], identity: Option[BasicAtom]) = {
    new AlgProp(loc, associative, commutative, idempotent, absorber, identity)
  }
  
  /**
   * Apply one atom to another.
   * 
   * @param loc           Location of this specification.
   * @param operator      The operator.
   * @param argument      The argument.
   * @return  The result.
   */
  def newApply(loc: Loc, operator: BasicAtom, argument: BasicAtom) = {
    
  }
  
  /**
   * Make a new atom collection.
   * 
   * @param loc           Location of this specification.
   * @param properties    The algebraic properties of the collection.
   * @param atoms         The atoms in the collection.
   * @return  The new collection.
   */
  def newAtomSeq(loc: Loc, properties: AlgProp, atoms: Seq[BasicAtom]) = {
    
  }
  
  /**
   * Make a new lambda.
   * 
   * @param loc           Location of this specification.
   * @param parameter     The lambda parameter.
   * @param body          The lambda body.
   * @return  The new lambda.
   */
  def newLambda(loc: Loc, parameter: Variable, body: BasicAtom) = {
    
  }
  
  /**
   * Make a new symbol literal.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: Symbol) = {
    
  }
  
  /**
   * Make a new string literal.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: String) = {
    
  }
  
  /**
   * Make a new integer literal.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: BigInt) = {
    
  }
  
  /**
   * Make a new bit string literal.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param value         The value.
   * @param length        The width of the bit field.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: BigInt, length: Int) = {
    
  }
  
  /**
   * Make a new symbol literal.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param significand   The significand.
   * @param exponent      The exponent.
   * @param radix         The preferred radix.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, significand: BigInt, exponent: Int,
      radix: Int) = {
    
  }
  
  /**
   * Make a new map pair.
   * 
   * @param loc           Location of this specification.
   * @param pattern       The pattern to match.
   * @param rewrite       The rewrite atom.
   * @return  The new map pair.
   */
  def newMapPair(loc: Loc, pattern: BasicAtom, rewrite: BasicAtom) = {
    
  }
  
  /**
   * Make a new match atom.
   * 
   * @param loc           Location of this specification.
   * @param pattern       The pattern to match.
   * @return  The new match atom.
   */
  def newMatchAtom(loc: Loc, pattern: BasicAtom) = {
    
  }
  
  /**
   * Make a new special form.
   * 
   * @param loc           Location of this specification.
   * @param tag           The special form tag.
   * @param content       The content.
   * @return  The new special form.
   */
  def newSpecialForm(loc: Loc, tag: BasicAtom, content: BasicAtom) = {
    
  }
  
  /**
   * Make a new term variable.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param name          The name of the variable.
   * @param guard         The variable guard.  True by default.
   * @param labels        The labels.  None by default.
   * @param byname        If true, this is a "by name" variable.  The default
   *                      is false.
   * @return  The new term variable.
   */
  def newTermVariable(loc: Loc, typ: BasicAtom, name: String,
      guard: BasicAtom = Literal.TRUE, labels: Set[String] = Set(),
      byname: Boolean = false) = {
    
  }
  
  /**
   * Make a new meta variable.
   * 
   * @param loc           Location of this specification.
   * @param typ           The type.
   * @param name          The name of the variable.
   * @param guard         The variable guard.  True by default.
   * @param labels        The labels.  None by default.
   * @param byname        If true, this is a "by name" variable.  The default
   *                      is false.
   * @return  The new meta variable.
   */
  def newMetaVariable(loc: Loc, typ: BasicAtom, name: String,
      guard: BasicAtom = Literal.TRUE, labels: Set[String] = Set(),
      byname: Boolean = false) = {
    
  }
}
