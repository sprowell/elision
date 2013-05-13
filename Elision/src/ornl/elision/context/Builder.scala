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

import ornl.elision.util.Loc
import ornl.elision.core.BasicAtom
import ornl.elision.core.AlgProp
import ornl.elision.core.Literal
import ornl.elision.core.AtomSeq
import ornl.elision.core.Lambda
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.StringLiteral
import ornl.elision.core.IntegerLiteral
import ornl.elision.core.BitStringLiteral
import ornl.elision.core.FloatLiteral
import ornl.elision.core.MapPair
import ornl.elision.core.MatchAtom
import ornl.elision.core.SpecialForm
import ornl.elision.core.Variable
import ornl.elision.core.TermVariable
import ornl.elision.core.MetaVariable
import ornl.elision.core.Bindings
import ornl.elision.core.BindingsAtom
import ornl.elision.core.OperatorRef
import ornl.elision.core.Operator
import ornl.elision.core.TypedSymbolicOperator
import ornl.elision.core.CaseOperator
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.TypeUniverse
import ornl.elision.core.ANY
import ornl.elision.core.NoProps
import ornl.elision.core.Associative
import ornl.elision.core.NONE
import ornl.elision.core.RewriteRule
import ornl.elision.core.STRING
import ornl.elision.core.BOOLEAN
import ornl.elision.core.SYMBOL
import ornl.elision.core.BooleanLiteral

/**
 * Construct atoms, first applying any evaluation logic.
 */
abstract class Builder {
  
  /**
   * Rewrite the provided atom by applying the replacements defined in the
   * map.  The atom itself may be in the map, in which case its replacement
   * is returned.  This is a "one pass" replacement.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def replace(atom: BasicAtom,
      map: Map[BasicAtom, BasicAtom]): (BasicAtom, Boolean)
      
  /**
   * Rewrite the provided atom, replacing instances of variables bound (by
   * name) in the bindings with the bound values.
   * 
   * @param atom    The atom to rewrite.
   * @param binds   The bindings.
   * @return  A pair consisting of the result atom and a flag that is true
   *          iff any rewriting was performed.
   */
  def rewrite(atom: BasicAtom, binds: Bindings): (BasicAtom, Boolean)

  /**
   * Make a new algebraic property specification.
   * 
   * @param loc           Location of this atom's declaration.
   * @param associative   Optional associativity.  Default is none.
   * @param commutative   Optional commutativity.  Default is none.
   * @param idempotent    Optional idempotency.  Default is none.
   * @param absorber      Optional absorber.  Default is none.
   * @param identity      Optional identity.  Default is none.
   * @return  The new algebraic properties specification.
   */
  def newAlgProp(loc: Loc, associative: Option[BasicAtom],
      commutative: Option[BasicAtom], idempotent: Option[BasicAtom],
      absorber: Option[BasicAtom], identity: Option[BasicAtom]): AlgProp = {
    new AlgProp(loc, associative, commutative, idempotent, absorber, identity)
  }
  
  /**
   * Apply one atom to another.
   * 
   * @param loc           Location of this atom's declaration.
   * @param operator      The operator.
   * @param argument      The argument.
   * @param bypass        Whether to bypass native handlers.  False by default.
   * @return  The result.
   */
  def newApply(loc: Loc, operator: BasicAtom, argument: BasicAtom,
      bypass: Boolean = false): BasicAtom
  
  /**
   * Make a new atom collection.
   * 
   * @param loc           Location of this atom's declaration.
   * @param properties    The algebraic properties of the collection.
   * @param atoms         The atoms in the collection.
   * @return  The new collection.
   */
  def newAtomSeq(loc: Loc, properties: AlgProp,
      atoms: IndexedSeq[BasicAtom]): AtomSeq = {
    val theType = {
      if (atoms.length == 0) LIST(ANY) else {
        val aType = atoms(0).theType
        if (atoms.forall(aType == _.theType)) LIST(aType) else LIST(ANY)
      }
    }
    new AtomSeq(loc, theType, properties, atoms)
  }
  
  /**
   * Make a new atom collection.
   * 
   * @param loc           Location of this atom's declaration.
   * @param properties    The algebraic properties of the collection.
   * @param atoms         The atoms in the collection.
   * @return  The new collection.
   */
  def newAtomSeq(loc: Loc, properties: AlgProp,
      atoms: BasicAtom*): AtomSeq = {
    val theType = {
      if (atoms.length == 0) LIST(ANY) else {
        val aType = atoms(0).theType
        if (atoms.forall(aType == _.theType)) LIST(aType) else LIST(ANY)
      }
    }
    new AtomSeq(loc, theType, properties, atoms.toIndexedSeq)
  }
  
  /**
   * Make a new bindings atom.
   * 
   * @param loc           Location of this atom's declaration.
   * @param binds         The bindings.
   * @return  The new bindings atom.
   */
  def newBindingsAtom(loc: Loc, binds: Bindings): BindingsAtom = {
    new BindingsAtom(loc, binds)
  }
  
  /**
   * Make a new bindings atom.
   * 
   * @param loc           Location of this atom's declaration.
   * @param binds         The bindings.
   * @return  The new bindings atom.
   */
  def newBindingsAtom(loc: Loc, binds: Map[String, BasicAtom]): BindingsAtom = {
    new BindingsAtom(loc, binds)
  }
  
  /**
   * Make a new lambda.
   * 
   * @param loc           Location of this atom's declaration.
   * @param parameter     The lambda parameter.
   * @param body          The lambda body.
   * @return  The new lambda.
   */
  def newLambda(loc: Loc, parameter: Variable, body: BasicAtom): Lambda = {
    val (lvar, lbody) = adjust(parameter, body)
    new Lambda(loc, lvar, lbody, MAP(lvar.theType, lbody.theType)) {
      def apply(arg: BasicAtom) = newApply(Loc.internal, this, arg)
    }
  }
  
  /**
   * Make a new Boolean literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: Boolean): BooleanLiteral = {
    new BooleanLiteral(loc, typ, value)
  }
  
  /**
   * Make a new symbol literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: Symbol): SymbolLiteral = {
    new SymbolLiteral(loc, typ, value)
  }
  
  /**
   * Make a new string literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: String): StringLiteral = {
    new StringLiteral(loc, typ, value)
  }
  
  /**
   * Make a new integer literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param value         The value.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: BigInt): IntegerLiteral = {
    new IntegerLiteral(loc, typ, value)
  }
  
  /**
   * Make a new bit string literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param value         The value.
   * @param length        The width of the bit field.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, value: BigInt,
      length: Int): BitStringLiteral = {
    new BitStringLiteral(loc, typ, value, length)
  }
  
  /**
   * Make a new symbol literal.
   * 
   * @param loc           Location of this atom's declaration.
   * @param typ           The type.
   * @param significand   The significand.
   * @param exponent      The exponent.
   * @param radix         The preferred radix.
   * @return  The new literal.
   */
  def newLiteral(loc: Loc, typ: BasicAtom, significand: BigInt, exponent: Int,
      radix: Int): FloatLiteral = {
    new FloatLiteral(loc, typ, significand, exponent, radix)
  }
  
  /**
   * Make a new map pair.
   * 
   * @param loc           Location of this atom's declaration.
   * @param pattern       The pattern to match.
   * @param rewrite       The rewrite atom.
   * @return  The new map pair.
   */
  def newMapPair(loc: Loc, pattern: BasicAtom, rewrite: BasicAtom): MapPair = {
    new MapPair(loc, pattern, rewrite)
  }
  
  /**
   * Make a new rewrite rule.  Provide the special form content.
   * 
   * @param loc         Location of the atom's declaration.
   * @param content     The content of the special form, lazily evaluated.
   * @param pattern     The pattern to match.
   * @param rewrite     The rewrite to apply on match.
   * @param guards      Guards that must be true to accept a match.
   * @param rulesets    The rulesets that contain this rule.
   * @param name        Optional rule name.
   * @param description Optional rule description.
   * @param detail      Optional detailed rule description.
   * @param synthetic   If true, this is a synthetic rule.
   * @return  The new rewrite rule.
   */
  def newRewriteRule(
      loc: Loc,
      content: => BasicAtom,
      pattern: BasicAtom,
      rewrite: BasicAtom,
      guards: Seq[BasicAtom],
      rulesets: Set[String],
      name: Option[String] = None,
      description: String = "",
      detail: String = "",
      synthetic: Boolean = false) = {
    new RewriteRule(loc, content, pattern, rewrite, guards, rulesets, name,
        description, detail, synthetic)
  }
  
  
  /**
   * Make a new rewrite rule.  Provide the special form content.
   * 
   * @param loc         Location of the atom's declaration.
   * @param pattern     The pattern to match.
   * @param rewrite     The rewrite to apply on match.
   * @param guards      Guards that must be true to accept a match.
   * @param rulesets    The rulesets that contain this rule.
   * @param name        Optional rule name.
   * @param description Optional rule description.
   * @param detail      Optional detailed rule description.
   * @param synthetic   If true, this is a synthetic rule.
   * @return  The new rewrite rule.
   */
  def newRewriteRule(
      loc: Loc,
      pattern: BasicAtom,
      rewrite: BasicAtom,
      guards: Seq[BasicAtom],
      rulesets: Set[String],
      name: Option[String] = None,
      description: String = "",
      detail: String = "",
      synthetic: Boolean = false) = {
    var content = Bindings {
      "" -> newAtomSeq(Loc.internal, NoProps,
          newMapPair(Loc.internal, pattern, rewrite))
      "guards" -> newAtomSeq(Loc.internal, NoProps, guards.toIndexedSeq)
      "description" -> newLiteral(Loc.internal, STRING, description)
      "detail" -> newLiteral(Loc.internal, STRING, detail)
    }
    if (name.isDefined) {
      content += ("name" -> newLiteral(Loc.internal, SYMBOL, Symbol(name.get)))
    }
    new RewriteRule(loc, content,
        pattern, rewrite, guards, rulesets, name,
        description, detail, synthetic)
  }

  /**
   * Make a new symbolic operator.  Provide special form content.
   * 
   * @param loc           The location.
   * @param content       The content.  This is lazily evaluated.
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
  def newTypedSymbolicOperator(
      loc: Loc,
      content: => BasicAtom,
      name: String,
      typ: BasicAtom,
      params: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean,
      handlertxt: Option[String]): TypedSymbolicOperator = {
    new TypedSymbolicOperator(loc, content, name, typ,
        _makeOperatorType(params, typ), params, description, detail, evenMeta,
        handlertxt) {
      def apply(args: IndexedSeq[BasicAtom]) =
        newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
    }
  }
  
  /**
   * Make a new case operator.  Provide special form content.
   * 
   * @param loc           The location.
   * @param content       The content.  This is lazily evaluated.
   * @param name          The operator name.
   * @param typ           The operator type.
   * @param cases         The definition.
   * @param description   An optional short description for the operator.
   * @param detail        Optional detailed help for the operator.
   * @param evenMeta      Apply this operator even when the arguments contain
   *                      meta-terms.  This is not advisable, and you should
   *                      probably leave this with the default value of false.
   */
  def newCaseOperator(
      loc: Loc,
      content: => BasicAtom,
      name: String,
      typ: BasicAtom,
      cases: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean): CaseOperator = {
    new CaseOperator(loc, content, name, typ, cases, description, detail,
        evenMeta) {
      def apply(args: IndexedSeq[BasicAtom]) =
        newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
    }
  }

  /**
   * Make a new symbolic operator.
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
  def newTypedSymbolicOperator(
      loc: Loc,
      name: String,
      typ: BasicAtom,
      params: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean,
      handlertxt: Option[String]): TypedSymbolicOperator = {
    new TypedSymbolicOperator(
        loc,
        new BindingsAtom(loc, Bindings {
          "name" -> new SymbolLiteral(Loc.internal, Symbol(name))
          "type" -> typ
          "params" -> params
          "description" -> new StringLiteral(Loc.internal, description)
          "detail" -> new StringLiteral(Loc.internal, detail)
          "evenmeta" -> new BooleanLiteral(Loc.internal, evenMeta)
        }), name, typ, _makeOperatorType(params, typ), params, description,
        detail, evenMeta, handlertxt) {
      def apply(args: IndexedSeq[BasicAtom]) =
        newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
    }
  }
  
  /**
   * Make a new case operator.
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
  def newCaseOperator(
      loc: Loc,
      name: String,
      typ: BasicAtom,
      cases: AtomSeq,
      description: String,
      detail: String,
      evenMeta: Boolean): CaseOperator = {
    new CaseOperator(
        loc,
        Bindings {
          "name" -> new SymbolLiteral(Loc.internal, Symbol(name))
          "type" -> typ
          "cases" -> cases
          "description" -> new StringLiteral(Loc.internal, description)
          "detail" -> new StringLiteral(Loc.internal, detail)
          "evenmeta" -> new BooleanLiteral(Loc.internal, evenMeta)
        }, name, typ, cases, description, detail, evenMeta) {
      def apply(args: IndexedSeq[BasicAtom]) =
        newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
    }
  }
  
  /**
   * Make a new operator reference.
   * 
   * @param loc           Location of this atom's declaration.
   * @param operator      The operator.
   * @return  The new operator reference.
   */
  def newOperatorRef(loc: Loc, operator: Operator): OperatorRef = {
    new OperatorRef(loc, operator) {
      def apply(args: IndexedSeq[BasicAtom]) = operator(args)
    }
  }
  
  /**
   * Make a new special form.
   * 
   * @param loc           Location of this atom's declaration.
   * @param tag           The special form tag.
   * @param content       The content.
   * @return  The new special form.
   */
  def newSpecialForm(loc: Loc, tag: BasicAtom, content: BasicAtom): SpecialForm
  
  /**
   * Make a new term variable.
   * 
   * @param loc           Location of this atom's declaration.
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
      byname: Boolean = false): TermVariable = {
    new TermVariable(loc, typ, name, guard, labels, byname)
  }
  
  /**
   * Make a new meta variable.
   * 
   * @param loc           Location of this atom's declaration.
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
      byname: Boolean = false): MetaVariable = {
    new MetaVariable(loc, typ, name, guard, labels, byname)
  }

  //======================================================================
  // Support Lambda construction.
  //======================================================================
  
  /**
   * Convert a given lambda variable and body into a new variable and body
   * that use de Bruijn indices if those are enabled.  Otherwise return the
   * input variable and body.
   *
   * @param given_lvar  The given lambda parameter.
   * @param given_body  The given lambda body.
   * @return  The pair consisting of the corrected variable and body.
   */
  def adjust(given_lvar: Variable, given_body: BasicAtom) = {
    // Make and return the new lambda.
    if (Lambda.useDeBruijnIndices) {
      // Decide what De Bruijn index to use for this lambda.  We will use one
      // greater than the maximum index of the body.
      val dBI = given_body.deBruijnIndex + 1

      // Classes that implement De Bruijn indices.
      class DBIV(
          typ: BasicAtom,
          val dBI: Int,
          guard: BasicAtom,
          lvar: Set[String])
          extends TermVariable(Loc.internal, typ, ":" + dBI, guard, lvar) {
        override val isDeBruijnIndex = true
        override val deBruijnIndex = dBI
      }
      class DBIM(
          typ: BasicAtom,
          val dBI: Int,
          guard: BasicAtom,
          lvar: Set[String])
          extends MetaVariable(Loc.internal, typ, ":" + dBI, guard, lvar) {
        override val isDeBruijnIndex = true
        override val deBruijnIndex = dBI
      }
      
      // Now make new De Bruijn variables for the index.
      val newvar =
        new DBIV(given_lvar.theType, dBI, given_lvar.guard, given_lvar.labels)
      val newmvar =
        new DBIM(given_lvar.theType, dBI, given_lvar.guard, given_lvar.labels)
      
      // Create a map.
      val map = Map[BasicAtom, BasicAtom](
          given_lvar.asTermVariable -> newvar,
          given_lvar.asMetaVariable -> newmvar)
    
      // Bind the old variable to the new one and rewrite the body.
      val (newbody, notfixed) = replace(given_body, map)
      
      // Return the result.
      if (given_lvar.isTerm) {
        (newvar, newbody)
      } else {
        (newmvar, newbody)
      }
    } else {
      (given_lvar, given_body)
    }
  }
  
  //======================================================================
  // Special fixed values.
  //======================================================================
  
  /** An empty atom sequence with no properties. */
  val EmptySeq = newAtomSeq(Loc.internal, NoProps)

  //======================================================================
  // Support methods for operator construction.
  //======================================================================

  /**
   * Compute an operator type for a typed symbolic operator.
   *
   * @param params  The parameters.
   * @param typ     The type of the fully-applied operator.
   * @return  The type for the operator.
   */
  private def _makeOperatorType(params: AtomSeq, typ: BasicAtom) =
    params.length match {
      case 0 =>
        // For nilary operators the type is NONE -> typ.
        MAP(NONE, typ)
        
      case 1 =>
        // For unary operators the type is domain -> codomain.
        MAP(params(0).theType, typ)
        
      case _ =>
        // For n-ary operators the type is the direct product of the
        // parameter types mapped to the overall type.
        MAP(xx(params.map(_.theType)), typ)
    }

  //======================================================================
  // Define the necessary primitive operators to bootstrap operator typing.
  //======================================================================
  
  /**
   * The well-known MAP operator.  This is needed to define the types of
   * operators, but is not used to define its own type.  The type of the MAP
   * operator is ^TYPE, indicating that it is a root type.  We could, with
   * great justice, use xx (the cross product) for this operator, but don't.
   * This makes the types of operators look more natural when viewed.
   */
  val MAP = new OperatorRef(
      Loc.internal,
      new SymbolicOperator(
          Loc.internal,
          "MAP",
          TypeUniverse,
          newAtomSeq(Loc.internal, NoProps, 'domain, 'codomain),
          "Mapping constructor.",
          "This operator is used to construct types for operators.  It " +
          "indicates a mapping from one type (the domain) to another type " +
          "(the codomain).") {
    def apply(args: IndexedSeq[BasicAtom]) =
      newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
  })
      
  /**
   * The well-known cross product operator.  This is needed to define the
   * types of operators, but is not used to define its own type.  The type
   * of the cross product is ANY.  Note that it must be ANY, since it is
   * associative.
   */
  val xx = new OperatorRef(
      Loc.internal,
      new SymbolicOperator(
          Loc.internal,
          "xx",
          ANY,
          newAtomSeq(Loc.internal, Associative(true), 'x, 'y),
          "Cross product.",
          "This operator is used to construct types for operators.  It " +
          "indicates the cross product of two atoms (typically types).  " +
          "These originate from the types of the parameters of an operator.") {
    def apply(args: IndexedSeq[BasicAtom]) =
      newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
  })
      
  /**
   * The well-known list operator.  This is used to define the type of lists
   * such as the atom sequence.  It has type ^TYPE, indicating that it is a
   * root type.
   */
  val LIST = new OperatorRef(
      Loc.internal,
      new SymbolicOperator(
          Loc.internal,
          "LIST",
          TypeUniverse,
          newAtomSeq(Loc.internal, NoProps, 'type),
          "List type constructor.",
          "This operator is used to indicate the type of a list.  It takes a " +
          "single argument that is the type of the atoms in the list.  For " +
          "heterogeneous lists this will be ANY.") {
    def apply(args: IndexedSeq[BasicAtom]) =
      newApply(Loc.internal, this, newAtomSeq(Loc.internal, NoProps, args))
  })
}