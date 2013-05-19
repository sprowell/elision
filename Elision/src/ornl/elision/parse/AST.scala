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
package ornl.elision.parse

import ornl.elision.context.Context
import ornl.elision.core.ANY
import ornl.elision.core.Absorber
import ornl.elision.core.AlgProp
import ornl.elision.core.Apply
import ornl.elision.core.Associative
import ornl.elision.core.AtomSeq
import ornl.elision.core.BITSTRING
import ornl.elision.core.BOOLEAN
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.BindingsAtom
import ornl.elision.core.BitStringLiteral
import ornl.elision.core.Commutative
import ornl.elision.core.FLOAT
import ornl.elision.core.FloatLiteral
import ornl.elision.core.INTEGER
import ornl.elision.core.Idempotent
import ornl.elision.core.Identity
import ornl.elision.core.IntegerLiteral
import ornl.elision.core.Lambda
import ornl.elision.core.Literal
import ornl.elision.core.MapPair
import ornl.elision.core.MetaVariable
import ornl.elision.core.NamedRootType
import ornl.elision.core.NoProps
import ornl.elision.core.OperatorRef
import ornl.elision.core.STRING
import ornl.elision.core.SYMBOL
import ornl.elision.core.SpecialForm
import ornl.elision.core.StringLiteral
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.TypeUniverse
import ornl.elision.core.Variable
import ornl.elision.core.boolToLiteral
import ornl.elision.core.wrapBindingsAtom
import ornl.elision.util.Loc
import ornl.elision.context.ApplyBuilder


/**
 * Base class for abstract syntax tree nodes.
 * 
 * @param TYPE  The type of `BasicAtom` held in this abstract syntax tree node.
 * @param loc   Location of the atom's declaration, if known.
 */
abstract class AST[+TYPE <: BasicAtom](val loc: Loc) {
  /**
   * Interpret this AST node to produce an atom of the specified type.
   * @param context   The context providing rulesets and operators.
   * @return  The generated atom.
   */
  def interpret(context: Context): TYPE
}


/**
 * Create abstract syntax tree nodes.
 */
object AST {
  
  /** Quick reference for an abstract syntax tree node holding a `BasicAtom`. */
  type BA = AST[BasicAtom]
  
  /** Marker trait used to indicate a "naked" symbol.  These are special. */
  trait Naked
  
  /**
   * Make a simple AST around a known atom.
   * 
   * @param atom  The atom to store.
   * @return  The AST node.
   */
  def known[KIND <: BasicAtom](atom: KIND) = new AST[KIND](Loc.internal) {
    def interpret(context: Context) = atom
  }
  
  /**
   * Process a symbol whose type is unspecified.  These might be special
   * root type names, or the special literals `true` or `false`.
   * 
   * @param value   Value of the symbol.
   * @param loc     The location where this node arose.
   * @return  The resulting AST.
   */
  def sym(value: String, loc: Loc) = new BA(loc) with Naked {
    def interpret(context: Context) = {
      value match {
        case "true" => true
        case "false" => false
        case _ =>
          val lookup = (if (value == "_") "ANY" else value)
          NamedRootType.get(lookup) match {
            case Some(nrt) => nrt
            case _ => context.builder.newLiteral(loc, SYMBOL, Symbol(value))
          }
      }
    }
  }
  
  /**
   * Quick method to make a symbol AST.
   * 
   * @param value   Value of the symbol.
   * @param typ     The type AST.
   * @param loc     The location where this node arose.
   * @return  The new symbol AST.
   */
  def sym(value: String, typ: BA, loc: Loc) = new BA(loc) {
    def interpret(context: Context) = {
      // Check for Boolean literals here.
      typ.interpret(context) match {
        case BOOLEAN if value == "true" => true
        case BOOLEAN if value == "false" => false
        case t:Any => context.builder.newLiteral(loc, t, Symbol(value))
      }
    }
  }
  
  /**
   * Make a string literal AST.
   * 
   * @param value   Value of the string.
   * @param loc     The location where this node arose.
   * @return  The new string AST.
   */
  def string(value: String, loc: Loc) = new AST[StringLiteral](loc) {
    def interpret(context: Context) =
      context.builder.newLiteral(loc, STRING, value)
  }
  
  /**
   * Make a string literal AST.
   * 
   * @param value   Value of the string.
   * @param typ     The type AST.
   * @param loc     The location where this node arose.
   * @return  The new string AST.
   */
  def string(value: String, typ: BA, loc: Loc) = new AST[StringLiteral](loc) {
    def interpret(context: Context) =
      context.builder.newLiteral(loc, typ.interpret(context), value)
  }
  
  /**
   * Make an AST for a bit string.
   * 
   * @param flag    Optional flag indicating if the original bits is negative.
   * @param bits    An integer to interpret as the bits.
   * @param len     The length.
   * @param otyp    The overriding type for the bit string.
   * @param loc     The location where this node arose.
   * @return  The constructed literal AST node.
   */
  def bitstring(oflag: Option[Boolean], bits: (Int, String), len: (Int, String),
      otyp: Option[BA], loc: Loc) = new AST[Literal[_]](loc) {
    def interpret(context: Context) = {
      val neg = oflag.getOrElse(false)
      val typ = otyp.getOrElse(known(BITSTRING)).interpret(context)
      context.builder.newLiteral(loc, typ,
          if (neg) -BigInt(bits._2, bits._1) else BigInt(bits._2, bits._1),
          Integer.parseInt(len._2, len._1))
    }
  }
  
  /**
   * Make an AST for a number.
   * 
   * @param oflag   Optional flag indicating if the number is negative.
   * @param whole   The whole part of the number, as radix / digits.
   * @param frac    The fractional part of the number, as radix / digits.
   * @param exp     The exponent of the number, as negative flag / radix / digits.
   * @param otyp    The overriding type for the number.
   * @param loc     The location where this node arose.
   * @return  The constructed literal, either an integer or a float literal.
   */
  def number(oflag: Option[Boolean],
      whole: (Int, String),
      frac: Option[(Int, String)],
      exp: Option[(Boolean, Int, String)],
      otyp: Option[BA],
      loc: Loc) = new AST[Literal[_]](loc) {
    def interpret(context: Context) = {
      // Get flag.
      val neg = oflag.getOrElse(false)
      // If either a fractional part or an exponent is specified, interpret
      // this as a float.  Otherwise interpret this as an integer.
      if (frac.isEmpty && exp.isEmpty) {
        // Interpret this as an integer.
        val typ = otyp.getOrElse(known(INTEGER)).interpret(context)
        context.builder.newLiteral(loc, typ,
            if (neg) -BigInt(whole._2, whole._1)
            else BigInt(whole._2, whole._1))
      } else {
        // Interpret this as a float.  Pull out the pieces.
        val integer = whole._2
        val fraction = frac match {
          case None => ""
          case Some((base, digits)) => digits
        }
        var exponent = exp match {
          case None => 0
          case Some((neg, base, digits)) =>
            if (neg) -Integer.parseInt(digits, base)
            else Integer.parseInt(digits, base)
        }
        
        // We need to modify the integer and fraction parts to create the
        // proper significand.  This is done as follows.  If there are n digits
        // in the fraction, then we need to subtract n from the exponent.  We
        // converted the exponent into an integer above.
        
        // Correct the significand by adding the integer and fractional part
        // together.  This looks odd, but remember that they are still
        // strings.
        val significand = integer + fraction
        
        // Now adjust the exponent to account for the fractional part.  Since
        // the decimal moves right, we subtract from the original exponent.
        exponent -= fraction.length
        
        // Now interpret this as floating point literal.
        val typ = otyp.getOrElse(known(FLOAT)).interpret(context)
        context.builder.newLiteral(loc, typ,
            if (neg) -BigInt(significand, whole._1)
            else BigInt(significand, whole._1), exponent, whole._1)
      }
    }
  }
  
  /**
   * Quick access to the type universe AST.
   */
  val typeuniverse = known(TypeUniverse)
  
  /**
   * Quick access to the ANY AST.
   */
  val any = known(ANY)
  
  /**
   * Make an AST for a map pair.
   * 
   * @param left    Left AST of the map pair (the pattern).
   * @param right   Right AST of the map pair (the rewrite).
   * @param loc     The location where this node arose.
   * @return  The new map pair AST.
   */
  def mappair(left: BA, right: BA, loc: Loc) = new AST[MapPair](loc) {
    def interpret(context: Context) =
      context.builder.newMapPair(loc, left.interpret(context),
          right.interpret(context))
  }
  
  /**
   * Make an AST node for applying one atom to another.
   * 
   * @param left    AST to left of applicative dot (the operator).
   * @param right   AST to right of applicative dot (the argument).
   * @param loc     The location where this node arose.
   * @return  AST for the application.
   */
  def apply(left: BA, right: BA, loc: Loc) = new BA(loc) {
    def interpret(context: Context) = {
      // If the left element is a naked symbol, then try to interpret it as
      // an operator.  Otherwise just interpret it.
      val op = left.interpret(context) match {
        case SymbolLiteral(_, _, value) if left.isInstanceOf[Naked] =>
          context.operatorLibrary(value.name)
          
        case value: Any => value
      }
      context.builder.newApply(loc, op, right.interpret(context))
    }
  }
  
  /**
   * Make an absorber property AST.
   * 
   * @param atom    AST for the absorber.
   * @return  The new AST.
   */
  def absorber(atom: BA) = new AST[AlgProp](atom.loc) {
    def interpret(context: Context) = Absorber(atom.interpret(context))
  }
  
  /**
   * Make an identity property AST.
   * 
   * @param atom    AST for the identity.
   * @return  The new AST.
   */
  def identity(atom: BA) = new AST[AlgProp](atom.loc) {
    def interpret(context: Context) = Identity(atom.interpret(context))
  }
  
  /**
   * Make an associative property AST.
   * 
   * @param atom    AST for the associative condition.
   * @return  The new AST.
   */
  def associative(atom: BA) = new AST[AlgProp](atom.loc) {
    def interpret(context: Context) = Associative(atom.interpret(context))
  }
  
  /**
   * Make a commutative property AST.
   * 
   * @param atom    AST for the commutative condition.
   * @return  The new AST.
   */
  def commutative(atom: BA) = new AST[AlgProp](atom.loc) {
    def interpret(context: Context) = Commutative(atom.interpret(context))
  }
  
  /**
   * Make an idempotent property AST.
   * 
   * @param atom    AST for the idempotent condition.
   * @return  The new AST.
   */
  def idempotent(atom: BA) = new AST[AlgProp](atom.loc) {
    def interpret(context: Context) = Idempotent(atom.interpret(context))
  }
  
  /**
   * Make a variable AST.  This includes metavariables and "by name" variables.
   * 
   * @param meta    True iff this should be a metavariable.
   * @param name    The variable name.
   * @param byname  True iff this is a by-name variable.
   * @param guard   The guard AST.
   * @param typ     The type AST.
   * @param tags    A list of tags for the variable.
   * @param loc     The location where this node arose.
   * @return  The new variable AST.
   */
  def variable(meta: Boolean, name: String, byname: Boolean, guard: Option[BA],
      typ: Option[BA], tags: List[String], loc: Loc) = new AST[Variable](loc) {
    def interpret(context: Context) = if (meta) {
      context.builder.newMetaVariable(loc,
          typ.getOrElse(any).interpret(context), name,
          guard.getOrElse(known(true: BasicAtom)).interpret(context),
          tags.toSet, byname)
    } else {
      context.builder.newTermVariable(loc,
          typ.getOrElse(any).interpret(context), name,
          guard.getOrElse(known(true: BasicAtom)).interpret(context),
          tags.toSet, byname)
    }
  }
  
  /**
   * Make a lambda AST.
   * 
   * @param param   The parameter AST.
   * @param body    The body AST.
   * @param loc     The location where this node arose.
   * @return  The new lambda AST.
   */
  def lambda(param: AST[Variable], body: BA, loc: Loc) = new AST[Lambda](loc) {
    def interpret(context: Context) =
      context.builder.newLambda(loc, param.interpret(context),
          body.interpret(context))
  }
  
  /**
   * Make a special form AST.
   * 
   * @param tag     The tag AST.
   * @param content The content AST.
   * @param loc     The location where this node arose.
   * @return  The new special form AST.
   */
  def special(tag: BA, content: BA, loc: Loc) = new BA(loc) {
    def interpret(context: Context) =
      context.builder.newSpecialForm(loc, tag.interpret(context),
          content.interpret(context))
  }
  
  /**
   * Combine algorithmic properties into a single AST.
   * 
   * @param props   A list of algorithmic property AST's.
   * @return  The new algorithmic property AST.
   */
  def algprop(props: List[AST[AlgProp]]) = new AST[AlgProp](Loc.internal) {
    def interpret(context: Context) = props.foldLeft(NoProps: AlgProp) {
      (sofar, next) => sofar and next.interpret(context)
    }
  }
  
  /** Quick reference to the no-properties AST. */
  val noprops = algprop(List())
  
  /**
   * Make an atom sequence AST.
   * 
   * @param props   The algorithmic properties AST.
   * @param atoms   The sequence of atom AST's.
   * @param loc     The location where this node arose.
   * @return  The new sequence AST.
   */
  def atomseq(props: AST[AlgProp], atoms: List[BA],
      loc: Loc) = new AST[AtomSeq](loc) {
    def interpret(context: Context) =
      context.builder.newAtomSeq(loc, props.interpret(context),
          atoms.map(_.interpret(context)).toIndexedSeq)
  }
  
  /**
   * Construct a bindings AST.
   * 
   * @param pairs   A list of pairs consisting of name (string) and value AST.
   * @return  The new bindings AST.
   */
  def binding(pairs: List[(String, BA)]) = new AST[BindingsAtom](Loc.internal) {
    def interpret(context: Context) = Bindings(pairs.map {
      (pair) => (pair._1, pair._2.interpret(context))
    }:_*)
  }
  
  /**
   * Construct an operator reference AST.
   * 
   * @param name    The name of the operator.
   * @return  The new AST.
   */
  def opref(name: String) = new AST[OperatorRef](Loc.internal) {
    def interpret(context: Context) = context.operatorLibrary(name)
  }
}
