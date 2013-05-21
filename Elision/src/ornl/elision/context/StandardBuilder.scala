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

import ornl.elision.core.ANY
import ornl.elision.core.AtomSeq
import ornl.elision.core.BINDING
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.BindingsAtom
import ornl.elision.core.BooleanLiteral
import ornl.elision.core.Literal
import ornl.elision.core.MapPair
import ornl.elision.core.MatchAtom
import ornl.elision.core.RewriteRule
import ornl.elision.core.SpecialForm
import ornl.elision.core.SpecialFormException
import ornl.elision.core.StringLiteral
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.toESymbol
import ornl.elision.util.Loc
import ornl.elision.core.GuardStrategy

object StandardBuilderComponents {
  
  /**
   * Define a mapping from special form tag to the structure mapping for the
   * special form.  See [[ornl.elision.core.SpecialForm]]`.check`. 
   */
  val structures = Map {
    'operator -> Map {
      "name" -> true
      "type" -> false
      "params" -> false
      "cases" -> false
      "detail" -> false
      "description" -> false
      "handler" -> false
      "evenmeta" -> false
    }
    'rule -> Map {
      "name" -> false
      "" -> true
      "detail" -> false
      "description" -> false
      "rulesets" -> false
      "ruleset" -> false
      "if" -> false
    }
    'match -> Map {
      "" -> true
      "if" -> false
    }
  }
  
  /**
   * Get a component from a special form binding.
   * 
   * If the default is `None` then the requested value must be present or an
   * exception is thrown.  If the value is the wrong type, an exception is
   * thrown.  Otherwise it is correctly cast and returned.
   * 
   * @param loc     The location of the atom.
   * @param tag     The tag.
   * @param binds   The binding from the content.
   * @param key     The requested key.
   * @param default An optional default value.
   * @return  The requested value.
   */
  private def getAs[TYPE](
      loc: Loc,
      tag: BasicAtom,
      binds: Bindings,
      key: String,
      default: Option[TYPE])
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    if (!binds.contains(key)) {
      if (default.isDefined) {
        return default.get
      } else {
        throw new SpecialFormException(loc,
            "Special form "+tag.toParseString+
            " requires that a key "+toESymbol(key)+
            " of type "+mTYPE.toString+
            " be given, but the key is missing.")
      }
    }
    val value = binds(key)
    if (mTYPE >:> Manifest.classType(key.getClass)) {
      value.asInstanceOf[TYPE]
    } else {
      throw new SpecialFormException(loc,
          "Special form "+tag.toParseString+
          " requires that key "+toESymbol(key)+
          " be of type "+mTYPE.toString+
          ", but the type "+value.getClass.toString+" was found.")
    }
  }
  
  private val _no_detail =
    Some(new StringLiteral(Loc.internal, "(no detail)"))
  private val _no_description = 
    Some(new StringLiteral(Loc.internal, "(no description)"))
  private val _no_name = 
    Some(new SymbolLiteral(Loc.internal, Symbol("(no name)")))
  private val _false = Some(Literal.FALSE)
  
  /**
   * Build a match atom from a special form.
   * 
   * @param loc       Location of the atom's declaration.
   * @param tag       Tag for the special form, included for consistency.
   * @param content   The content.
   * @param builder   The builder to make atoms.
   * @return  The new match atom.
   */
  def buildMatch(loc: Loc, tag: BasicAtom, content: BasicAtom,
      builder: Builder) = {
    // The content is a binding.
    val binds = content.asInstanceOf[BindingsAtom].mybinds
    val pattern = getAs[BasicAtom](loc, tag, binds, "", None)
    val guards = getAs[AtomSeq](loc, tag, binds, "if", Some(builder.EmptySeq))
    new MatchAtom(loc, content, pattern, guards, builder.MAP(ANY, BINDING))
  }
  
  /**
   * Build a rewrite rule from a special form.
   * 
   * @param loc       Location of the atom's declaration.
   * @param tag       Tag for the special form, included for consistency.
   * @param content   The content.
   * @param builder   The builder to make atoms.
   * @param strategy  The strategy to rewrite rule guards.
   * @return  The new rule.
   */
  def buildRule(loc: Loc, tag: BasicAtom, content: BasicAtom,
      builder: Builder, strategy: GuardStrategy) = {
    // The content is a binding.
    val binds = content.asInstanceOf[BindingsAtom].mybinds
    val name = if (binds.contains("name")) {
      Some(getAs[SymbolLiteral](loc, tag, binds, "name", None).value.name)
    } else {
      None
    }
    val description = getAs[StringLiteral](loc, tag, binds, "description",
        _no_description).value
    val detail = getAs[StringLiteral](loc, tag, binds, "detail",
        _no_detail).value
    val map = getAs[MapPair](loc, tag, binds, "", None)
    val rulesetseq = getAs[AtomSeq](loc, tag, binds, "rulesets",
        Some(builder.EmptySeq))
    val rulesets = rulesetseq map {
      item => item match {
        case SymbolLiteral(_, _, value) =>
          value.name
          
        case _ =>
          throw new SpecialFormException(loc,
              "Special form "+tag.toParseString+
              " requires that key rulesets be a list of symbols, but the " +
              "non-symbol atom "+item.toParseString+" was found.")
      }
    } toSet
    val guards = getAs[AtomSeq](loc, tag, binds, "if", Some(builder.EmptySeq))
    new RewriteRule(loc, content, map.left, map.right, guards, rulesets,
        strategy, name, description, detail)
  }
  
  /**
   * Build an operator from a special form.
   * 
   * @param loc       Location of the atom's declaration.
   * @param tag       Tag for the special form, included for consistency.
   * @param content   The content.
   * @param builder   The builder to make atoms.
   * @param strategy  A guard strategy for new rules.
   * @return  The new operator.
   */
  def buildOperator(loc: Loc, tag: BasicAtom, content: BasicAtom,
      builder: Builder, strategy: GuardStrategy) = {
    // The content is a binding.
    val binds = content.asInstanceOf[BindingsAtom].mybinds
    
    // Get those elements common to all operators.
    val name = getAs[SymbolLiteral](loc, tag, binds, "name", None).value.name
    val typ = getAs[BasicAtom](loc, tag, binds, "type", Some(ANY))
    val description = getAs[StringLiteral](loc, tag, binds, "description",
        _no_description).value
    val detail = getAs[StringLiteral](loc, tag, binds, "detail",
        _no_detail).value
    val evenmeta = getAs[BooleanLiteral](loc, tag, binds, "evenmeta",
        _false).value
    
    // Figure out if this is a case operator or a symbolic operator.
    if (binds.contains("params")) {
      // This is a symbolic operator.
      val params = getAs[AtomSeq](loc, tag, binds, "params", None)
      val handlertxt = (if (binds.contains("handler")) {
        Some(getAs[StringLiteral](loc, tag, binds, "handler", None).value)
      } else {
        None
      })
      builder.newTypedSymbolicOperator(loc, strategy, name, typ, params,
          description, detail, evenmeta, handlertxt)
    } else if (binds.contains("cases")) {
      // This is a case operator.
      val cases = getAs[AtomSeq](loc, tag, binds, "cases", None)
      builder.newCaseOperator(loc, strategy, name, typ, cases, description,
          detail, evenmeta)
    } else {
      // This is an error.
      throw new SpecialFormException(loc,
          "Special form "+tag.toParseString+
          " requires that either params or cases be given, but neither" +
          " was found.")
    }
  }
}

/**
 * Build atoms using a default evaluation.
 * 
 * @param appbuilder  The apply builder to use.
 */
class StandardBuilder(appbuilder: ApplyBuilder) extends Evaluator {
  
  /**
   * Apply one atom to another.
   * 
   * @param loc           Location of this specification.
   * @param operator      The operator.
   * @param argument      The argument.
   * @param strategy      A guard strategy to use for new rules.
   * @param bypass        If true, bypass native handler.  Default is false.
   * @return  The result.
   */
  def newApply(loc: Loc, operator: BasicAtom, argument: BasicAtom,
      strategy: GuardStrategy, bypass: Boolean = false): BasicAtom = {
    appbuilder(operator, argument, this, strategy, bypass)
  }
  
  /**
   * Make a new special form.
   * 
   * @param loc           Location of this specification.
   * @param tag           The special form tag.
   * @param content       The content.
   * @param strategy      The strategy to use to rewrite rule guards.
   * @return  The new special form.
   */
  def newSpecialForm(loc: Loc, tag: BasicAtom, content: BasicAtom,
      strategy: GuardStrategy): SpecialForm = {
    // The specific kind of special form made depends on the interpretation
    // from the tag.
    tag match {
      case sym: SymbolLiteral =>
        // The tag is a symbol literal.  If the symbol is known, check the
        // structure.
        StandardBuilderComponents.structures.get(sym.value) match {
          case None =>
            // Nothing special; make a special form and we are done.
            new SpecialForm(loc, tag, content)
            
          case Some(map: Map[String,Boolean]) =>
            SpecialForm.check(tag, content, map)
            sym.value match {
              case 'operator =>
                StandardBuilderComponents.buildOperator(loc, tag, content,
                    this, strategy)
                
              case 'rule =>
                StandardBuilderComponents.buildRule(loc, tag, content, this,
                    strategy)
                
              case 'match =>
                StandardBuilderComponents.buildMatch(loc, tag, content, this)
                
              case _ =>
                new SpecialForm(loc, tag, content)
            }
        }
        
      case _ =>
        new SpecialForm(loc, tag, content)
    }
  }
}
