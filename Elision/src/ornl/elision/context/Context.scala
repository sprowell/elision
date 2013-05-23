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
package ornl.elision.context

import scala.collection.mutable.Set
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fickle
import ornl.elision.core.Operator
import ornl.elision.core.OperatorRef
import ornl.elision.core.RewriteRule
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.TypedSymbolicOperator
import ornl.elision.core.Literal
import ornl.elision.core.MapPair
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.toEString
import ornl.elision.core.Variable
import ornl.elision.dialects.ScalaGenerator
import ornl.elision.dialects.ElisionGenerator
import ornl.elision.util.Cache
import ornl.elision.util.PropertyManager
import ornl.elision.util.Version
import ornl.elision.util.Version.build
import ornl.elision.util.Version.major
import ornl.elision.util.Version.minor
import ornl.elision.util.toQuotedString
import ornl.elision.util.Console
import ornl.elision.util.PrintConsole
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.rewrite.RewriteTask
import ornl.elision.rewrite.RewriteEngine
import ornl.elision.core.GuardStrategy
import ornl.elision.core.AtomSeq

/**
 * A requested setting is not present.
 * 
 * @param msg A human-readable message describing the error.
 */
class MissingSettingException(msg: String)
extends ElisionException(Loc.internal, msg)

/**
 * A context provides access to operator libraries and rules, along with
 * the global set of bindings in force at any time.
 * 
 * The context maintains a cache for use during runtime, and also maintains
 * the following items.
 *    
 *  - The __console__ that should get output.
 * 
 * Note that the settings must be specified at construction time.
 *
 * == Use ==
 * In general it is not necessary to make an instance; one is typically
 * provided by a higher-level (semantically) class.
 *
 * This class provides for management of four things:
 *  - A set of [[ornl.elision.core.Bindings]].
 *  - An instance of [[ornl.elision.context.OperatorLibrary]].
 *  - Rulesets.
 *  - "Automatic" rewriting of atoms using rules.
 */
class Context()
extends PropertyManager with Fickle with Mutable with Cache {
  
  override def clone = {
    val clone = new Context()
    clone.setApplyDataBuilder(applyDataBuilder)
    clone.binds = this.binds.clone
    clone.operatorLibrary = this.operatorLibrary.clone
    clone.ruleLibrary = this.ruleLibrary.clone
    clone
  }
  
  //======================================================================
  // Handle native operator application.
  //======================================================================
  
  private var _applyDataBuilder: OperatorApplyHandler.ApplyDataBuilder = _
  
  /**
   * Set the apply data builder for this context. If one is not set, then
   * native handlers cannot execute.
   * 
   * @param abd    A closure to make apply data blocks for native handlers.
   */
  def setApplyDataBuilder(adb: OperatorApplyHandler.ApplyDataBuilder) {
    _applyDataBuilder = adb
  }
  
  /**
   * Get the apply data builder for this context.
   */
  def applyDataBuilder = _applyDataBuilder
  
  //======================================================================
  // Settings that may need to be overridden.
  //======================================================================
  
  /* We take the rewrite engine and make it into a guard strategy here, so that
   * it does not have to "know" about any of the parts of the context.  This
   * helps restrict knowledge of the context (and break dependency chains)
   * in the library.
   */
  
  /**
   * The strategy to use to rewrite guards.
   */
  val guardstrategy: GuardStrategy = new RewriteEngine with GuardStrategy {
    def apply(atom: BasicAtom): (BasicAtom, Boolean) = {
      val task = RewriteTask(atom, ruleLibrary, memo, builder, this)
      RewriteEngine(task)
    }
  }
  
  /* This sequence of initialization builds the pieces so that knowledge of the
   * context (which is necessary for building native handlers) is restricted
   * to the specialized version of the operator apply handler.
   */
  
  /**
   * The operator apply handler.
   */
  private val _ophandler: OperatorApplyHandler =
    new StandardOperatorApplyHandler(this)
  
  /**
   * The builder that makes applications.
   */
  val applybuilder: ApplyBuilder = new ApplyBuilder(_ophandler)
  
  /**
   * The builder to use to make atoms.
   */
  val builder: Builder = new StandardBuilder(applybuilder)

  //======================================================================
  // The console.
  //======================================================================

  /**
   * Get a console native handlers can use.  By default this is a simple
   * console that just prints to standard output.  Replace this with a
   * "real" console if you have one.
   */
  var console: Console = PrintConsole
  
  //======================================================================
  // Build a (transient) memoization cache for this context.
  //======================================================================
  
  /** Memoization cache for this instance. */
  val memo = new Memo(this)

  //======================================================================
  // Global bindings management.
  //======================================================================

  /** The current bindings. */
  private var _binds: Bindings = Bindings()

  /**
   * Bind a variable in this context.
   *
   * @param vname		The variable name to bind.
   * @param atom		The atom to bind to the variable.
   * @return	This context.
   */
  def bind(vname: String, atom: BasicAtom) = {
    _binds += (vname -> atom)
    this
  }

  /**
   * Unbind a variable in this context.
   *
   * @param vname		The variable name.
   * @return	This context.
   */
  def unbind(vname: String) = {
    _binds -= vname
    this
  }

  /**
   * Get the current bindings for this context.
   *
   * @return	The bindings for this context.
   */
  def binds = _binds

  /**
   * Set the bindings to use. Any prior value is lost.
   *
   * @param bindings    The new bindings.
   * @return            This context.
   */
  def binds_=(bindings: Bindings) = {
    require(bindings != null)
    _binds = bindings
    this
  }

  //======================================================================
  // Operator library management.
  //======================================================================

  /** The current operator library. */
  private var _oplib: OperatorLibrary = _
  
  /**
   * Get the current operator library.  If none has explicitly been set, then
   * a default instance is created and returned.
   *
   * @return	The current operator library.
   */
  def operatorLibrary = {
    if (_oplib == null) {
      // Make a new operator library and then install the primitives required
      // by the builder, if any.
      _oplib = new OperatorLibrary(this)
      for (prim <- builder.primitiveOperators) {
        _oplib.add(prim)
      } // Install primitives required by the builder.
    }
    _oplib
  }

  /**
   * Set the operator library to use.  Any prior value is lost.
   *
   * @param lib	The new operator library.
   * @return	This context.
   */
  def operatorLibrary_=(lib: OperatorLibrary) = {
    require(lib != null)
    _oplib = lib
    this
  }

  //======================================================================
  // Rule library management.
  //======================================================================

  /** The current rule library. */
  private var _rulelib: RuleLibrary = _

  /**
   * Get the current rule library.  If none has explicitly been set, then
   * a default instance is created and returned.
   *
   * @return	The current rule library.
   */
  def ruleLibrary = {
    if (_rulelib == null) { _rulelib = new RuleLibrary(memo) }
    _rulelib
  }

  /**
   * Set the rule library to use.  Any prior value is lost.
   *
   * @param lib	The new rule library.
   * @return	This context.
   */
  def ruleLibrary_=(lib: RuleLibrary) = {
    require(lib != null)
    _rulelib = lib
    this
  }
}
