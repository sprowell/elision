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
package ornl.elision.context

import scala.collection.immutable.HashSet
import scala.collection.immutable.List
import scala.collection.mutable.BitSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}

import ornl.elision.core.Apply
import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.Fickle
import ornl.elision.core.Literal
import ornl.elision.core.Mutable
import ornl.elision.core.NoProps
import ornl.elision.core.Operator
import ornl.elision.core.OperatorRef
import ornl.elision.core.RewriteRule
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.Variable
import ornl.elision.core.giveMkParseString
import ornl.elision.util.Debugger
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.util.OmitSeq

/**
 * Indicate an attempt to use an undeclared ruleset.
 * 
 * @param loc   Location of the atom containing the bad reference, or of the
 *              bad reference.
 * @param msg		A human readable message.
 */
class NoSuchRulesetException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add an identity rule.
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class IdentityRuleException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add a rule whose pattern is bindable (i.e., a
 * simple variable).
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class BindablePatternException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add a rule whose pattern is a literal, when such
 * are not allowed.
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class LiteralPatternException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Encapsulate a rule library.
 * 
 * == Purpose ==
 * A rule library instance contains rules and methods for accessing relevant
 * rules and for performing "automated" rewriting.
 * 
 * Rules can be organized into rulesets.  A single rule may be in multiple
 * rulesets.  A rule can also be placed in no rulesets, though this is not
 *  advisable, as it is difficult to access the rule later.  In fact, the
 * only way to see rules that are not part of any active ruleset is via the
 * `getRuleList` method.
 * 
 * The rule library also manages the rulesets.  These must be declared before
 * they can be used, and they can be enabled and disabled to better control
 * rewriting.
 * 
 * @param memo      The memoization cache to use.
 * @param context   The context needed to build atoms.
 */
class RuleLibrary(memo: Memo, context: Context) extends Fickle with Mutable {
  
  /**
   * Create a shallow clone of this rule library.  This returns a new rule
   * library instance; since the content of the library is immutable, this
   * new instance is technically independent of the original.
   * 
   * @return  The clone.
   */
  override def clone: RuleLibrary = {
    val clone = new RuleLibrary(memo, context)
    
    clone._kind2rules.clear
    for(mapping <- this._kind2rules) {
        clone._kind2rules += mapping
    }
    
    clone._op2rules.clear
    for(mapping <- this._op2rules) {
        clone._op2rules += mapping
    }
    
    clone._rs2bit.clear
    for(mapping <- this._rs2bit) {
        clone._rs2bit += mapping
    }
    
    clone._active.clear
    for(mapping <- this._active) {
        clone._active += mapping
    }

    for(mapping <- this._activeNames) {
        clone._activeNames += mapping
    }
    
    clone._nextrs = this._nextrs
    clone
  }
  
  //======================================================================
  // Control what rules can be added.
  //======================================================================
  
  /**
   * Should rules that rewrite literals be permitted.  If true, it is possible
   * to create rules that rewrite one literal to another.  In general this is
   * a bad idea.
   */
  var _allowLiteralRules = false
  
  //======================================================================
  // Controlling active rulesets.
  //======================================================================

  /** The active rulesets. */
  val _active = new BitSet()

  /** The active rulesets, by names. */
  var _activeNames : Set[String] = new HashSet[String]()

  /**
   * Enable a ruleset.
   * 
   * @param name	The name of the ruleset to enable.
   * @return	This library.
   */
  def enableRuleset(name: String) = {
    _active += getRulesetBit(name)
    _activeNames += name
    Debugger("rule.rulesets", "Enabled ruleset: " + name)
    Debugger("rule.rulesets", "Active rulesets: " +
        _activeNames.mkString(", "))
    this
  }
  
  /**
   * Disable a ruleset.
   * 
   * @param name	The name of the ruleset to disable.
  * @return	This library.
  */
  def disableRuleset(name: String) = {
    _active -= getRulesetBit(name)
    _activeNames -= name
    Debugger("rule.rulesets", "Disabled ruleset: " + name)
    Debugger("rule.rulesets", "Active rulesets: " +
        _activeNames.mkString(", "))
    this
  }
    
  //======================================================================
  // Ruleset management.
  //======================================================================

  /**
  * A map from ruleset names to integers indicating the rulesets position
  * in the bitsets.
  */
  private val _rs2bit = MMap[String,Int]()
  
  /** Bit index of the next ruleset. */
  private var _nextrs = 1
  
  /** Local convenience method to get the next ruleset index. */
  private def bump() = { val tmp = _nextrs ; _nextrs += 1 ; tmp }
  
  /** Bit zero is reserved for the default ruleset. */
  _rs2bit += ("DEFAULT" -> 0)
  
  /** The default ruleset is on by default. */
  enableRuleset("DEFAULT")
  
  /**
   * Get all ruleset names.  Note that this includes the `DEFAULT` ruleset.
   * 
   * return An iterable list of ruleset names.
   */
  def getAllRulesets() = {
    _rs2bit map { _._1 }
  }
  
  /**
   * Get the set of active ruleset names.  This may include the `DEFAULT`
   * ruleset.
   * 
   * @return The set of active ruleset names.
   */
  def getActiveRulesets() = {
    _activeNames
  }
  
  /**
  * Get the bit for a ruleset.
  * 
  * @param name	The ruleset name.
  * @return	The bit for the ruleset.
  * @throws	NoSuchRulesetException
  * 					The ruleset has not been declared, and undeclared rulesets are
  * 					not allowed.
  */
  private def getRulesetBit(name: String) =
    _rs2bit.getOrElseUpdate(name,
        throw new NoSuchRulesetException(Loc.internal,
            "The ruleset " + name + " has not been declared."))
  
  /**
   * Get the bit set for a collection of rulesets.
   * 
   * @param names   The names of the rulesets to include.
   * @return  The bit set for those rulesets.
   */
  def getRulesetBits(names: Set[String]) = {
    names.foldLeft(new BitSet())(_ += getRulesetBit(_))
  }
  
  /**
   * Get the bit set for the currently active rulesets.
   * 
   * @return  The bit set for the currently active rulesets.
   */
  def getRulesetBits() = {
    _active
  }
  
  /**
  * Declare the ruleset.
  * 
  * @param name	The name of the new ruleset.
  * @return	True if the ruleset was declared, and false if it was already
  * 					(previously) declared.
  */
  def declareRuleset(name: String) = {
    _rs2bit.get(name) match {
      case None => _rs2bit += (name -> bump()) ; true
      case _ => false
    }
  }

  //======================================================================
  // Rule management.
  //======================================================================
  
  /**
   * Map each kind of atom to a list of rules for rewriting that atom.  The
   * rules are ordered, and each has an associated bit set that tells which
   * rulesets the rule is in.
   */
  private val _kind2rules = MMap[Class[_],ListBuffer[(BitSet,RewriteRule)]]()

  /**
   * Map each operator to a list of rules for rewriting terms with that
   * operator at the root.  The rules are ordered, and each has an associated
   * bit set that tells which rulesets the rule is in.
   */
  private val _op2rules = MMap[String,ListBuffer[(BitSet,RewriteRule)]]()
  
  /**
   * Map rule names to the rules that were added with that name.  Rules do not
   * require names, but if a rule has a name it must be unique among all added
   * rules.  This requires special enforcement during addition, and also means
   * that it is sometimes necessary to remove rules.
   */
  private val _name2rules = MMap[String, List[RewriteRule]]()

  /**
   * Add a rewrite rule to this library.
   * 
   * @param rule	The rewrite rule to add.
   * @throws	NoSuchRulesetException
   * 					At least one ruleset mentioned in the rule has not been declared,
   * 					and undeclared rulesets are not allowed.
   */
  def add(rule: RewriteRule) = {
    // A rule whose left-hand side is either bindable is not allowed.
    if (rule.pattern.isBindable) {
      throw new BindablePatternException(rule.loc,
          "The rule " + rule.toParseString +
          " has a bindable pattern.  It cannot be added to the system.")
    }
    
    // Rules that rewrite literals might not be allowed.
    if (rule.pattern.isInstanceOf[Literal[_]] && !_allowLiteralRules) {
      throw new LiteralPatternException(rule.loc,
          "The rule " + rule.toParseString +
          " has a literal pattern.  It cannot be added to the system.")
    }
    
    // Complete the rule.
    val rules = Completor.complete(rule, context)
    
    // Rules can have names, and adding a rule with the same name as a
    // previously-added rule replaces the prior rule.  This is a novel
    // case, since typically rules added first have precedence.
    // For this reason, if the rule has a name first remove any prior rules
    // with the same name.  Then record the rules under the name.
    rule.name match {
      case None =>
      case Some(value) =>
        // The rule has a name.  See if we need to remove prior rules that
        // were added with that name.
        _name2rules.get(value) match {
          case None =>
          case Some(priors) =>
            for (prior <- priors) doRemove(prior)
        }
        _name2rules(value) = rules
    }
    
    // Add the rules.
    for (rule2 <- rules) doAdd(rule2)
    this
  }

  /**
   * Add a rewrite rule to this library.
   * 
   * @param rule	The rewrite rule to add.
   * @throws	NoSuchRulesetException
   * 					At least one ruleset mentioned in the rule has not been declared,
   * 					and undeclared rulesets are not allowed.
   */
  private def doAdd(rule: RewriteRule) = {
    // Make sure the rule is not (or does not appear to be) an identity.
    if (rule.pattern == rule.rewrite) {
      throw new IdentityRuleException(rule.loc,
          "The rule " + rule.toParseString +
          " appears to be an identity.  It cannot be added to the system.")
    }
    
    // Figure out what rulesets this rule is in.  We build the bitset here.
    val bits = new BitSet()
    for (rs <- rule.rulesets) bits += getRulesetBit(rs)
    
    // Get (or create) the list for the kind of atom the rule's pattern uses.
    val list = getRuleList(rule.pattern)
    
    // Okay, now add the rule to the list.  We perform no checking to see if
    // the rule is already present.
    list += Pair(bits, rule)
    this
  }
  
  /**
   * Remove a rewrite rule from this library.
   * 
   * @param rule  The rewrite rule to remove.
   * @throws  NoSuchRulesetException
   *          At least one ruleset mentioned in the rule has not been declared,
   *          and undeclared rulesets are not allowed.
   */
  private def doRemove(rule: RewriteRule) = {
    // Get the list for the kind of atom the rule's pattern uses.
    val list = getRuleList(rule.pattern)
    // Get the bitset for the rule's ruleset membership.
    val bits = new BitSet()
    for (rs <- rule.rulesets) bits += getRulesetBit(rs)
    // Now remove every instance of the rule from the list.
    for (idx <- list.indices.reverse)
      if (list(idx) == (bits, rule)) list.remove(idx)
    this
  }

  /**
   * Helper method to get all rules for a particular kind of atom.
   * 
   * @param atom	The atom.
   * @return	The list of rules for the given kind of atom.
   */
  private def getRuleList(atom: BasicAtom) = atom match {
    case Apply(op:Operator, _) =>
      _op2rules.getOrElseUpdate(op.name, ListBuffer[(BitSet, RewriteRule)]())
    case Apply(OperatorRef(op), _) =>
      _op2rules.getOrElseUpdate(op.name, ListBuffer[(BitSet, RewriteRule)]())
    case _ =>
      _kind2rules.getOrElseUpdate(atom.getClass(),
          ListBuffer[(BitSet, RewriteRule)]())
  }

  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the currently active rulesets.
   * 
   * @param atom	The atom to which the rule may apply.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom) =
      for ((bits, rule) <- getRuleList(atom) ; if (!(bits & _active).isEmpty))
        yield rule

  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the specified rulesets.
   * 
   * @param atom	The atom to which to the rules may apply.
   * @param name	The ruleset names.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom, names: Set[String]) = {
    val rsbits = names.foldLeft(new BitSet())(_ += getRulesetBit(_))
    for ((bits, rule) <- getRuleList(atom); if (!(bits & rsbits).isEmpty))
      yield rule
  }
  
  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the specified rulesets.
   * 
   * @param atom  The atom to which to the rules may apply.
   * @param name  The rulesets as a bitstring.
   * @return  A list of rules.
   */
  def getRules(atom: BasicAtom, rsbits: BitSet) = {
    for ((bits, rule) <- getRuleList(atom); if (!(bits & rsbits).isEmpty))
      yield rule
  }

  /**
   * Get all rules contained in the system, in the order they would be
   * considered during rewrite.
   * 
   * @return  The list of rules.
   */
  def getAllRules() = {
    var all = List[RewriteRule]()
    for ((_, list) <- _kind2rules ++ _op2rules; (_, rule) <- list) all :+= rule
    all
  }
}


/**
 * Generate synthetic rules based on the provided rule, if necessary.
 * 
 * Synthetic rules are required when a rule pattern's root is an associative
 * operator.  There are two cases.
 * 
 * If the operator is both associative and commutative, then one synthetic rule
 * is constructed by adding an additional argument to the right-hand side of
 * the argument list in both the pattern and the rewrite.
 * 
 * Example:
 * {{{
 * { rule and($x, not($x)) -> false }
 * }}}
 * Synthetic Rule:
 * {{{
 * { rule and($x, not($x), $r) -> and(false, $r) }
 * }}}
 * (The rewrite in the above rule is of course reduced to `false`.)
 * 
 * If the operator is associative but not commutative, then we must add three
 * synthetic rules that add new arguments to either end of both the pattern
 * and rewrite.
 * 
 * Example:
 * {{{
 * { rule concat($x, inv($x)) -> "" }
 * }}}
 * Synthetic Rules:
 * {{{
 * { rule concat($l, $x, inv($x)) -> concat($l, "") }
 * { rule concat($x, inv($x), $r) -> concat("", $r) }
 * { rule concat($l, $x, inv($x), $r) -> concat($l, "", $r) }
 * }}}
 */
private object Completor {
  
  /**
   * Perform partial completion for the given rule by generating the necessary
   * synthetic rules.
   * 
   * @param rule    The provided rule.
   * @param op      The operator.
   * @param as      The atom sequence.
   * @param context The context needed to build atoms.
   * @return  A list of rules, including the original rule and any synthetic
   *          rules.
   */
  private def _complete(rule: RewriteRule, op: Operator, as: AtomSeq,
      context: Context): List[RewriteRule] = {
    // Make the list
    var list = List[RewriteRule](rule)
    
    // Extract the operator properties.
    val props = op match {
      case po: SymbolicOperator => po.params.props
      case _ => NoProps
    }
    
    // If the operator is not associative, we don't need to do anything.
    if (!props.isA(false)) {
      return list
    }
    
    // Extract the pattern and rewrite.
    val pattern = rule.pattern
    val rewrite = rule.rewrite

    // The operator is associative.  We must at least add an argument on
    // the right-hand side.  Make and add the argument, and then add the
    // synthetic rule.
    var right = Variable(as(0).theType, "::R")
    var newpatternlist = as.atoms :+ right
    var newrewritelist = OmitSeq[BasicAtom](rewrite) :+ right
    val newRule = RewriteRule(rule.loc,
        ApplyHandler(op, AtomSeq(props, newpatternlist), context),
        ApplyHandler(op, AtomSeq(props, newrewritelist), context),
        rule.guards,
        rule.rulesets,
        true)
    list :+= newRule
    
    // If the operator is commutative, we are done.
    if (props.isC(false)) {
      for (rule <- list) {
        Debugger("rule.completion", "Generated synthetic rule: " +
            newRule.toParseString)
      } // Show the rules.
      return list
    }
    
    // Repeat the above to add an argument on the left-hand side.
    var left = Variable(as(0).theType, "::L")
    newpatternlist = left +: as.atoms
    newrewritelist = left +: OmitSeq[BasicAtom](rewrite)
    list :+= RewriteRule(rule.loc,
        ApplyHandler(op, AtomSeq(props, newpatternlist), context),
        ApplyHandler(op, AtomSeq(props, newrewritelist), context),
        rule.guards, rule.rulesets, true)
        
    // And again add the argument on the right-hand side.
    newpatternlist = newpatternlist :+ right
    newrewritelist = newrewritelist :+ right
    list :+= RewriteRule(rule.loc,
        ApplyHandler(op, AtomSeq(props, newpatternlist), context),
        ApplyHandler(op, AtomSeq(props, newrewritelist), context),
        rule.guards, rule.rulesets, true)
        
    // Done.
    for (rule <- list) {
      Debugger("rule.completion", "Generated synthetic rule: " +
          newRule.toParseString)
    } // Show the rules.
    return list
  }
  
  /**
   * Perform partial completion for the given rule by generating the necessary
   * synthetic rules.
   * 
   * @param rule	The provided rule.
   * @param context The context needed to build atoms.
   * @return	A list of rules, including the original rule and any synthetic
   * 					rules.
   */
  def complete(rule: RewriteRule, context: Context): List[RewriteRule] = {
    rule.pattern match {
      case Apply(op: OperatorRef, as: AtomSeq) =>
        _complete(rule, op.operator, as, context)
      case Apply(op: Operator, as: AtomSeq) =>
        _complete(rule, op, as, context)
      case _ =>
        List[RewriteRule](rule)
    }
  }
}
