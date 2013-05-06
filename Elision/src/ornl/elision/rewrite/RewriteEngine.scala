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
package ornl.elision.rewrite

import ornl.elision.util.Debugger
import scala.collection.mutable.BitSet
import scala.compat.Platform
import ornl.elision.core.BasicAtom
import ornl.elision.core.AtomSeq
import ornl.elision.core.AlgProp
import ornl.elision.core.Apply
import ornl.elision.core.Lambda
import ornl.elision.core.Variable
import ornl.elision.core.SpecialForm
import scala.math.BigInt.int2bigInt
import scala.math.BigInt.long2bigInt
import ornl.elision.context.Context
import ornl.elision.context.RuleApplyHandler
import ornl.elision.core.Bindings
import ornl.elision.context.ApplyBuilder

/**
 * Hold defaults for all rewrite engine instances.
 */
object RewriteEngine {
  /** The maximum number of rewrites allowed.  Negative values disable this. */
  var limit: BigInt = -1
  
  /** The rewrite timeout, in milliseconds.  Negative values disable this. */
  var timeout: BigInt = -1
  
  /** Whether to descend into child atoms during rewriting. */
  var descend = true
  
  /** Whether to normalize children completely during rewriting. */
  var normalizeChildren = true
  
  /**
   * Normalize the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit and timeout, if set.
   * 
   * @param atom      The atom to rewrite.
   * @param con       The context providing the rule library, console, and
   *                  memoization cache.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def apply(atom: BasicAtom, con: Context) = new RewriteEngine(con)(atom)
}

/**
 * Manage and perform rewriting of atoms.
 * 
 * This class controls the rewriting process, including timing out, descending
 * into child atoms, and interacting with the memoization cache.
 * 
 * To use this make an instance, configure it, and then hand an atom to the
 * `apply` method.  You can check condition flags at the end to see what
 * happened.
 * 
 * @param context The context providing the rule library, console, and
 *                memoization cache.
 */
class RewriteEngine(context: Context) {
  
  //======================================================================
  // Configuration.
  //======================================================================

  /** The rewrite limit.  Negative numbers mean *no* limit. */
  private var _limit = RewriteEngine.limit
  
  /** Remaining rewrites. */
  private var _remain = _limit
  
  /**
   * Set the rewrite limit.  Negative values mean no limit.
   * 
   * @param lim The rewrite limit.
   * @return  This instance.
   */
  def limit_=(lim: BigInt) = {
    Debugger("engine", "Set limit: "+lim)
    _limit = lim
    this
  }
  
  /** The method to use to normalize a child. */
  private var _rewritechild: (BasicAtom, BitSet) => (BasicAtom, Boolean) =
    _rewrite _
  
  /**
   * Set whether to normalize children when rewriting.  The alternative is to
   * rewrite children once, recursively, and then return to the top level to
   * check it again.  The latter is a better strategy in general, but the
   * former results in the normalized form of children being stored in the
   * memoization cache.
   * 
   * Both should ultimately result in the same rewrite
   * if everything works correctly, but can result in different rewrites
   * under certain conditions.  Specifically, if an intermediate form of a
   * child causes a higher-level rewrite to occur, whereas the normalized form
   * of the child causes a different (or no) higher-level rewrite to occur,
   * then you will get different results.  This is really a problem with the
   * rules.
   * 
   * The default is to fully normalize children.
   * 
   * @param norm  Whether to normalize children.
   * @return  This instance.
   */
  def normalizeChildren_=(norm: Boolean) = {
    Debugger("engine", "Normalize Children: " +
        (if (norm) "enabled" else "disabled"))
    if (norm) _rewritechild = _rewrite _
    else _rewritechild = _rewriteOnce _
    this
  }
  normalizeChildren_=(RewriteEngine.normalizeChildren)
    
  /** Iff true, normalize children. */
  private var _descend = RewriteEngine.descend
  
  /**
   * Set whether to rewrite children recursively.  The default is to rewrite
   * children.
   * 
   * @param descend  Whether to rewrite children recursively.
   * @return  This instance.
   */
  def descend_=(descend: Boolean) = {
    Debugger("engine", "Descent: " +
        (if (descend) "enabled" else "disabled"))
    _descend = descend
    this
  }
  
  /** Maximum duration to allow rewriting to continue.  No limit if negative. */
  private var _timeout: BigInt =
    if (context.hasProperty("rewrite_timeout")) {
      context.getProperty[BigInt]("rewrite_timeout")
    } else {
      -1
    }
  
  /**
   * Set the timeout for rewriting.  The default is to try to pull the timeout
   * value from the property `rewrite_timeout`.  If this fails, -1 is used.
   * 
   * @param timeout The timeout duration, in milliseconds.
   * @return  This instance.
   */
  def timeout_=(timeout: BigInt) = {
    Debugger("engine", "Timeout: " + timeout)
    _timeout = timeout
    this
  }
  
  //======================================================================
  // Condition flags.
  //======================================================================
  
  /** Has rewriting timed out? */
  private var _timedOut = false
  
  /**
   * Whether rewriting has timed out.
   * 
   * @return  True if rewriting timed out.
   */
  def timedOut = _timedOut
  
  /** Has the rewriting limit been? */
  private var _limitExceeded = false
  
  /**
   * Whether the rewriting limit has been reached.
   * 
   * @return  True if the rewriting limit has been reached.
   */
  def limitExceeded = _limitExceeded
  
  //======================================================================
  // Rewriting.
  //======================================================================
  
  /** This, when non-negative, holds the time when the rewrite should stop. */
  private var _endTick: BigInt = -1 
  
  /**
   * Normalize the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit and timeout, if set.
   * 
   * This method uses the memoization cache, if enabled.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   *                  All enabled is the default.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def apply(atom: BasicAtom, rulesets: Set[String] = Set.empty) = {
    Debugger("rewrite", "Rewriting "+atom.toParseString+
        ", timing out with "+_timeout+".")
    _timedOut = false
    _remain = _limit
    val bits = (
        if (rulesets.isEmpty) context.ruleLibrary.getRulesetBits()
        else context.ruleLibrary.getRulesetBits(rulesets))
    _endTick = (if (_timeout >= 0) Platform.currentTime + _timeout else -1)
    _rewrite(atom, bits)
  }
  
  def doRewrite(atom: BasicAtom, hint: Option[Any] = None) = apply(atom)

  /**
   * Rewrite the provided atom using the specified rulesets and the
   * configuration options of the engine.  This is where rewriting is
   * ultimately performed.
   * 
   * This method uses the memoization cache, if enabled.  This should be
   * the only method that references the cache.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, as a bitstring.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewrite(atom: BasicAtom,
      rulesets: BitSet): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (_endTick >= 0 && Platform.currentTime > _endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      _timedOut = true
      return (atom, false)
    }
    
    // See if the atom is already "clean" with respect to the rulesets we are
    // using now.  If so, we are done.
    if (rulesets.subsetOf(atom.cleanRulesets)) {
      // The atom is already clean.
      Debugger("rewrite", "Atom "+atom.toParseString+" is clean.")
      return (atom, false)
    }
    
    // Now we can check the memoization cache.  If we find the atom in the
    // cache, we are done.
    context.memo.get(atom, rulesets) match {
      case Some(pair) =>
        // The atom has been rewritten.
        Debugger("rewrite", "Got cached rewrite: "+atom.toParseString+
            " to: "+pair._1.toParseString+".")
        return pair
        
      case None =>
        // Not found in the cache.  We actually have to perform the rewriting,
        // and then we can store the result in the cache.
        var run = true
        var finalatom = atom
        var changed = false
        while (run) {
          // The rewrite once method checks for timeout, so we don't need to
          // do that here.
          _rewriteOnce(finalatom, rulesets) match {
            case (anatom, false) =>
              // No rewrite happened.  We are done.
              run = false
              finalatom = anatom
              
            case (anatom, true) =>
              // A rewrite happened.  If we "climbed" then use the new atom.
              // Otherwise we might be at a fixpoint.  If so, stop.
              if (finalatom == anatom) {
                Debugger("rewrite", "Hit fixpoint for atom "+
                    anatom.toParseString+".")
                run = false
              } else {
                Debugger("rewrite", "Rewrote to: "+anatom.toParseString)
                changed = true
                finalatom = anatom
              }
          }
        } // Run until we can no longer rewrite.
        
        // The atom has been rewritten as much as it can be.  Memoize it.
        // Mark it clean with respect to the given rulesets.
        Debugger("rewrite", "Completed rewrite of "+atom.toParseString+
            " to "+finalatom.toParseString+".")
        context.memo.put(atom, rulesets, finalatom, 0)
        finalatom.cleanRulesets = finalatom.cleanRulesets.union(rulesets)
        return (finalatom, changed)
    }
  }
  
  /**
   * Rewrite the provided atom once, if possible.  Children may be rewritten,
   * depending on whether descent is enabled.
   * 
   * Do not memoize this method.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteOnce(atom: BasicAtom,
      rulesets: BitSet): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (_endTick >= 0 && Platform.currentTime > _endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      _timedOut = true
      return (atom, false)
    }
    // Decrement the allowed rewrite count.
    if (_remain > 0) {
      _remain -= 1
      if (_remain <= 0) {
        Debugger("rewrite", "Rewrite limit exceeded: "+atom.toParseString)
        _limitExceeded = true
        return (atom, false)
      }
    }
    var (newtop, appliedtop) = _rewriteTop(atom, rulesets)
    if (_descend) {
      var (newatom, applied) = _rewriteChildren(newtop, rulesets)     
      (newatom, appliedtop || applied)
    } else {
      (newtop, appliedtop)
    }
  }
  
  /**
   * Rewrite the atom at the top level, once.
   * 
   * Do not memoize this method.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteTop(atom: BasicAtom,
      rulesets: BitSet): (BasicAtom, Boolean) = {
    // Now try every rule until one applies.
    Debugger("rewrite", "Rewriting atom: " + atom.toParseString)
    for (rule <- context.ruleLibrary.getRules(atom, rulesets)) {
      // Before applying the rule, check the timeout.  If we have timed out,
      // stop right now, put the ruleset down, and hand in our work.
      if (_endTick >= 0 && Platform.currentTime > _endTick) {
        Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
        _timedOut = true
        return (atom, false)
      }      
      Debugger("rewrite", "Trying rule: " + rule.toParseString)
      val (newatom, applied) =
        RuleApplyHandler(rule, atom, Bindings(), None, context)
      if (applied) {
        // Return the rewrite result.
        Debugger("rewrite", "Rewrote to: " + newatom.toParseString)
        return (newatom, true)
      }
    } // Try all rules.

    Debugger("rewrite", "No rule applied to: " + atom.toParseString)
    return (atom, false)
  }
  
  /**
   * Recursively rewrite the atom and its children.  This method understands
   * atom collections and operators.
   * 
   * Do not memoize this method.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteChildren(atom: BasicAtom,
      rulesets: BitSet): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (_endTick >= 0 && Platform.currentTime > _endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      _timedOut = true
      return (atom, false)
    } else {
      atom match {
        case AtomSeq(props, atoms) =>
          var flag = false
          // Rewrite the properties.  The result must still be a property spec.
          // If not, we keep the same properties.
          val newProps = _rewritechild(props, rulesets) match {
            case (ap: AlgProp, true) => flag = true; ap
            case _ => props
          }
          // Rewrite the atoms.
          val newAtoms = atoms.map {
            atom =>
              val (newatom, applied) = _rewritechild(atom, rulesets)
              flag ||= applied
            newatom
          }
          // Return the result.
          if (flag) (AtomSeq(newProps, newAtoms), true) else (atom, false)
        
        case Apply(lhs, rhs) =>
          val newlhs = _rewritechild(lhs, rulesets)
          val newrhs = _rewritechild(rhs, rulesets)
          if (newlhs._2 || newrhs._2) {
            (ApplyHandler(newlhs._1, newrhs._1, context), true)
          } else {
            (atom, false)
          }
        
        case Lambda(param, body) =>
          val newparam = _rewritechild(param, rulesets) match {
            case (v: Variable, true) => (v, true)
              case _ => (param, false)
          }
          val newbody = _rewritechild(body, rulesets)
          if (newparam._2 || newbody._2) {
            (Lambda(newparam._1, newbody._1), true)
          } else {
            (atom, false)
          }

        case SpecialForm(tag, content) =>
          val newlhs = _rewritechild(tag, rulesets)
          val newrhs = _rewritechild(content, rulesets)
          if (newlhs._2 || newrhs._2) {
            (SpecialForm(atom .loc, newlhs._1, newrhs._1), true)
          } else {
            (atom, false)
          }

        case _ =>
          // Do nothing in this case.
          (atom, false)
      }
    }
  }
}