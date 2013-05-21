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
import ornl.elision.core.Bindings
import ornl.elision.context.ApplyBuilder
import ornl.elision.util.Loc
import ornl.elision.core.GuardStrategy
import ornl.elision.context.RuleApplyHandler

/**
 * Hold defaults for all rewrite engine instances.
 */
object RewriteEngine {
  
  private var reNormalizeChildren: RewriteEngine = new RewriteEngine()
  private var re: RewriteEngine = new RewriteEngine() {
    override protected[elision] val _rewritechild = _rewriteOnce _
  }
  
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
   */
  var normalizeChildren = true
  
  /**
   * Obtain a rewrite engine.  Because rewrite engines are stateless, this
   * actually returns an instance based on the setting of rewrite children,
   * and will not construct an object.
   * 
   * @param task    The rewrite task.  This may be modified during rewrite.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def apply(task: RewriteTask) = {
    if (normalizeChildren) {
      reNormalizeChildren(task)
    } else {
      re(task)
    }
  }
}

/**
 * Manage and perform rewriting of atoms.
 * 
 * This class controls the rewriting process, including timing out, descending
 * into child atoms, and interacting with the memoization cache.
 * 
 * Obtain an instance through the companion object, not through the constructor.
 * Then create a task object to perform rewriting.
 * 
 * This class is stateless so that a single instance can be used everywhere.
 */
class RewriteEngine protected[elision] () {
  
  /* Do not add state data to this class!  It must be stateless; that is the
   * purpose of the task object.  If you need to keep information as you do
   * the rewrite, that is the place (RewriteTask) to stash that information!
   */
  
  /** The method to use to normalize a child. */
  protected[elision] val _rewritechild: (BasicAtom, RewriteTask) => (BasicAtom, Boolean) =
    _rewrite _
  
  /**
   * Normalize the given atom using the specified task parameters.
   * 
   * This method uses the memoization cache, if enabled.
   *
   * @param task    The rewrite task. 
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def apply(task: RewriteTask) = {
    Debugger("rewrite", "Rewriting "+task._atom.toParseString+
        ", timing out with "+task._timeout+".")
    task._endTick =
      (if (task._timeout >= 0) Platform.currentTime + task._timeout else -1)
    _rewrite(task._atom, task)
  }

  /**
   * Rewrite the provided atom using the specified rulesets and the
   * configuration options of the engine.  This is where rewriting is
   * ultimately performed.
   * 
   * This method uses the memoization cache, if enabled.  This should be
   * the only method that references the cache.
   * 
   * @param atom    The atom to rewrite.
   * @param task    The rewrite task. 
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewrite(atom: BasicAtom,
      task: RewriteTask): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (task._endTick >= 0 && Platform.currentTime > task._endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      task._timedout = true
      return (atom, false)
    }
    
    // See if the atom is already "clean" with respect to the rulesets we are
    // using now.  If so, we are done.
    if (task._rulesets.subsetOf(atom.cleanRulesets)) {
      // The atom is already clean.
      Debugger("rewrite", "Atom "+atom.toParseString+" is clean.")
      return (atom, false)
    }
    
    // Now we can check the memoization cache.  If we find the atom in the
    // cache, we are done.
    task._memo.get(atom, task._rulesets) match {
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
          _rewriteOnce(finalatom, task) match {
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
        task._memo.put(atom, task._rulesets, finalatom, 0)
        finalatom.cleanRulesets = finalatom.cleanRulesets.union(task._rulesets)
        return (finalatom, changed)
    }
  }
  
  /**
   * Rewrite the provided atom once, if possible.  Children may be rewritten,
   * depending on whether descent is enabled.
   * 
   * Do not memoize this method.
   * 
   * @param atom    The atom to rewrite.
   * @param task    The rewrite task. 
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteOnce(atom: BasicAtom,
      task: RewriteTask): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (task._endTick >= 0 && Platform.currentTime > task._endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      task._timedout = true
      return (atom, false)
    }
    // Decrement the allowed rewrite count.
    if (task._remain > 0) {
      task._remain -= 1
      if (task._remain <= 0) {
        Debugger("rewrite", "Rewrite limit exceeded: "+atom.toParseString)
        task._limitexceeded = true
        return (atom, false)
      }
    }
    var (newtop, appliedtop) = _rewriteTop(atom, task)
    if (task._descend) {
      var (newatom, applied) = _rewriteChildren(newtop, task)     
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
   * @param atom    The atom to rewrite.
   * @param task    The rewrite task. 
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteTop(atom: BasicAtom,
      task: RewriteTask): (BasicAtom, Boolean) = {
    // Now try every rule until one applies.
    Debugger("rewrite", "Rewriting atom: " + atom.toParseString)
    for (rule <- task._library.getRules(atom, task._rulesets)) {
      // Before applying the rule, check the timeout.  If we have timed out,
      // stop right now, put the ruleset down, and hand in our work.
      if (task._endTick >= 0 && Platform.currentTime > task._endTick) {
        Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
        task._timedout = true
        return (atom, false)
      }      
      Debugger("rewrite", "Trying rule: " + rule.toParseString)
      val (newatom, applied) =
        RuleApplyHandler(rule, atom, Bindings(), None, task._builder)
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
   * @param atom    The atom to rewrite.
   * @param task    The rewrite task. 
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  private def _rewriteChildren(atom: BasicAtom,
      task: RewriteTask): (BasicAtom, Boolean) = {
    // Has rewriting timed out?
    if (task._endTick >= 0 && Platform.currentTime > task._endTick) {
      Debugger("rewrite", "Rewriting timed out: " + atom.toParseString)
      task._timedout = true
      return (atom, false)
    } else {
      atom match {
        case AtomSeq(props, atoms) =>
          var flag = false
          // Rewrite the properties.  The result must still be a property spec.
          // If not, we keep the same properties.
          val newProps = _rewritechild(props, task) match {
            case (ap: AlgProp, true) => flag = true; ap
            case _ => props
          }
          // Rewrite the atoms.
          val newAtoms = atoms.map {
            atom =>
              val (newatom, applied) = _rewritechild(atom, task)
              flag ||= applied
            newatom
          }
          // Return the result.
          if (flag) {
            (task._builder.newAtomSeq(Loc.internal, newProps, newAtoms), true)
          } else {
            (atom, false)
          }
        
        case Apply(lhs, rhs) =>
          val newlhs = _rewritechild(lhs, task)
          val newrhs = _rewritechild(rhs, task)
          if (newlhs._2 || newrhs._2) {
            (task._builder.newApply(Loc.internal, newlhs._1, newrhs._1,
                task._strategy), true)
          } else {
            (atom, false)
          }
        
        case Lambda(param, body) =>
          val newparam = _rewritechild(param, task) match {
            case (v: Variable, true) => (v, true)
              case _ => (param, false)
          }
          val newbody = _rewritechild(body, task)
          if (newparam._2 || newbody._2) {
            (task._builder.newLambda(Loc.internal, newparam._1,
                newbody._1, task._strategy), true)
          } else {
            (atom, false)
          }

        case SpecialForm(tag, content) =>
          val newlhs = _rewritechild(tag, task)
          val newrhs = _rewritechild(content, task)
          if (newlhs._2 || newrhs._2) {
            (task._builder.newSpecialForm(Loc.internal, newlhs._1,
                newrhs._1, task._strategy), true)
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