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

import ornl.elision.core.BasicAtom
import scala.collection.mutable.BitSet
import ornl.elision.context.Context
import scala.collection.mutable.MutableList
import ornl.elision.util.Debugger
import ornl.elision.context.Memo
import ornl.elision.context.RuleLibrary
import ornl.elision.context.Builder
import scala.compat.Platform
import ornl.elision.core.GuardStrategy

/**
 * Package defaults and provide for creation of rewrite tasks.
 * 
 * To make a new rewrite task, set the default (if you have not already) and
 * then invoke the apply method.  This returns an object from a pool, so be
 * sure to `release` the task object when you are done to return it to the
 * pool.  This avoids object construction.
 */
object RewriteTask {
  
  /**
   * Set the size of the rewrite task pool.
   */
  private val _initialPool = 1000
  
  /**
   * The pool provides a collection of pre-allocated rewrite task objects.
   * This avoids (hopefully) having to create new instances.
   */
  private val _pool = MutableList[RewriteTask]()
  for (_count <- 0 until _initialPool) _pool += new RewriteTask
  
  /** The maximum number of rewrites allowed.  Negative values disable this. */
  var limit: Long = -1
  
  /** The rewrite timeout, in milliseconds.  Negative values disable this. */
  var timeout: Long = -1
  
  /** Whether to descend into child atoms during rewrite. */
  var descend = true
  
  /** Whether to normalize children completely during rewrite. */
  var normalizeChildren = true
  
  /**
   * Create a new rewrite task.  Once you are done with this task, you should
   * release it.
   * 
   * @param atom      The atom to rewrite.
   * @param library   The rule library that provides the rulesets.
   * @param memo      A memoization cache to use.
   * @param builder   The builder to make new atoms.
   * @param strategy  A default guard strategy to use for new rules.
   * @param rulesets  The rulesets to use for rewriting, or `Set.empty` to use
   *                  all enabled, which is the default.
   * @return  The new rewrite task.
   */
  def apply(atom: BasicAtom, library: RuleLibrary, memo: Memo, builder: Builder,
      strategy: GuardStrategy, rulesets: Set[String] = Set.empty) = {
    // Lock the pool so we can get the next object safely.
    val rt = _pool.synchronized {
      // See if there is an object in the pool.
      if (_pool.isEmpty) {
        // No objects in the pool.  Make a new one.
        Debugger("rewritepool", "Pool exhausted.")
        new RewriteTask
      } else {
        // Grab the next object from the pool.
        val rt = _pool.head
        _pool.drop(1)
        rt
      }
    }
    // Populate the task object.
    rt._remain = limit
    rt._descend = descend
    rt._atom = atom
    rt._library = library
    rt._memo = memo
    rt._builder = builder
    rt._strategy = strategy
    rt._rulesets = library.getRulesetBits(rulesets)
    rt._timeout = timeout
    rt
  }
  
  /**
   * Release the rewrite task back into the pool.
   * 
   * @param rt  The task to release.
   */
  def release(rt: RewriteTask) {
    _pool += rt
  }
}

/**
 * Encapsulate information about a single rewrite task.  This includes all
 * data about termination and timeout.
 * 
 * Defaults for new instances can be set in the companion object.
 * 
 * This is a mutable, single-thread-friendly object.  Do not use an instance
 * across multiple threads!
 */
class RewriteTask private () extends Mutable {
  
  /* Fields that configure how this operates are private vars so that the
   * companion object can reset them when reusing a task object as part of
   * the pool.
   */

  /** How many rewrites are allowed. */
  private[rewrite] var _remain: Long = _
  
  /** Whether to descend into child atoms. */
  private[rewrite] var _descend: Boolean = _
  
  /** The atom being rewritten. */
  private[rewrite] var _atom: BasicAtom = _
  
  /** The rule library to use. */
  private[rewrite] var _library: RuleLibrary = _
  
  /** The memoization cache to use. */
  private[rewrite] var _memo: Memo = _
  
  /** The builder to use. */
  private[rewrite] var _builder: Builder = _
  
  /** A default guard strategy to use. */
  private[rewrite] var _strategy: GuardStrategy = _
  
  /** The rulesets to use during rewrite. */
  private[rewrite] var _rulesets: BitSet = _
  
  /** Grab this at creation time. */
  private[rewrite] var _timeout: Long = _
  
  /** When nonnegative, the time that rewrite must stop. */
  private[rewrite] var _endTick: Long = -1
  
  /** Whether this task has timed out. */
  private[rewrite] var _timedout: Boolean = false
  
  /** Whether this task has exceeded its rewrite limit. */
  private[rewrite] var _limitexceeded: Boolean = false
  
  /**
   * Determine if the task was stopped due to a timeout.
   * @return  True iff the task has timed out.
   */
  def timedout = _timedout
  
  /**
   * Determine if the task was stopped because the limit was exceeded.
   * @return  True iff the limit was exceeded. 
   */
  def limitexceeded = _limitexceeded
}