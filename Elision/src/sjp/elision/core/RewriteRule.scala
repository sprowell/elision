/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com)
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package sjp.elision.core

/**
 * This strategy applies the rules in a ruleset.  The first rule, in declared
 * order, to rewrite the atom wins.  At most one rewrite is performed.
 * 
 * The basic syntax is:
 * {{{
 * { rulesets NAME1, NAME2, ..., NAMEn }
 * }}}
 * This applies rules from the named rulesets.  The listing order of the
 * rulesets is //not// significant; only the declaration order of the rules.
 * 
 * @param context	The context supplying the rules.
 * @param names		The names of the rulesets to apply.
 */
case class RulesetStrategy(context: Context, names: List[String])
extends BasicAtom with Rewriter {
  val theType = STRATEGY
  val isConstant = true
  val isTerm = true
  val constantPool = None
  val deBruijnIndex = 0
  val depth = 0
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
    case RulesetStrategy(_, onames) if names == onames => Match(binds)
    case _ => Fail("Ruleset strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = (this, false)
  def toParseString =
    names.map(toESymbol(_)).mkString("{ apply rulesets ", ", ", " }")
  override def toString = "RulesetStrategy(" + context + ", " +
  		names.map(toESymbol(_)).mkString(", ") + ")"
  		
  /**
   * Apply this strategy.  If any rule completes then the returned flag is
   * true.  Otherwise it is false.
   */
  def doRewrite(atom: BasicAtom): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = context.getRules(atom, names)
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.tryRewrite(atom)
      if (applied) return (newatom, applied)
    }
    return (atom, false)
  }
}

class MapStrat(sfh: SpecialFormHolder, val lhs: BasicAtom,
    val include: BasicAtom, val exclude: BasicAtom)
extends SpecialForm(sfh.tag, sfh.content) with Rewriter {
  override val theType = STRATEGY
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomSeq(props, atoms) =>
	    // All we can do is apply the lhs to each atom in the list.
	    (AtomSeq(props, atoms.map(Apply(lhs,_))), true)
	  case Apply(op, AtomSeq(props, atoms)) =>
      // We apply the lhs to each argument whose parameter meets the
      // label criteria.  This is modestly tricky.
      (Apply(op, AtomSeq(props, atoms.map(Apply(lhs,_)))), true)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
}

case class MapStrategy(include: Set[String], exclude: Set[String],
    lhs: BasicAtom) extends BasicAtom with Rewriter {
	val theType = STRATEGY
	
	val isConstant = lhs.isConstant
	val isTerm = lhs.isTerm
	val deBruijnIndex = lhs.deBruijnIndex
	val depth = lhs.depth + 1
	val constantPool =
	  Some(BasicAtom.buildConstantPool(theType.hashCode, lhs))
	  
	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
	    hints: Option[Any]) =
  	subject match {
	  case MapStrategy(oin, oex, olhs) =>
	    if (include != oin || exclude != oex)
	      Fail("Labels do not match.", this, subject)
      else
        lhs.tryMatch(olhs, binds, hints)
	  case _ => Fail("Subject is not a map strategy.", this, subject)
	}
	  
	def rewrite(binds: Bindings) = lhs.rewrite(binds) match {
	  case (newlhs, true) => (MapStrategy(include, exclude, newlhs), true)
	  case _ => (this, false)
	}
	
	def toParseString = "{ map " +
			include.map("@" + toESymbol(_)).mkString(" ") +
			exclude.map("-@" + toESymbol(_)).mkString(" ") +
			" " + lhs.toParseString + " }"
			
  override def toString = "MapStrategy(" +
  		include.map(toEString(_)).mkString("List(", ", ", ")") +
  		exclude.map(toEString(_)).mkString("List(", ", ", ")") +
  		", " + lhs.toString + ")"
  		
  override def hashCode = lhs.hashCode
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomSeq(props, atoms) =>
	    // All we can do is apply the lhs to each atom in the list.
	    (AtomSeq(props, atoms.map(Apply(lhs,_))), true)
	  case Apply(op, AtomSeq(props, atoms)) =>
      // We apply the lhs to each argument whose parameter meets the
      // label criteria.  This is modestly tricky.
      (Apply(op, AtomSeq(props, atoms.map(Apply(lhs,_)))), true)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
}

case class RMapStrategy(rhs: BasicAtom) extends BasicAtom with Rewriter {
	val theType = STRATEGY
	
	val isConstant = rhs.isConstant
	val isTerm = rhs.isTerm
	val deBruijnIndex = rhs.deBruijnIndex
	val depth = rhs.depth + 1
	val constantPool =
	  Some(BasicAtom.buildConstantPool(theType.hashCode, rhs))
	  
	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
	    hints: Option[Any]) =
  	subject match {
	  case RMapStrategy(orhs) => rhs.tryMatch(orhs, binds, hints)
	  case _ => Fail("Subject is not an rmap strategy.", this, subject)
	}
	  
	def rewrite(binds: Bindings) = rhs.rewrite(binds) match {
	  case (newrhs, true) => (RMapStrategy(newrhs), true)
	  case _ => (this, false)
	}
	
	def toParseString = "{ rmap " + rhs.toParseString + " }"
			
  override def toString = "RMapStrategy(" + rhs.toString + ")"
  		
  override def hashCode = rhs.hashCode
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomSeq(props, atoms) =>
	    // All we can do is apply the rhs to each atom in the list.
	    (AtomSeq(props, atoms.map(Apply(_,rhs))), true)
	  case Apply(op, AtomSeq(props, atoms)) =>
      // We apply the rhs to each argument whose parameter meets the
      // label criteria.  This is modestly tricky.
      (Apply(op, AtomSeq(props, atoms.map(Apply(_,rhs)))), true)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
}

/**
 * Easier construction and pattern matching of rewrite rules.
 */
object RewriteRule {
  val tag = Literal(Symbol("rule"))
  
  /**
   * Create a rewrite rule.  The rule is not synthetic.
   * 
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
   */
  def apply(pattern: BasicAtom, rewrite: BasicAtom, guards: Seq[BasicAtom],
      rulesets: Set[String]) = {
    val sfh = _makeSpecialFormHolder(pattern, rewrite, guards, rulesets)
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, false)
  }
  
  /**
   * Create a rewrite rule.
   * 
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
	 * @param synthetic		If true, this is a synthetic rule.
   */
  def apply(pattern: BasicAtom, rewrite: BasicAtom, guards: Seq[BasicAtom],
      rulesets: Set[String], synthetic: Boolean) = {
    val sfh = _makeSpecialFormHolder(pattern, rewrite, guards, rulesets)
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, synthetic)
  }
  
  private def _makeSpecialFormHolder(pattern: BasicAtom, rewrite: BasicAtom,
      guards: Seq[BasicAtom], rulesets: Set[String]) = {
    // Make the map pair.
    val mappair = MapPair(pattern, rewrite)
    // Make the binding.
    val binds:Bindings = Bindings() +
    	(""->mappair) +
    	("if"->AtomSeq(Associative(true) and Commutative(true), guards.toIndexedSeq[BasicAtom])) +
    	("rulesets"->
    		AtomSeq(Associative(true) and Commutative(true), rulesets.map {
    			str => Literal(Symbol(str))
    		}.toIndexedSeq[BasicAtom]))
    new SpecialFormHolder(tag, BindingsAtom(binds))
  }

  /**
   * Break a rewrite rule into its parts.  The synthetic flag is not returned.
   * 
   * @param rule	The rewrite rule.
   * @return	The pattern, rewrite, guards, and rulesets.
   */
  def unapply(rule: RewriteRule) = Some((rule.pattern, rule.rewrite,
      rule.guards, rule.rulesets))
      
  def apply(sfh: SpecialFormHolder): RewriteRule = {
    // A rewrite rule must be given with a binding.
    val bh = sfh.requireBindings
    // Check the content.
    bh.check(Map(""->true, "if"->false, "ruleset"->false, "rulesets"->false))
    // Get the map pair.
    val mappair = bh.fetchAs[AtomSeq]("")
    if (mappair.length < 1)
      throw new SpecialFormException(
          "Rewrite rule does not contain a map pair.")
    if (mappair.length > 1)
      throw new SpecialFormException(
          "Too many items at the top level of a rewrite rule; did you forget a marker?")
    val pair = mappair(0) match {
      case mp:MapPair => mp
      case x =>
        throw new SpecialFormException(
            "Top-level item in rewrite rule is not a map pair: " + x.toParseString)
    }
    // Get the guards.
    val guards = bh.fetchAs[AtomSeq]("if", Some(EmptySeq))
    // Get the rulesets.
    val rulesets = bh.fetchAs[AtomSeq]("rulesets", Some(EmptySeq)) map {
      rs => rs match {
        case SymbolLiteral(_, name) => name.name
        case _ =>
          throw new SpecialFormException(
              "Ruleset specification is not a symbol: " + rs.toParseString)
      }
    }
    // Build the rule.
    new RewriteRule(sfh, pair.left, pair.right, guards.atoms, rulesets.toSet)
  }
}

/**
 * Encapsulate a rewrite rule.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param sfh					The special form holder.
 * @param pattern			The pattern to match.
 * @param rewrite			The rewrite to apply on match.
 * @param guards			Guards that must be true to accept a match.
 * @param rulesets		The rulesets that contain this rule.
 * @param synthetic		If true, this is a synthetic rule.
 */
class RewriteRule private (
    sfh: SpecialFormHolder,
    val pattern: BasicAtom, val rewrite: BasicAtom,
    val guards: Seq[BasicAtom], val rulesets: Set[String],
    val synthetic: Boolean = false)
    extends SpecialForm(sfh.tag, sfh.content) with Rewriter {
  override val theType = STRATEGY
  
  /**
   * Attempt to apply this rule to a given atom.
   * 
   * To successfully apply a rule, the subject must match the pattern and
   * given the provided bindings (if any).  The complete set of bindings is
   * then used to rewrite every guard, and these are checked to see if they
   * are the literal true.  At present no further rewriting of guards is
   * performed.
   * 
   * If all guards are true, then the bindings are applied to the rewrite and
   * the result is returned.
   * 
   * @param subject	The subject to test.
   * @param binds		Bindings to honor.
   * @return	A pair consisting of an atom and a boolean.  The boolean is
   * 					true if the rewrite yielded a new atom, and is false otherwise.
   */
  def tryRewrite(subject: BasicAtom, binds: Bindings = Bindings()):
  (BasicAtom, Boolean) = {
    // Local function to check the guards.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- guards) {
        val (newguard, _) = guard.rewrite(candidate)
        if (!newguard.isTrue) return false
      }
      true
    }
    
    // Local function to perform the rewrite if the rule fires.  We return
    // true in the pair no matter what, since the rule fired.
    def doRuleRewrite(candidate: Bindings) = (rewrite.rewrite(candidate)._1, true)
    
    // First we try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds) match {
      case fail:Fail => return (subject, false)
      case Match(newbinds) =>
        // We got a match.  Check the guards.
        if (checkGuards(newbinds)) return doRuleRewrite(newbinds)
        else return (subject, false)
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          if (checkGuards(newbinds)) return doRuleRewrite(newbinds)
        }
        return (subject, false)
    }
  }
  	
  // To make a Scala parseable string we have to make the ruleset names into
  // parseable strings.
  override def toString = "RewriteRule(" +
  	pattern.toString + ", " +
  	rewrite.toString + ", " +
  	guards.toString + ", " +
  	rulesets.map(toEString(_)) + ")"
  	
  def doRewrite(atom: BasicAtom) = doRewrite(atom, Bindings())
  
  def doRewrite(atom: BasicAtom, binds: Bindings) = {
    // Try to apply the rewrite rule.  Whatever we get back is the result.
    //println("Rewriting with rule.")
    val result = tryRewrite(atom)
    (result._1, result._2)
  }
}
