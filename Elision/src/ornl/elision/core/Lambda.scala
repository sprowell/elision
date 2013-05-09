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
package ornl.elision.core

import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.matcher.Matcher
import ornl.elision.matcher.Match
import ornl.elision.matcher.Fail
import ornl.elision.matcher.Many

/* Notes on De Bruijn indices.
 * 
 * The De Bruijn index (DBI) is the number of binders in scope for a given
 * lambda variable.
 * 
 * \$x.\$y.\$z.($x.$z.($y.$z))
 *               3  1   2  1
 *               
 * We rewrite this in the De Bruijn notation as:
 * 
 * \.\.\.3 1 (2 1)
 * 
 * (This is the S combinator from SKI calculus.)
 * 
 * As another example, consider this atom.
 * 
 * \$x.\$y.$x
 *         2
 * 
 * \.\.2
 * 
 * (This is the K combinator from SKI calculus.)
 * 
 * Let's consider $S.$K.
 *     
 * \$x.\$y.\$z.($x.$z.($y.$z)).\$x.\$y.$x
 *     \$y.\$z.((\$x.\$y.$x).$z.($y.$z))
 *     \$y.\$z.(\$y.$z.($y.$z))
 *     \$y.\$z.($z)
 *     
 * And now $S.$K.$K.
 * 
 * $S.$K.$K
 * \$y.\$z.($z).(\$x.\$y.\$x)
 * \$z.$z
 * 
 * Thus we get the identity.
 */

/**
 * A lambda creates an operator that binds a single variable in a term.
 * 
 * To create an instance (or to match an instance) use the methods in the
 * companion object.
 * 
 * == Structure and Syntax ==
 * A lambda is indicated by a backslash (`\`) followed by the lambda variable,
 * a dot (`.`), and the lambda body.
 * {{{
 * \\$``x.7                -> Constant function
 * \\$``x.$``x               -> Identity function
 * \\$``x.add($``x,$``x)       -> Doubling function
 * }}}
 * In order to protect the lambda variable from rewriting or binding it is
 * converted to a De Bruijn index as described in the documentation for
 * [[ornl.elision.core.BasicAtom]] (see the field `deBruijnIndex`).
 * 
 * == Type ==
 * The type of a lambda is a mapping from the type of the lambda variable to
 * the type of the lambda body.  Of course either - or both - may be variables.
 * 
 * == Equality and Matching ==
 * Lambdas are equal iff their variables and bodies are equal ''after'' the
 * De Bruijn index substitution.  This means that the following two lambdas
 * are equal.
 * {{{
 * \\$``x.$``x
 * \\$``y.$``y
 * }}}
 * Both are rewritten to <code>\\$`:1`.$`:1`</code>.
 * 
 * @param loc         The location of the atom declaration.
 * @param given_lvar	The lambda variable which must match the De Bruijn
 * 										index.
 * @param given_body  The lambda body.
 */
class Lambda protected[elision] (
    loc: Loc,
    given_lvar: Variable,
    given_body: BasicAtom) extends BasicAtom(loc) with Applicable {
  // Correct the parts.
  val (lvar, body) = Lambda.adjust(given_lvar, given_body)
  
  /** The type is a mapping from the variable type to the body type. */
  lazy val theType = SymbolicOperator.MAP(lvar.theType, body.theType)
  
  /**
   * A lambda is constant iff its body is constant.  This is different from
   * saying that the lambda is itself constant.  The lambda `\\$``x.$``y`
   * is a constant, but its body contains a variable, so it is not constant in
   * this sense.
   */
  lazy val isConstant = body.isConstant
  
  /** The De Bruijn index is the max of the parameter and body. */
  lazy val deBruijnIndex = body.deBruijnIndex max lvar.deBruijnIndex
  
  /**
   * The lambda is a term iff its body is a term.  
   */
  lazy val isTerm = body.isTerm
  
  /**
   * Lambda depth is body depth plus one.
   */
  lazy val depth = body.depth + 1
  
  override lazy val hashCode = lvar.hashCode * 31 + body.hashCode
  lazy val otherHashCode = lvar.otherHashCode + 8191*body.otherHashCode
  
  override def equals(other: Any) = other match {
    case lambda:Lambda =>
      feq(lambda, this, lvar == lambda.lvar && body == lambda.body)
      
    case _ =>
      false
  }
}

/**
 * Companion object with convenient methods to create lambdas.
 */
object Lambda {
  /**
   * Control whether we are using De Bruijn indices.  This is `true` by
   * default, and you shoud probably '''leave it alone''' unless you are
   * doing something that involves debugging lambdas.  You aren't, so don't
   * modify this.
   */
  var useDeBruijnIndices = true
  
  /**
   * Break a lambda into its parameter and body.
   * 
   * @param lambda	The lambda to match.
   * @return	The variable and then body.
   */
  def unapply(lambda: Lambda) = Some(lambda.lvar, lambda.body)  
  
  /**
   * Convert a given lambda variable and body into a new variable and body
   * that use de Bruijn indices if those are enabled.  Otherwise return the
   * input variable and body.
   *
   * @param given_lvar	The given lambda parameter.
   * @param given_body	The given lambda body.
   * @return  The pair consisting of the corrected variable and body.
   */
  def adjust(given_lvar: Variable, given_body: BasicAtom) = {
    // Make and return the new lambda.
    if (useDeBruijnIndices) {
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
	    val (newbody, notfixed) = given_body.replace(map)
	    
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
}
