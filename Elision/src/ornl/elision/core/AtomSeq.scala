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

import scala.collection.IndexedSeq
import ornl.elision.util.OmitSeq
import ornl.elision.matcher.AMatcher
import ornl.elision.matcher.CMatcher
import ornl.elision.matcher.ACMatcher
import ornl.elision.matcher.SequenceMatcher
import ornl.elision.util.Loc

/**
 * An atom sequence is just that: a sequence of atoms.
 * 
 * == Properties ==
 * Atom sequences may have properties like operators.  That is, they may be
 * commutative, associative, idempotent, and may have absorbers and identities.
 * These properties have an effect on the list when it is constructed.  For
 * example, a list that is associative, commutative, and idempotent is
 * essentially a set.  If not idempotent, it is a multiset.  Properties are
 * specified with an algebraic properties object.
 * 
 * == Use ==
 * Create a list by specifying the properties and then providing a list of 
 * atoms to include in the list.  The atoms are specified using an instance
 * of an `IndexedSeq`.
 *
 * @param loc         The location of the atom's declaration. 
 * @param theType     The type of this sequence.
 * @param props		    The properties for this atom sequence.
 * @param orig_xatoms	The sequence of atoms in this sequence.  Note that,
 *                    depending on the specified properties, the stored
 *                    sequence may be different.
 */
class AtomSeq protected[elision] (
    loc: Loc,
    val theType: BasicAtom,
    val props: AlgProp,
    orig_xatoms: IndexedSeq[BasicAtom])
    extends BasicAtom(loc) with IndexedSeq[BasicAtom] {
  
  require(xatoms != null)
  require(props != null)
  
  /**
   * Determine whether we have to sort the atoms.  If we know the list is
   * commutative, then we have to sort it.
   */
  lazy private val xatoms =
    (if (props.isC(false)) orig_xatoms.sorted(BasicAtomComparator)
        else orig_xatoms)
  
  /**
   * Whether this sequence is specified to be associative.  Note that false here
   * just means the sequence was not marked as associative; it's associativity
   * may be unspecified.
   */
  lazy val associative = props.isA(false)
  
  /**
   * Whether this sequence is specified to be commutative.  Note that false here
   * just means the sequence was not marked as commutative; it's associativity
   * may be unspecified.
   */
  lazy val commutative = props.isC(false)
  
  /**
   * Whether this sequence is specified to be idempotent.  Note that false here
   * just means the sequence was not marked as idempotent; it's associativity
   * may be unspecified.
   */
  lazy val idempotent = props.isI(false)
  
  /**
   * The absorber for this sequence, if any.
   */
  lazy val absorber = props.absorber
  
  /**
   * The identity for this sequence, if any.
   */
  lazy val identity = props.identity
  
  /**
   * The atoms in this sequence.
   */
  val atoms = AtomSeq.process(props, xatoms)

  lazy val isConstant = atoms.forall(_.isConstant)
  lazy val isTerm = atoms.forall(_.isTerm)
  lazy val deBruijnIndex = atoms.foldLeft(0)(_ max _.deBruijnIndex)
  lazy val depth = atoms.foldLeft(0)(_ max _.depth) + 1
  
  /**
   * Get an element of this sequence by (zero-based) index.
   * 
   * @param idx	The index.
   * @return	The requested element.
   */
  def apply(idx: Int) = atoms(idx)
  
  /** The length of this sequence. */
  def length = atoms.length

  /**
   * Provide a "naked" version of the sequence, without the parens and property
   * indicators.
   * 
   * @return	The elements of the sequence, separated by commas.  Items internal
   * 					to the sequence may themselves be lists; that is okay, since the
   * 					parse string is used for those atoms.
   */
  lazy val toNakedString = atoms.mkParseString("", ", ", "")
  
  override lazy val hashCode = atoms.hashCode * 31 + props.hashCode
  lazy val otherHashCode = atoms.otherHashCode + 8191*props.otherHashCode

  /**
   * Two sequences are equal iff their properties and atoms are equal.
   * 
   * @param other   The other atom.
   * @return  True iff the other atom is equal to this one.
   */
  override def equals(other: Any) = {
    other match {
      case oseq: AtomSeq =>
        feq(oseq, this, (props == oseq.props) && (atoms == oseq.atoms))
        
      case _ => false
    }
  }
}

/**
 * Simplified construction and matching for atom sequences.
 */
object AtomSeq {
  
  /**
   * Match an atom sequence's parts.
   * 
   * @param seq	The sequence.
   * @return	The properties and the atoms.
   */
  def unapply(seq: AtomSeq) = Some(seq.props, seq.atoms)
  
  /**
   * Process the atoms and build the new sequence.  This reduces any included
   * associative sequences, and incidentally makes sure the result is an
   * `OmitSeq`.
   * 
   * This method is used during instance construction.
   * 
   * @param props	The properties.
   * @param atoms	The atoms.
   * @return	The possibly-new sequence.
   */
  private def process(props: AlgProp,
      xatoms: IndexedSeq[BasicAtom]): OmitSeq[BasicAtom] = {
    // If the list is associative, has an identity, or has an absorber, we
    // process it.  Idempotency is handled at the very end.
    val assoc = props.isA(false)
    val ident = props.identity.getOrElse(null)
    val absor = props.absorber.getOrElse(null)
    var atoms: OmitSeq[BasicAtom] = xatoms
    if (assoc || ident != null || absor != null) {
      var index = 0
      while (index < atoms.size) {
        val atom = atoms(index)
        if (absor == atom) {
          // Found the absorber.  It must be the only thing in the sequence.
          return OmitSeq[BasicAtom]() :+ atom
        }
        if (ident != atom) {
          if (assoc) atom match {
            case AtomSeq(oprops, args) if props == oprops =>
              // Add the arguments directly to this list.  We can assume this
              // list has already been processed, so no deeper checking is
              // needed.
              atoms = atoms.omit(index)
              atoms = atoms.insert(index, args)
            case _ =>
              // Nothing to do in this case.
          }          
        }
        index += 1
      } // Run through all arguments.
    }
    
    // Now handle idempotency.  If we change the sequence with idempotency,
    // then we replace the old sequence with the new one, since we don't need
    // to keep the old sequence around.  Othewise we leave as-is.
    if (props.isI(false)) {
      val testseq: OmitSeq[BasicAtom] = atoms.distinct
      if (testseq.length != atoms.length) {
        // Idempotency changed the sequence.  Replace the old one with the new
        // one.
        atoms = testseq
      }
    }
    
    // Done!
    return atoms
  }
}

/**
 * Improve matching of atom sequences as lists of atoms.
 * 
 * This is intended for use in matching.  The general form is:
 * {{{
 * args match {
 *   case Args(item1: Lambda, item2: Variable) => //...
 *   //...
 * }
 * }}}
 */
object Args {
  
  /**
   * Allow matching of an atom sequence as a sequence.
   * 
   * @param seq	The atom sequence.
   * @return	The children as a matchable sequence.
   */
  def unapplySeq(seq: AtomSeq) = Some(seq.atoms)
}
