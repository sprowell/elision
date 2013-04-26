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
package ornl.elision.matcher

import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.util.Debugger
import ornl.elision.core.ANY
import ornl.elision.core.AlgProp
import ornl.elision.core.Apply
import ornl.elision.util.OmitSeq
import ornl.elision.core.NamedRootType
import ornl.elision.core.RulesetRef
import ornl.elision.core.SpecialForm
import ornl.elision.core.Variable
import ornl.elision.core.Literal
import ornl.elision.core.TypeUniverse
import ornl.elision.core.NONE
import ornl.elision.core.wrapBindingsAtom
import ornl.elision.util.OmitSeq.fromIndexedSeq
import ornl.elision.core.AtomSeq
import ornl.elision.core.BindingsAtom
import ornl.elision.core.Lambda
import ornl.elision.core.MapPair
import ornl.elision.core.OperatorRef
import ornl.elision.core.Operator
import scala.compat.Platform

/**
 * Perform matching of two atoms, and return the result.
 */
object Matcher {
  
  /* Implementation Note
   * ===================
   * Timing out is handled in _tryMatchWithoutTypes, and in the specialized
   * matcher classes (SequenceMatcher, AMatcher, CMatcher, ACMatcher).  It
   * is not processed anywhere else in this object, as that should not be
   * needed.
   * 
   * Note that everything is dependency injected here; there is no mutable
   * or user-configurable data in this object.  Please keep it that way.
   * 
   * Finally, note that the interface consists of one public method.  Please
   * help keep the interface simple; don't pollute.
   */

  /**
   * Attempt to match the given subject against the given pattern, with the
   * provided bindings and the hints.
   * 
   * Matching can be limited by the use of `endtick`, that specifies the
   * maximum value for `Platform.currentTime`.  Once this value is exceeded
   * then matching terminates.
   * 
   * To allow two seconds for matching, use a value of
   * `Platform.currentTime + 2000` for `endtick`.  A value of `-1` can be
   * used for `endtick` to disable this and find all matches (if any).
   * 
   * @param pattern   The pattern to match.
   * @param subject   The subject.
   * @param binds     Bindings to honor on any match.
   * @param hints     Hints to the matcher.
   * @param endtick   Stop matching if the current time is later than this.
   * @return  The result of the match.
   */
  def apply(pattern: BasicAtom, subject: BasicAtom,
      binds: Bindings = Bindings(), hints: Option[Any] = None,
      endtick: Long = -1): Outcome = {
    // ANY and NONE are special cases.
    if (pattern == ANY) {
      return Match(binds)
    } else if (pattern == NONE) {
      return (if (subject == NONE || subject == ANY) Match(binds)
          else Fail("NONE only matches itself and ANY."))
    }
    
    var what = 0L
    Debugger("matching") {
      // We compute a hash code for this match.  This is used in the output to
      // associate lines referring to the match, since matches can be nested and
      // (potentially) interleaved.
      what = this.hashCode * 31 + subject.hashCode
    
      // The match attempt is starting.  Write out information about the
      // attempted match.
      Debugger.debugf("matching",
          "TRYING  (%x) in %s:\n", what, pattern.getClass.toString)
      Debugger("matching", "  pattern: "+pattern.toParseString+"\n  subject: "+
          subject.toParseString+"\n  with: "+binds.toParseString)
    }
        
    // Perform the match.
    val outcome = _doMatch(pattern, subject, binds, hints, endtick)
    Debugger("matching") {
      // Write out information about the result of the match attempt.
      outcome match {
        case fail:Fail =>
          Debugger.debugf("matching", "FAILURE (%x): ", what)
          Debugger("matching", fail)
        case Match(bnd) =>
          Debugger.debugf("matching", "SUCCESS (%x): ", what)
          Debugger("matching", bnd.toParseString)
        case many:Many =>
          Debugger.debugf("matching", "SUCCESS (%x): ", what)
          Debugger("matching", "  Many Matches")
      }
    }
    
    // The value is the outcome of the match.
    return outcome
  }

  /**
   * Recursively match the types.  This is unbounded recursion; it is expected
   * that a class (a type universe) will override this method to create a basis
   * case.
   *
   * NOTE: When strategies are finally implemented, this is where the selection
   * of type matching strategies may be done.
   *
   * @param pattern The pattern atom.
   * @param subject The subject atom.
   * @param binds   The bindings to observe.
   * @param hints   Optional hints.
   * @param endtick Time at which matching must stop.
   * @return  The outcome of the match.
   */
  private def _matchTypes(pattern: BasicAtom, subject: BasicAtom,
      binds: Bindings, hints: Option[Any], endtick: Long): Outcome = {
    // The type universe is a special case, and the basis case, for matching
    // types.
    if (pattern == TypeUniverse) {
      // Done.  Match without types.
      return _tryMatchWithoutTypes(pattern, subject, binds, hints, endtick)
    } else {
      apply(pattern.theType, subject.theType, binds, hints, endtick) match {
        case mat: Match => mat
        case many: Many => many
        case fail: Fail =>
          Fail("Types do not match.", pattern, subject, Some(fail))
      }
    }    
  }

  /**
   * Perform matching.
   * 
   * @param pattern The pattern atom.
   * @param subject The subject atom.
   * @param binds   The bindings to observe.
   * @param hints   Optional hints.
   * @param endtick The time at which to stop matching.
   * @return  The outcome of the match.
   */
  private def _doMatch(pattern: BasicAtom, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    if (subject == ANY && !pattern.isBindable) {
      // Any pattern is allowed to match the subject ANY.  In the matching
      // implementation for ANY, any subject is allowed to match ANY.
      // Thus ANY is a kind of wild card.  Note that no bindings are
      // applied - anything can match ANY.
      //
      // Of course, if this atom is bindable, we might want to bind to ANY,
      // so we exempt that case.
      Match(binds)
    } else if (pattern.depth > subject.depth) {
      // If this pattern has greater depth than the subject, reject immediately.
      Fail("Depth of pattern greater than depth of subject.", pattern, subject)
    } else if (pattern.isConstant && pattern == subject) {
      // Don't bother to try to match equal atoms that are constant.  The
      // constancy check is required; otherwise we might "match" $x against
      // $x, but not bind.  This leaves us free to bind $x to something
      // different later, invalidating the original "match".  Matching is
      // tricky.
      Match(binds)
    } else {
      // We didn't find a fast way to match, so we need to actually perform
      // the match.  First we try to match the types.  If this succeeds, then
      // we invoke the implementation of tryMatchWithoutTypes.
      _matchTypes(pattern, subject, binds, hints, endtick) match {
        case fail: Fail => 
          fail
          
        case mat: Match =>
          _tryMatchWithoutTypes(pattern, subject, mat.binds, hints, endtick)
          
        case Many(submatches) =>
          Many(MatchIterator(_tryMatchWithoutTypes(pattern, subject, _, hints,
              endtick), submatches))
      }
    }
  }

  /**
   * Try to match this atom, as a pattern, against the given subject.  Do not
   * do type matching for this atom, but use [[BasicAtom.tryMatch]] for any
   * children, so their types are correctly matched.
   *
   * @param pattern The pattern atom.
   * @param subject The subject atom.
   * @param binds   Any bindings that must be observed.
   * @param hints   Optional hints.
   * @param endtick The time at which matching must be stopped.
   * @return  The matching outcome.
   */
  private def _tryMatchWithoutTypes(pattern: BasicAtom, subject: BasicAtom,
      binds: Bindings, hints: Option[Any], endtick: Long): Outcome = {
    // Check for timeout now.  If we time out, we fail the match.
    if (endtick >= 0 && Platform.currentTime > endtick) {
      // Timeout!
      Fail("Timeout", pattern, subject)
    } else {
      pattern match {
        case patap: AlgProp => _apply(patap, subject, binds, hints, endtick)
        case patapply: Apply => _apply(patapply, subject, binds, hints, endtick)
        case patseq: AtomSeq => _apply(patseq, subject, binds, hints, endtick)
        case patbinds: BindingsAtom => _apply(patbinds, subject, binds, hints, endtick)
        case patlam: Lambda => _apply(patlam, subject, binds, hints, endtick)
        case nrt: NamedRootType => _apply(nrt, subject, binds, hints, endtick)
        case patlit: Literal[_] => _apply(patlit, subject, binds, hints, endtick)
        case patmap: MapPair => _apply(patmap, subject, binds, hints, endtick)
        case patrr: RulesetRef => _apply(patrr, subject, binds, hints, endtick)
        case patsf: SpecialForm => _apply(patsf, subject, binds, hints, endtick)
        case patvar: Variable => _apply(patvar, subject, binds, hints, endtick)
        case _ =>
          Fail("No matching method is defined for the pattern.", pattern, subject)
      }
    }
  }

  /**
   * Match two optional atoms against one another.  A match is really only
   * performed iff both are atoms.  If the pattern is unspecified, then the
   * match succeeds.  If the pattern is specified, but the subject is not, then
   * the pattern is matched against Nothing.  If both are specified, they are
   * matched as usual.
   * 
   * @param pat     The pattern.
   * @param sub     The subject.
   * @param binds   The bindings.
   * @param endtick The time at which matching must be stopped.
   * @return  The outcome.
   */
  private def _match(pat: Option[BasicAtom], sub: Option[BasicAtom],
      binds: Bindings, endtick: Long) = pat match {
    case None =>
      Match(binds)
      
    case Some(pattern) =>
      sub match {
        case None =>
          Matcher(pattern, ANY, binds, None, endtick)
          
        case Some(subject) =>
          Matcher(pattern, subject, binds, None, endtick)
    }
  }
  
  /**
   * Match two lists of options using `_match` for each, and return the
   * result.
   * 
   * @param plist   The pattern list.
   * @param slist   The subject list.
   * @param binds   Bindings to honor in any match.
   * @param endtick The time at which matching must be stopped.
   * @return  The outcome.
   */
  private def _matchAll(plist: List[Option[BasicAtom]],
      slist: List[Option[BasicAtom]], binds: Bindings, endtick: Long): Outcome =
    if (plist.length == 0) {
      Match(binds)
    } else {
      _match(plist.head, slist.head, binds, endtick) match {
        case fail: Fail =>
          fail
          
        case Match(newbinds) =>
          _matchAll(plist.tail, slist.tail, newbinds, endtick)
          
        case Many(iter) =>
          Many(iter ~> ((newbinds: Bindings) =>
            _matchAll(plist.tail, slist.tail, newbinds, endtick)))
      }
    }

  private def _apply(patap: AlgProp, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    patap match {
      case ap: AlgProp =>
        _matchAll(
          List(patap.associative, patap.commutative, patap.idempotent,
              patap.absorber, patap.identity),
          List(ap.associative, ap.commutative, ap.idempotent, ap.absorber,
              ap.identity),
          binds, endtick)
        
      case _ => Fail("Properties only match other properties.", patap, subject)
    }
  }
  
  private def _apply(patapply: Apply, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    subject match {
      case Apply(oop, oarg) =>
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        Matcher(patapply, oop, binds, Some(patapply.op), endtick) match {
          case fail: Fail =>
            Fail("Operators do not match.", patapply, subject, Some(fail))
            
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            patapply.arg match {
              case as:AtomSeq =>
                Matcher(as, oarg, newbinds, Some(patapply.op), endtick)
                
              case _ =>
                Matcher(patapply.arg, oarg, newbinds, Some(patapply.op),
                    endtick)
            }
          
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(MatchIterator(Matcher(patapply.arg, oarg, _,
                Some(patapply.op), endtick), matches))
        }
      
      case _ => Fail("Applications only match other applications.",
          patapply, subject)
    }
  }
  
  private def _apply(patseq: AtomSeq, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    // We only care if the hint is an operator.  We do this in two steps, since
    // the "obvious" way to do it doesn't work because of type erasure.  Boo!
    val operator = hints match {
      case Some(value) => value match {
        case oper: Operator => Some(OperatorRef(oper))
        case oper: OperatorRef => Some(oper)
        case _ => None
      }
      case _ => None
    }
      
    // Atom sequences only match other atom sequences.
    subject match {
      case as: AtomSeq =>
        // Local function to complete sequence matching by matching the actual
        // sequences using the appropriate matching algorithm based on the
        // properties.
        def doMatchSequences(usebinds: Bindings): Outcome = {
          // Now we have to decide how to compare the two sequences.  Note that
          // if the properties matching changes, this will like have to change,
          // too, to use the matched properties.
          if (patseq.associative) {
            if (patseq.commutative) {
              ACMatcher.tryMatch(patseq, as, usebinds, operator)
            } else {
              AMatcher.tryMatch(patseq, as, usebinds, operator)
            }
          } else {
            if (patseq.commutative) {
              CMatcher.tryMatch(patseq, as, usebinds)
            } else {
              SequenceMatcher.tryMatch(patseq, as, usebinds)
            }
          }
        }
      
        // Match properties.  This may alter the bindings.
        Matcher(patseq, as.props, binds, None, endtick) match {
          case fail: Fail =>
            Fail("Sequence properties do not match.", patseq, subject,
                Some(fail))
            
          case Match(newbinds) =>
            doMatchSequences(newbinds)
            
          case Many(iter) =>
            Outcome.convert(iter ~> (doMatchSequences _),
                Fail("Sequence properties do not match.", patseq, subject))
        }
      
      case _ =>
        Fail("An atom sequence may only match another atom sequence.",
            patseq, subject)
    }
  }
  
  private def _apply(patbinds: BindingsAtom, subject: BasicAtom,
      binds: Bindings, hints: Option[Any], endtick: Long) = {
    subject match {
      case BindingsAtom(obinds) =>
        // The bindings must bind the same variables.  Check that first.
        if (patbinds.mybinds.keySet != obinds.keySet) {
          Fail("Bindings bind different variables.", patbinds, subject)
        } else {
          // Now iterate over the keys.  The ordering does not matter.  This
          // creates two lists of atoms that we then match using the sequence
          // matcher.
          var mine = OmitSeq[BasicAtom]()
          var theirs = OmitSeq[BasicAtom]()
          for ((key, value) <- patbinds.mybinds) {
            mine :+= value
            theirs :+= obinds(key)
          } // Build lists of atoms.
          SequenceMatcher.tryMatch(mine, theirs, binds)
        }
        
      case _ =>
        Fail("Bindings can only match other bindings.", patbinds, subject)
    }
  }
  
  private def _apply(patlam: Lambda, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    subject match {
      case Lambda(olvar, obody) =>
        if (olvar == patlam.lvar) {
          Matcher(patlam.body, obody, binds, hints, endtick) match {
            case fail: Fail =>
              Fail("Lambda bodies do not match.", patlam, subject)
            case mat: Match => mat
            case mat: Many => mat
          }
        } else Fail("Lambda variables do not match.", patlam, subject)
      
      case _ => Fail("Lambdas only match other lambdas.", patlam, subject)
    }
  }
  
  private def _apply(nrt: NamedRootType, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    // A root type matches only itself.
    if (nrt == subject) Match(binds)
    else Fail("This type matches only itself.", nrt, subject)
  }
  
  private def _apply(patlit: Literal[_], subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    subject match {
      case lit: Literal[_] if patlit.value == lit.value => Match(binds)
      case _ => Fail("Literal pattern does not match subject.", patlit, subject)
    }
  }
  
  private def _apply(patmap: MapPair, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    subject match {
      case MapPair(oleft, oright) =>
        SequenceMatcher.tryMatch(Vector(patmap.left, patmap.right),
            Vector(oleft, oright), binds)
                                 
      case _ =>
        Fail("Subject of match is not a pair.", patmap, subject)
    }
  }
  
  private def _apply(pator: OperatorRef, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    if (subject == this) {
      Match(binds)
    } else subject match {
      case OperatorRef(oop) if (oop == pator.operator) =>
        Match(binds)
      
      case oop: Operator if (oop == pator.operator) =>
        Match(binds)
      
      case _ =>
        Fail("Operator reference does not match subject.", pator, subject)
    }
  }
  
  private def _apply(patrr: RulesetRef, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    if (subject == this) Match(binds) else subject match {
      case rr:RulesetRef if (rr.name == patrr.name) =>
        Match(binds)
        
      case _ =>
        Fail("Ruleset reference does not match subject.", patrr, subject)
    }
  }
  
  private def _apply(patsf: SpecialForm, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    subject match {
      case sf:SpecialForm =>
        Matcher(patsf.tag, sf.tag, Bindings(), None) match {
          case fail:Fail =>
            Fail("Tags do not match.", patsf.tag, sf.tag, Some(fail))
            
          case Match(newbinds) =>
            Matcher(patsf.content, sf.content, newbinds, hints, endtick)
            
          case Many(matches) =>
            Many(MatchIterator(Matcher(patsf.content, sf.content, _, hints,
                endtick), matches))
        }
        
      case _ =>
        Fail("Special forms match only special forms.", patsf, subject)
    }
  }
  
  private def _apply(patvar: Variable, subject: BasicAtom, binds: Bindings,
      hints: Option[Any], endtick: Long) = {
    // if the variable allows binding, and it is not already bound to a
    // different atom.  We also allow the variable to match ANY.
    if (patvar.isBindable) binds.get(patvar.name) match {
      case None =>
        // This is tricky.  We bind if we match against ANY.  Are
        // there unforseen consequences to this decision?  Otherwise we have
        // to add a binding of the variable name to the subject.
        patvar.bindMe(subject, binds)
      case Some(ANY) =>
        // We should re-bind this variable now.
        patvar.bindMe(subject, binds)
      case Some(atom) if subject == ANY || atom == subject =>
        // The variable is already bound, and it is bound to the subject, so
        // the match succeeds with the bindings as they are.
        Match(binds)
      case _ =>
        // The variable is already bound and it is bound to an unequal subject,
        // so the match fails.
        Fail("Variable " + patvar.toParseString +
          " is already bound to the term " +
          binds.get(patvar.name).get.toParseString + ".", patvar, subject)
    } else {
      // Variables that are not bindable cannot be bound, and cannot match
      // any subject.  This is to prevent allowing them to "match" a bindable
      // variable of the same name and type, and having chaos ensue.
      Fail("Variable is not bindable.", patvar, subject)
    }
  }
}
