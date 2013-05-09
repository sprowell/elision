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
package ornl.elision.core

import scala.collection.immutable.Map
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc

/**
 * Construction of a special form failed for the specified reason.
 * 
 * @param loc Location of the bad special form.
 * @param msg A human-readable message.
 */
class SpecialFormException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Methods for working with special form objects.
 */
object SpecialForm {
  
  /**
   * Extract the parts of the special form.
   * 
   * @param sf    The special form.
   * @return  The tag and content.
   */
  def unapply(sf: SpecialForm) = {
    Some((sf.tag, sf.content))
  }
  
  /**
   * Check the structure of the content of a special form.
   * 
   * The following requirements are checked.
   * 1. The content must be a binding.
   * 1. If string `s` is mapped to true in `structure` then `s` must be bound.
   * 1. If string `s` is mapped to false in `structure` then `s` may be bound.
   * 1. Nothing else may be bound except by the above rules.
   * 
   * If any of these rules are violated then a
   * [[ornl.elision.core.SpecialFormException]] is thrown.
   * 
   * @param tag       The tag, for error reporting.
   * @param content   The content of the special form.
   * @param structure The required structure of the content.
   */
  def check(tag: BasicAtom, content: BasicAtom,
      structure: Map[String,Boolean]) {
    // If we come here then the structure must be a binding.
    content match {
      case binds: BindingsAtom =>
        // First we make sure nothing is bound that should not be.
        binds.keys.find(! structure.contains(_)) match {
          case None =>
            
          case Some(badkey) =>
            throw new SpecialFormException(Loc.internal,
                "The special form with tag "+tag.toParseString+
                " does not allow the key "+toESymbol(badkey)+".")
        }
        
        // Next make sure that everything that is required is present.
        structure.keys.find(
            key => structure(key) && (! binds.contains(key))) match {
          case None =>
            
          case Some(badkey) =>
            throw new SpecialFormException(Loc.internal,
                "The special form with tag "+tag.toParseString+
                " requires the key "+toESymbol(badkey)+".")
        }
        
      case _ =>
        throw new SpecialFormException(Loc.internal,
            "The special form with tag "+tag.toParseString+
            " must have a binding as its content, but "+content.toParseString+
            " was found instead.")
    }
  }
}

/**
 * Encapsulate a special form.  This is the common base class for all special
 * forms.
 * 
 * A special form has two components: a _tag_ and some _content_.  In this
 * sense a special form is just a pair.  In general special forms are used
 * to capture other kinds of structures.
 * 
 * @param loc         The location of this atom's declaration.
 * @param tag         The tag.
 * @param thecontent  The content.  This is lazily evaluated to yield the
 *                    actual content atom.
 */
class SpecialForm(
    loc: Loc,
    val tag: BasicAtom,
    thecontent: => BasicAtom) extends BasicAtom(loc) {
  
  /** The content of this special form. */
  lazy val content = thecontent
  
  /** If the content is a binding, store it here. */
  private lazy val binding = {
    content match {
      case bind: BindingsAtom => Some(bind.mybinds)
      
      case _ => None
    }
  }

  /** All special forms use the type ANY as their type. */
  val theType: BasicAtom = ANY
  lazy val depth = (tag.depth max content.depth) + 1
  lazy val deBruijnIndex = tag.deBruijnIndex max content.deBruijnIndex
  lazy val isConstant = tag.isConstant && content.isConstant
  lazy val isTerm = tag.isTerm && content.isTerm

  override lazy val hashCode = tag.hashCode * 31 + content.hashCode
  lazy val otherHashCode = tag.otherHashCode + 8191*content.otherHashCode
  
  /**
   * Fetch the value for the specified key from the bindings.  If no default
   * value is specified (or is `None`), then the key is ''required'' to be
   * present.  If a default is specified, and the key is not present, then
   * the default is returned.  On error a `SpecialFormException` is thrown.
   * 
   * The value for the key is cast to the specified `TYPE`.  If the value is
   * not of the specified `TYPE`, then a `SpecialFormException` is thrown.
   * 
   * @param TYPE        Type of the atom to return.
   * @param key         The key.
   * @param default     Optional default value if the key is not present.
   * @return  The value of the key, cast to the correct type.
   */
  def fetchAs[TYPE](key: String, default: Option[TYPE] = None)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    if (binding.isEmpty) {
        throw new SpecialFormException(Loc.internal,
            "The special form with tag "+tag.toParseString+
            " must have a binding as its content, but "+content.toParseString+
            " was found instead.")
    }
    // Tricky!  Remember that the binding is an option, so first get its
    // content, and *then* get the key.
    binding.get.get(key) match {
      case None => default match {
        case Some(value) => value
        case None =>
          throw new SpecialFormException(loc,
              "Form " + tag.toParseString +
              " requires key " + toESymbol(key) + " but it was not given.")
      }
      case Some(item) =>
        if (mTYPE >:> Manifest.classType(key.getClass))
          throw new SpecialFormException(loc,
              "The value for key " + toESymbol(key) + " of form " +
              tag.toParseString + " is of the wrong type: " +
              item.toParseString + ". Expected " + mTYPE.toString +
              " but got " + Manifest.classType(key.getClass) + ".")
        else
          item.asInstanceOf[TYPE]
    }
  }
  
  override def equals(other: Any) = other match {
    case sf:SpecialForm =>
      feq(sf, this, tag == sf.tag && content == sf.content)
      
    case _ =>
      false
  }
}