/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
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
import ornl.elision.util.ElisionException
import scala.util.Properties
import scala.collection.mutable.{OpenHashMap => HashMap}
import ornl.elision.util.Console

/**
 * The cache contains the wrong type of item for the requested key.
 * 
 * @param msg		The human-readable message explaining the problem.
 */
class CacheException(msg: String) extends ElisionException(msg)

/**
 * Manage a collection of properties.
 */
trait PropertyManager {
  val _prop2val = HashMap[String,Any]()
  val _prop2desc = HashMap[String,String]()
  val _prop2close = HashMap[String,(PropertyManager => Unit)]()
  
  /**
   * Declare a property.
   * 
   * @param name        The property name.
   * @param description Human-readable description of the property.
   * @param default     The default value of the property.
   * @param onchange    A closure to execute when the property is changed
   *                    via the `setProperty` methods.  Can be omitted.
   *                    The property manager instance is passed, not the new
   *                    property value, as this provides access to all
   *                    properties.
   * @return  The default value.
   */
  def declareProperty[TYPE](name: String, description: String, default: TYPE,
      onchange: (PropertyManager => Unit) = null) {
    default match {
      case str: String =>
      case value: BigInt =>
      case value: Int =>
      case flag: Boolean =>
      case atom: BasicAtom =>
      case _ =>
        throw new CacheException("Unsupported data type for properties.  " +
        		"Properties must be strings, integers, or Booleans, and may " +
        		"be any BasicAtom, but got " +
        		Manifest.classType(name.getClass) + ".")
    }
    _prop2val(name) = default
    _prop2desc(name) = description
    _prop2close(name) = onchange
    default
  }
  
  /**
   * Get the value of a property.  Properties are intended to be used to
   * control various functions of the executor and modular extensions to
   * it, and not just for storing arbitrary data during execution.
   * 
   * Properties must be declared before they can be set or read.  See
   * `declareProperty`.  The type of the property may not be changed once
   * it has been declared.
   * 
   * When getting a property value, specify the `TYPE` so the returned value
   * is correctly typed.
   * 
   * @param key     The property name.
   * @param default The default value of the property.
   * @return  The requested property value.
   */
  def getProperty[TYPE](name: String)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toEString(name) + ".")
      case Some(item) =>
        item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Set the value of a property.  See the documentation of `getProperty` for
   * information about the use of properties.
   * 
   * @param key     The property name.
   * @param value   The property value.
   * @return  The new property value.
   */
  def setProperty[TYPE](name: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toEString(name) + ".")
      case Some(item) =>
        _prop2val(name) = value
        _prop2close(name) match {
          case null =>
          case x => x(this)
        }
        value
    }
  }
  
  /**
   * Get all declared properties.
   * 
   * @return  The returned value is a set of entries of the form (name,
   *          description, value).
   */
  def getProperties = {
    var list = List[(String, String, Any)]()
    for (name <- _prop2val.
        keySet.
        toList.
        sortWith(_.toLowerCase < _.toLowerCase).
        filter(!_.startsWith("_"))) {
      val description = _prop2desc(name)
      val value = _prop2val(name)
      list :+= ((name, description, value))
    } // Collect all properties.
    list
  }
  
  /**
   * Get all public property names, sorted in alphabetical order.
   * 
   * @return  The property names.
   */
  def getPropertyNames =
    _prop2val.
    keys.
    toList.
    sortWith(_.toLowerCase < _.toLowerCase).
    filter(!_.startsWith("_"))
    
  def writeProperties(console: Console, width: Int = 80) = {
    // Get the property names and compute the longest.
    val pwidth = getPropertyNames.foldLeft(0)(_ max _.length)
    
    // There must be dots between the property name and the description,
    // and there must be spaces around these.
    val remain = width - 5 - pwidth
    
    // Compute the width of the description.
    val dwidth = (if (remain >= 20) remain else (20 max (width-5)))
    
    // Now write the properties.
    val text = new ornl.elision.util.Text
    for ((name, description, value) <- getProperties) {
      // Format the description.
      val what = (if (value.isInstanceOf[BasicAtom])
          value.asInstanceOf[BasicAtom].toParseString
        else
          value.toString)
      text.add(description + "  ("+what+")")
      val lines = text.wrap(dwidth-2)
      text.clear
      
      // Write out the property name.
      console.send(name)
      
      // Write the remainder.
      if (remain >= 20) {
        var lead = " "+("."*(width-dwidth-name.length))+" "
        for (line <- lines) {
          console.sendln(lead + line)
          lead = " "+(" "*(width-dwidth))+" "
        } // Print all lines.
      } else {
        console.sendln("")
        var lead = " "+("."*(width-dwidth))+" "
        for (line <- lines) {
          console.sendln(lead + line)
          lead = " "+(" "*(width-dwidth))+" "
        } // Print all lines.
      }
    } // Print all properties.
  }
}

/**
 * An executor is a class that can convert a string into a sequence of atoms.
 * This can be done by parsing the usual Elision representation of atoms, or
 * by some other means.
 */
trait Executor extends PropertyManager {
  
  /** A parse result. */
  abstract sealed class ParseResult
  
  /**
   * The parse was successful.
   * 
   * @param nodes	The atoms parsed.
   */
  case class ParseSuccess(nodes: List[BasicAtom]) extends ParseResult
  
  /**
   * The parse failed.
   * 
   * @param err	The reason for the parsing failure.
   */
  case class ParseFailure(err: String) extends ParseResult
  
  /** Cache for use by native methods. */
  private val _cache = scala.collection.mutable.OpenHashMap[String,Any]()
  
  /**
   * Provide typed access to the content of the cache.  This is intended for
   * use by native operators.  To avoid conflicts, name your cache entries
   * starting with your operator name.  This causes a performance hit, so
   * in general avoid using the cache.  Find somewhere else to shove your
   * data!
   * 
   * If the key is present, but of the wrong type, an exception is thrown.
   * This is a `CacheException`.
   * 
   * @param key			The key for the item to retrieve.
   * @param default	The value to return if the specified key is not present.
   * 								If this is returned, it is also stored, so be sure to type
   * 								it correctly.
   * @return	The requested value, or the default value.
   */
  def fetchAs[TYPE](key: String, default: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    _cache.get(key) match {
      case None =>
        _cache(key) = default
        default
      case Some(item) =>
        if (mTYPE >:> Manifest.classType(key.getClass))
          throw new CacheException(
              "The cache entry for key " + toEString(key) +
              " is of the wrong type.  Expected " + mTYPE.toString +
              " but got " + Manifest.classType(key.getClass) + ".")
        else
          item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Stash a value in the cache for later lookup with `fetchAs`.  Read the
   * documentation for `fetchAs` before you use the cache!
   * 
   * @param key		The key.
   * @param value	The value.
   * @return The stored value.
   */
  def stash[TYPE](key: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _cache(key) = value
    value
  }
  
  /**
   * Get a console native handlers can use.
   */
  def console: Console
  
  /**
   * Get the context used by this executor instance.
   */
  def context: Context
  
  /**
   * Parse the given string and return a sequence of basic atoms.  The
   * sequence may be empty, and it may be lazily constructed.
   * 
   * If operators are present in the stream, and applied, any side effects
   * will have been executed by the time this method returns.
   * 
   * @param text		The text to parse.
   * @return	The sequence of atoms.
   */
  def parse(text: String): ParseResult
}
