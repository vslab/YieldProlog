/*
 * Copyright (C) 2007-2008, Jeff Thompson
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright 
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright 
 *       notice, this list of conditions and the following disclaimer in the 
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its contributors 
 *       may be used to endorse or promote products derived from this software 
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// You should not call this constructor, but use Atom.a instead.
function Atom(name, module) {
    this._name = name;
    if (module == undefined)
      this._module = null;
    else
      this._module = module;
}

Atom._atomStore = new Object();
// Return an Atom object with the name and module.  If module is null or Atom.NIL,
// this behaves like Atom.a(name, undefined) and returns the unique object where the module is null.  
// If module is null or Atom.NIL, return a unique Atom object.
// If module is not null or Atom.NIL, this may or may not be the same object as another Atom
// with the same name and module.
// You should use this to create an Atom instead of calling the Atom constructor. 
Atom.a = function(name, module) {
   if (module == undefined || module == null || module == Atom.NIL) {
      var atom = Atom._atomStore[name];
      if (atom === undefined) {
	      atom = new Atom(name);
         Atom._atomStore[name] = atom;
	  }
	  return atom;
   }
   else
     return new Atom(name, module);
}
                                               
// If Obj is an Atom unify its _module with Module.  If the Atom's _module is null, use Atom.NIL.
Atom.module = function(Obj, Module) {
    Obj = YP.getValue(Obj);
    if (Obj instanceof Atom) {
        if (Obj._module == null)
            return YP.unify(Module, Atom.NIL);
        else
            return YP.unify(Module, Obj._module);
    }
    return YP.fail();
}
                                               
Atom.NIL = Atom.a("[]");
Atom.DOT = Atom.a(".");
Atom.F = Atom.a("f");
Atom.SLASH = Atom.a("/");
Atom.HAT = Atom.a("^");
Atom.RULE = Atom.a(":-");
Atom.TRUE = Atom.a("true");
                                                                  
Atom.prototype.unify = function(arg)
{
    arg = YP.getValue(arg);
    if (arg instanceof Atom)
        return this.equals(arg) ? YP.succeed() : YP.fail();
    else if (arg instanceof Variable)
        return arg.unify(this);
    else
        return YP.fail();
}

Atom.prototype.addUniqueVariables = function(variableSet)
{
    // Atom does not contain variables.
}

Atom.prototype.makeCopy = function(copyStore)
{
    // Atom does not contain variables that need to be copied.
    return this;
}

Atom.prototype.termEqual = function(term)
{
    return this.equals(YP.getValue(term));
}

Atom.prototype.ground = function()
{
    // Atom is always ground.
    return true;
}
                                                          
Atom.prototype.equals = function(obj) {
    if (obj instanceof Atom) {
        if (this._module == null && obj._module == null)
            // When _declaringClass is null, we always use an identical object from _atomStore.
            return this == obj;
        // Otherwise, ignore _declaringClass and do a normal string compare on the _name.
        return this._name == obj._name;
    }
    return false;
}                                                          
                                                          
Atom.prototype.toString = function() {
    return this._name;
}

Atom.prototype.toQuotedString = function() {
    if (this._name.length == 0)
        return "''";
    else if (this == Atom.NIL)
        return "[]";

    var result = new Array("");
    var useQuotes = false;
    for each (var c in this._name) {
        if (c == '\'') {
            result.push("''");
            useQuotes = true;
	}
        else if (c == '_' || c >= 'a' && c <= 'z' || 
				 c >= 'A' && c <= 'Z' || c >= '0' && c <= '9')
           	result.push(c);
        else {
            // Debug: Need to handle non-printable chars.
            result.push(c);
            useQuotes = true;
		}
	}

    if (!useQuotes && this._name[0] >= 'a' && this._name[0] <= 'z')
        return result.join("");
    else
        // Surround in single quotes.
        result.push('\'');
        return "'" + result.join("");
}


// Return true if _name is lexicographically less than atom._name.
Atom.prototype.lessThan = function(atom) {
    return this._name < atom._name;
}
