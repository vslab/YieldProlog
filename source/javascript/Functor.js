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

function Functor(name, args) {
    if (args.length < 3)
    {
        if (args.length == 0)
            throw "For arity 0 functor, just use name as an Atom";
        else if (args.length == 1)
            throw "For arity 1 functor, use Functor1";
        else if (args.length == 2)
            throw "For arity 2 functor, use Functor2";
        else if (args.length == 3)
            throw "For arity 3 functor, use Functor3";
        else
            // (This shouldn't happen, but include it for completeness.
            throw "Cannot create a Functor of arity " + args.length;
    }

    if (name instanceof Atom)
    	this._name = name;
    else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._args = args;
}

// Return an Atom, Functor1, Functor2, Functor3 or Functor depending on the
// length of args.  
// Note that this is different than the Functor constructor which requires
// the length of args to be greater than 3.
Functor.make = function(name, args) {
   if (!(name instanceof Atom))
	    // Assume name is a string.
	    name = Atom.a(name);

    if (args.length <= 0)
        return name;
    else if (args.length == 1)
        return new Functor1(name, args[0]);
    else if (args.length == 2)
        return new Functor2(name, args[0], args[1]);
    else if (args.length == 3)
        return new Functor3(name, args[0], args[1], args[2]);
    else
        return new Functor(name, args);
}

// If arg is another Functor, then succeed (yield once) if this and arg have the
// same name and all functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor)
    {
        if (this._name.equals(arg._name))
            return YP.unifyArrays(this._args, arg._args);
        else
            return YP.fail();
    }
    else if (arg instanceof Variable)
        return arg.unify(this);
    else
        return YP.fail();
}

Functor.prototype.toString = function() {
    var result = this._name + "(" + YP.getValue(this._args[0]);
    for (var i = 1; i < this._args.length; ++i)
        result += ", " + YP.getValue(this._args[i]);
    result += ")";
    return result;
}

Functor.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor) {
        if (this._name.equals(term._name) && this._args.length == term._args.length) {
            for (var i = 0; i < this._args.length; ++i)
            {
                if (!YP.termEqual(this._args[i], term._args[i]))
                    return false;
            }
            return true;
		}
	}
    return false;
}

Functor.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    if (this._args.length != functor._args.length)
        return this._args.length < functor._args.length;

    for (var i = 0; i < this._args.length; ++i) {
        if (!YP.termEqual(this._args[i], functor._args[i]))
            return YP.termLessThan(this._args[i], functor._args[i]);
    }

    return false;
}

Functor.prototype.ground = function() {
	for (var i = 0; i < this._args.length; ++i) {
		if (!YP.ground(this._args[i]))
            return false;
	}
	return true;
}

Functor.prototype.addUniqueVariables = function(variableSet) {
	for (var i = 0; i < this._args.length; ++i)
    	YP.addUniqueVariables(this._args[i], variableSet);
}

Functor.prototype.makeCopy = function(copyStore) {
	var argsCopy = [YP.makeCopy(arg, copyStore) for each (arg in this._args)];
    return new Functor(this._name, argsCopy);
}

