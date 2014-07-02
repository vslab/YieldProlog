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

function Functor2(name, arg1, arg2) {
	if (name instanceof Atom)
    	this._name = name;
	else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._arg1 = arg1;
    this._arg2 = arg2;
}

// If arg is another Functor2, then succeed (yield once) if this and arg have the
// same name and all functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor2.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor2)
    {
        if (this._name.equals(arg._name)) {
			for each (var l1 in YP.unify(this._arg1, arg._arg1)) {
				for each (var l1 in YP.unify(this._arg2, arg._arg2))
					yield false;
			}
		}
    }
    else if (arg instanceof Variable) {
        for each (var l1 in arg.unify(this))
			yield false;
	}
}

Functor2.prototype.toString = function() {
	if (this._name == Atom.DOT)
		return Functor2.listPairToString(this);
	else
    	return this._name + "(" + YP.getValue(this._arg1) + ", " + YP.getValue(this._arg2) + ")";
}

Functor2.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor2)
        return this._name.equals(term._name) && YP.termEqual(this._arg1, term._arg1) && 
		    YP.termEqual(this._arg2, term._arg2);
    return false;
}

Functor2.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    if (!YP.termEqual(this._arg1, functor._arg1))
        return YP.termLessThan(this._arg1, functor._arg1);

    return YP.termLessThan(this._arg2, functor._arg2);
}

Functor2.prototype.ground = function() {
    return YP.ground(this._arg1) && YP.ground(this._arg2);
}

Functor2.prototype.addUniqueVariables = function(variableSet) {
    YP.addUniqueVariables(this._arg1, variableSet);
    YP.addUniqueVariables(this._arg2, variableSet);
}

Functor2.prototype.makeCopy = function(copyStore) {
    return new Functor2(this._name, YP.makeCopy(this._arg1, copyStore), 
		YP.makeCopy(this._arg2, copyStore));
}

Functor2.listPairToString = function(listPair) {
    var result = "[";
    while (true) {
        var head = YP.getValue(listPair._arg1);
        var tail = YP.getValue(listPair._arg2);
        if (tail == Atom.NIL) {
            result += "" + head;
            break;
		}
        else if (tail instanceof Functor2 && tail._name == Atom.DOT) {
            result += head + ", ";
            listPair = tail;
            // Loop again.
		}
        else {
            // The list is not terminated with NIL.
            result += head + "|" + tail;
            break;
		}
    }
	
    result += "]";
    return result;
}

