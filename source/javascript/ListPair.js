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

function ListPair(head, tail) {
    Functor2.call(this, Atom.DOT, head, tail);
}

ListPair.prototype = new Functor2;

ListPair.make = function(arg1, arg2, arg3) {
    if (arg3 !== undefined)
		return new ListPair(arg1, new ListPair(arg2, new ListPair(arg3, Atom.NIL)));
    if (arg2 !== undefined)
		return new ListPair(arg1, new ListPair(arg2, Atom.NIL));
	
    if (arg1 instanceof Array) {
        if (arg1.length <= 0)
            return Atom.NIL;

        var result = Atom.NIL;
        // Start from the end.
        for (var i = arg1.length - 1; i >= 0; --i)
            result = new ListPair(arg1[i], result);
        return result;
    }
    else
  	  return new ListPair(arg1, Atom.NIL);
}

// Return a ListPair version of array, where repeated elements (according to YP.termEqual) are removed.
ListPair.makeWithoutRepeatedTerms = function(array) {
	if (array.length <= 0)
		return Atom.NIL;

	// Start from the end.
	var previousTerm = array[array.length - 1];
	var result = new ListPair(previousTerm, Atom.NIL);
	for (var i = array.length - 2; i >= 0; --i) {
		var term = array[i];
		if (YP.termEqual(term, previousTerm))
			continue;
		result = new ListPair(term, result);
        previousTerm = term;
	}
	return result;
}

// Return an array of the elements in list or null if it is not
// a proper list.  If list is Atom.NIL, return an array of zero elements.
// If the list or one of the tails of the list is Variable, raise an instantiation_error.
// This does not call YP.getValue on each element.
ListPair.toArray = function(list) {
    list = YP.getValue(list);
    if (list == Atom.NIL)
        return [];

    var result = [];
    var element = list;
    while(true) {
        if (element == Atom.NIL)
            break;
        if (element instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "List tail is an unbound variable");
        if (!((element instanceof Functor2) && element._name == Atom.DOT))
            // Not a proper list.
            return null;
        result.push(element._arg1);
        element = YP.getValue(element._arg2)
    }

    if (result.length <= 0)
        return null;
    return result;
}
