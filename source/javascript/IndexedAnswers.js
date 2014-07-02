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

// An IndexedAnswers holds answers to a query based on the values of index arguments.
function IndexedAnswers(arity) {
    this._arity = arity;
    // addAnswer adds the answer here and indexes it later.
    this._allAnswers = [];
    // The key has the arity of answers with non-null values for each indexed arg.  The value
    //   is a list of the matching answers.  The signature is implicit in the pattern on non-null index args.
    this._indexedAnswers = new Object();
    // Keeps track of whether we have started adding entries to _indexedAnswers for the signature.
    this._gotAnswersForSignature = new Object();
}

IndexedAnswers.MAX_INDEX_ARGS = 31;
                                                                    
// Append the answer to the list and update the indexes, if any.
// Elements of answer must be ground, since arguments with unbound variables make this
// into a dynamic rule which we don't index.
IndexedAnswers.prototype.addAnswer = function(answer) {
    this.addOrPrependAnswer(answer, false);
}               

// Prepend the answer to the list and clear the indexes so that they must be re-computed
// on the next call to match.  (Only addAnswer will maintain the indexes while adding answers.)
// Elements of answer must be ground, since arguments with unbound variables make this
// into a dynamic rule which we don't index.
IndexedAnswers.prototype.prependAnswer = function(answer) {
    this.addOrPrependAnswer(answer, true);
}               
   
// Do the work of addAnswer or prependAnswer.
IndexedAnswers.prototype.addOrPrependAnswer = function(answer, prepend) {
    if (answer.length != this._arity)
        return;

    // Store a copy of the answer array.
    var copyStore = new Variable.CopyStore();
    var answerCopy = [YP.makeCopy(value, copyStore)
                        for each (value in answer)];
    if (copyStore.getNUniqueVariables() > 0)
        throw "Elements of answer must be ground, but found " + copyStore.getNUniqueVariables() +
              " unbound variables";

    if (prepend) {
        this._allAnswers.unshift(answerCopy);
        this.clearIndexes();
    }
    else {
        this._allAnswers.push(answerCopy);
        // If match has already indexed answers for a signature, we need to add
        //   this to the existing indexed answers.
        for(var signature in this._gotAnswersForSignature)
            this.indexAnswerForSignature(answerCopy, signature);
    }
}

IndexedAnswers.prototype.indexAnswerForSignature = function(answer, signature) {
    // First find out which of the answer values can be used as an index.
    var indexValues = [IndexedAnswers.getIndexValue(YP.getValue(value))
                        for each (value in answer)];
    // We limit the number of indexed args in a 32-bit signature.
    for (var i = IndexedAnswers.MAX_INDEX_ARGS; i < indexValues.length; ++i)
	    indexValues[i] = null;

    // We need an entry in indexArgs from indexValues for each 1 bit in signature.
    var indexArgs = [];
    for (var i = 0; i < indexValues.length; ++i) {
        if ((signature & (1 << i)) == 0)
            indexArgs.push(null);
        else {
            if (indexValues[i] == null)
                // The signature wants an index value here, but we don't have one so
                //   we can't add it as an answer for this signature.
                return;
            else
                indexArgs.push(indexValues[i]);
        }
    }

    this.add(indexArgs, answer);
}

// Assume indexArgs is an array.  Return the array of answers from _indexedAnswers
//   for the indexArgs, or undefined in not found.
IndexedAnswers.prototype.get = function(indexArgs) {
    // The key lookup converts indexArgs to a string and may clash with a different
	//   indexArgs that converts to the same string, we we actually store an array
	//   of objects that have {_key: indexArgs, _value: answers} and we have to
	//   look through this array to make sure we got the right indexArgs.
    var keyValueArray = this._indexedAnswers[indexArgs];
	if (keyValueArray === undefined)
	    return undefined;
	for each (var keyValue in keyValueArray) {
	    if (IndexedAnswers.arrayEquals(keyValue._key, indexArgs))
		    return keyValue._value;
    }
	
	return undefined;
}

// Add answer to _indexedAnswers for indexArgs, creating the entry if needed.
IndexedAnswers.prototype.add = function(indexArgs, answer) {
	var answers = this.get(indexArgs);
	if (answers === undefined) {
   	    answers = [];
        var keyValueArray = this._indexedAnswers[indexArgs];
		if (keyValueArray === undefined) {
		  keyValueArray = []
		  this._indexedAnswers[indexArgs] = keyValueArray;
		}
        keyValueArray.push({_key: indexArgs, _value: answers});
	}

    answers.push(answer);
}

IndexedAnswers.prototype.match = function(arguments) {
    if (arguments.length != this._arity)
        return;

    // Set up indexArgs, up to arg position MAX_INDEX_ARGS.  The signature has a 1 bit for
    //   each non-null index arg.
    var indexArgs = [];
    var gotAllIndexArgs = true;
    var signature = 0;
    for (var i = 0; i < arguments.length; ++i) {
        var indexValue = null;
        if (i < IndexedAnswers.MAX_INDEX_ARGS) {
            // We limit the number of args in a 32-bit signature.
            indexValue = IndexedAnswers.getIndexValue(YP.getValue(arguments[i]));
            if (indexValue != null)
                signature += (1 << i);
        }
        if (indexValue == null)
            gotAllIndexArgs = false;
        indexArgs.push(indexValue);
    }

	var answers;
    if (signature == 0)
        // No index args, so we have to match from _allAnswers.
        answers = this._allAnswers;
    else {
        if (this._gotAnswersForSignature[signature] === undefined) {
            // We need to create the entry in _indexedAnswers.
            for each (var answer in this._allAnswers)
                this.indexAnswerForSignature(answer, signature);
            // Mark that we did this signature.
            this._gotAnswersForSignature[signature] = null;
        }
	    answers = this.get(indexArgs);
    	if (answers === undefined)
            return;
    }

    if (gotAllIndexArgs) {
        // All the arguments were already bound, so we don't need to do bindings.
        yield false;
        return;
    }

    // Find matches in answers.
    var iterators = [];
    for each (var answer in answers) {
        var gotMatch = true;
        var nIterators = 0;
        // Try to bind all the arguments.
        for (var i = 0; i < arguments.length; ++i) {
            if (indexArgs[i] != null)
                // We already matched this argument by looking up _indexedAnswers.
                continue;

            var iterator = Iterator(YP.unify(arguments[i], answer[i]));
            iterators[nIterators++] = iterator;
            // next() returns if YP.unify succeeds.
            try {
                iterator.next();
    		}
            catch (e if e instanceof StopIteration) {
                gotMatch = false;
                break;
	    	}
        }

        try {
            if (gotMatch)
                yield false;
        }
        finally {
    	    // Manually finalize all the iterators.
    	    for (var i = 0; i < nIterators; ++i)
        	    iterators[i].close();
        }
    }
}

IndexedAnswers.prototype.clause = function(Head, Body) {
    Head = YP.getValue(Head);
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");
    var arguments = YP.getFunctorArgs(Head);

    // We always match Head from _allAnswers, and the Body is Atom.TRUE.
    for each (var l1 in YP.unify(Body, Atom.TRUE)) {
        // The caller can assert another answer into this same predicate during yield, so we have to
        //   make a copy of the answers.
        for each (var answer in [x for each (x in this._allAnswers)]) {
            for each (var l2 in YP.unifyArrays(arguments, answer))
                yield false;
        }
    }
}

IndexedAnswers.prototype.retract = function(Head, Body) {
    Head = YP.getValue(Head);
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");
    var arguments = YP.getFunctorArgs(Head);

    // We always match Head from _allAnswers, and the Body is Atom.TRUE.
    for each (var l1 in YP.unify(Body, Atom.TRUE)) {
        // The caller can assert another answer into this same predicate during yield, so we have to
        //   make a copy of the answers.
        for each (var answer in [x for each (x in this._allAnswers)]) {
            for each (var l2 in YP.unifyArrays(arguments, answer)) {
                // Remove answer.
                this._allAnswers.splice(this._allAnswers.indexOf(answer), 1);
                this.clearIndexes();
                yield false;
            }
        }
    }
}

// After retracting or prepending an answer in _allAnswers, the indexes are invalid, so clear them.
IndexedAnswers.prototype.clearIndexes = function()
{
    this._indexedAnswers = new Object();
    this._gotAnswersForSignature = new Object();
}

// Assume a1 and a2 are arrays.  Return true if both have the same
//   length and all elements are equal.
IndexedAnswers.arrayEquals = function(a1, a2) {
    if (a1.length != a2.length)
        return false;

    for (var i = 0; i < a1.length; ++i) {
        if (a1[i] != a2[i])
            return false;
	}
    return true;
}

// If we keep an index on value, return the value, or null if we don't index it.
IndexedAnswers.getIndexValue = function(value) {
    if (value instanceof Atom || typeof(value) == "string" /* || typeof(value) == "number" */)
        return value;
    else
        return null;
}

