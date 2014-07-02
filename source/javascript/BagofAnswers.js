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

// A BagofAnswers holds answers for bagof and setof.

// To get the free variables, split off any existential qualifiers from Goal such as the X in 
// "X ^ f(Y)", get the set of unbound variables in Goal that are not qualifiers, then remove
// the unbound variables that are qualifiers as well as the unbound variables in Template.
function BagofAnswers(Template, Goal) {
    this._template = Template;

    // First get the set of variables that are not free variables.
    var variableSet = [];
    YP.addUniqueVariables(Template, variableSet);
    var UnqualifiedGoal = YP.getValue(Goal);
    while (UnqualifiedGoal instanceof Functor2 && UnqualifiedGoal._name == Atom.HAT) {
        YP.addUniqueVariables(UnqualifiedGoal._arg1, variableSet);
        UnqualifiedGoal = YP.getValue(UnqualifiedGoal._arg2);
    }

    // Remember how many non-free variables there are so we can find the unique free variables 
    //   that are added.
    var nNonFreeVariables = variableSet.length;
    YP.addUniqueVariables(UnqualifiedGoal, variableSet);
    var nFreeVariables = variableSet.length - nNonFreeVariables;
    if (nFreeVariables == 0) {
        // There were no free variables added, so we won't waste time with _bagForFreeVariables.
        this._freeVariables = null;
        this._findallBagArray = [];
    }
    else {
        // Copy the free variables.
        this._freeVariables = [];
        for (var i = 0; i < nFreeVariables; ++i)
            this._freeVariables[i] = variableSet[i + nNonFreeVariables];

        this._bagForFreeVariables = [];
    }
}

BagofAnswers.prototype.add = function() {
    if (this._freeVariables == null)
        // The goal has bound the values in _template but we don't bother with _freeVariables.
        this._findallBagArray.push(YP.makeCopy(this._template, new Variable.CopyStore()));
    else
    {
        // The goal has bound the values in _template and _freeVariables.
		// Copy the _freeVariables using YP.getValues so that we preserve them after unbinding.
        var freeVariableValues = [YP.getValue(value) for each (value in this._freeVariables)];
		
        // Find the entry in _bagForFreeVariables for this set of _freeVariables values.
		// _bagForFreeVariables is an array of objects like { _freeVariables: [], _bagArray: [] }
		// Debug: If Javascript develops support for a generalized Dictionary object, use it.
        var bagArray = null;
		for each (var valuesAndBag in this._bagForFreeVariables) {
		  if (BagofAnswers.arraysTermEqual(valuesAndBag._freeVariables, freeVariableValues)) {
		      bagArray = valuesAndBag._bagArray;
			  break;
		  }
		}
		if (bagArray == null) {
		    // Didn't find the freeVariables, so create a new entry.
            bagArray = [];
			this._bagForFreeVariables.push({ _freeVariables: freeVariableValues, _bagArray: bagArray });
        }

        // Now copy the template and add to the bag for the freeVariables values.
        bagArray.push(YP.makeCopy(this._template, new Variable.CopyStore()));
    }
}

// For each result, unify the _freeVariables and unify bagArrayVariable with the associated bag.
// bagArrayVariable is unified with the array of matches for template that 
// corresponds to the bindings for freeVariables.  Be very careful: this does not unify with a Prolog list.
BagofAnswers.prototype.resultArray = function(bagArrayVariable) {
    if (this._freeVariables == null) {
        // No unbound free variables, so we only filled one bag.  If empty, bagof fails.
        if (this._findallBagArray.length > 0) {
            for each (var l1 in bagArrayVariable.unify(this._findallBagArray))
                yield false;
        }
    }
    else {
		for each (var valuesAndBag in this._bagForFreeVariables) {
            for each (var l1 in YP.unifyArrays(this._freeVariables, valuesAndBag._freeVariables)) {
                for each (var l2 in bagArrayVariable.unify(valuesAndBag._bagArray))
                    yield false;
            }
            // Debug: Should we free memory of the answers already returned?
        }
    }
}

// For each result, unify the _freeVariables and unify Bag with the associated bag.
BagofAnswers.prototype.result = function(Bag) {
    var bagArrayVariable = new Variable();
    for each (var l1 in this.resultArray(bagArrayVariable)) {
        for each (var l2 in YP.unify(Bag, ListPair.make(bagArrayVariable.getValue())))
            yield false;
    }
}

// For each result, unify the _freeVariables and unify Bag with the associated bag which is sorted
// with duplicates removed, as in setof.
BagofAnswers.prototype.resultSet = function(Bag) {
    var bagArrayVariable = new Variable();
    for each (var l1 in this.resultArray(bagArrayVariable)) {
        var bagArray = bagArrayVariable.getValue();
        YP.sortArray(bagArray);
        for each (var l2 in YP.unify(Bag, ListPair.makeWithoutRepeatedTerms(bagArray)))
            yield false;
    }
}

// Return true if all members of the arrays are YP.termEqual.
BagofAnswers.arraysTermEqual = function(array1, array2) {
    if (array1.length != array2.length)
        return false;
    for (var i = 0; i < array1.length; ++i) {
        if (!YP.termEqual(array1[i], array2[i]))
            return false;
    }
    return true;
}

