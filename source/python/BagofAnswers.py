# Copyright (C) 2007-2008, Jeff Thompson
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright 
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright 
#       notice, this list of conditions and the following disclaimer in the 
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the copyright holder nor the names of its contributors 
#       may be used to endorse or promote products derived from this software 
#       without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

from YP import *
from ListPair import *
from Variable import *

# A BagofAnswers holds answers for bagof and setof.
class BagofAnswers(object):
    # To get the free variables, split off any existential qualifiers from Goal such as the X in 
    # "X ^ f(Y)", get the set of unbound variables in Goal that are not qualifiers, then remove
    # the unbound variables that are qualifiers as well as the unbound variables in Template.
    def __init__(self, Template, Goal):
        self._template = Template;

        # First get the set of variables that are not free variables.
        variableSet = []
        YP.addUniqueVariables(Template, variableSet)
        UnqualifiedGoal = YP.getValue(Goal)
        while isinstance(UnqualifiedGoal, Functor2) and UnqualifiedGoal._name == Atom.HAT:
            YP.addUniqueVariables(UnqualifiedGoal._arg1, variableSet)
            UnqualifiedGoal = YP.getValue(UnqualifiedGoal._arg2)

        # Remember how many non-free variables there are so we can find the unique free variables 
        #   that are added.
        nNonFreeVariables = len(variableSet)
        YP.addUniqueVariables(UnqualifiedGoal, variableSet)
        nFreeVariables = len(variableSet) - nNonFreeVariables
        if nFreeVariables == 0:
            # There were no free variables added, so we won't waste time with _bagForFreeVariables.
            self._freeVariables = None
            self._findallBagArray = []
        else:
            # Copy the free variables.
            self._freeVariables = nFreeVariables*[None]
            for i in range(nFreeVariables):
                self._freeVariables[i] = variableSet[i + nNonFreeVariables]

            self._bagForFreeVariables = []

    def add(self):
        if self._freeVariables == None:
            # The goal has bound the values in _template but we don't bother with _freeVariables.
            self._findallBagArray.append(YP.makeCopy(self._template, Variable.CopyStore()))
        else:
            # The goal has bound the values in _template and _freeVariables.
            # Copy the _freeVariables using YP.getValues so that we preserve them after unbinding.
            freeVariableValues = [YP.getValue(value) for value in self._freeVariables]
                    
            # Find the entry in _bagForFreeVariables for this set of _freeVariables values.
            # _bagForFreeVariables is an array of objects like { _freeVariables: [], _bagArray: [] }
            # Debug: Maybe we should implement an equality comparer and use a dictionary like in C#.
            bagArray = None
            for valuesAndBag in self._bagForFreeVariables:
                if BagofAnswers.arraysTermEqual(valuesAndBag["_freeVariables"], freeVariableValues):
                    bagArray = valuesAndBag["_bagArray"]
                    break
            if bagArray == None:
                # Didn't find the freeVariables, so create a new entry.
                bagArray = []
                self._bagForFreeVariables.append({ "_freeVariables": freeVariableValues, "_bagArray": bagArray })

            # Now copy the template and add to the bag for the freeVariables values.
            bagArray.append(YP.makeCopy(self._template, Variable.CopyStore()))

    # For each result, unify the _freeVariables and unify bagArrayVariable with the associated bag.
    # bagArrayVariable is unified with the array of matches for template that 
    # corresponds to the bindings for freeVariables.  Be very careful: this does not unify with a Prolog list.
    def resultArray(self, bagArrayVariable):
        if self._freeVariables == None:
            # No unbound free variables, so we only filled one bag.  If empty, bagof fails.
            if len(self._findallBagArray) > 0:
                for l1 in bagArrayVariable.unify(self._findallBagArray):
                    yield False
        else:
            for valuesAndBag in self._bagForFreeVariables:
                for l1 in YP.unifyArrays(self._freeVariables, valuesAndBag["_freeVariables"]):
                    for l2 in bagArrayVariable.unify(valuesAndBag["_bagArray"]):
                        yield False
                # Debug: Should we free memory of the answers already returned?

    # For each result, unify the _freeVariables and unify Bag with the associated bag.
    def result(self, Bag):
        bagArrayVariable = Variable()
        for l1 in self.resultArray(bagArrayVariable):
            for l2 in YP.unify(Bag, ListPair.make(bagArrayVariable.getValue())):
                yield False

    # For each result, unify the _freeVariables and unify Bag with the associated bag which is sorted
    # with duplicates removed, as in setof.
    def resultSet(self, Bag):
        bagArrayVariable = Variable()
        for l1 in self.resultArray(bagArrayVariable):
            bagArray = bagArrayVariable.getValue()
            YP.sortArray(bagArray)
            for l2 in YP.unify(Bag, ListPair.makeWithoutRepeatedTerms(bagArray)):
                yield False

    # Return true if all members of the arrays are YP.termEqual.
    @staticmethod
    def arraysTermEqual(array1, array2):
        if len(array1) != len(array2):
            return False
        for i in range(len(array1)):
            if not YP.termEqual(array1[i], array2[i]):
                return False
        return True
    
