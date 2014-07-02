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

class IUnifiable(object):
    pass

class Variable(IUnifiable):
    def __init__(self):
        # Use _isBound separate from _value so that it can be bound to any value,
        #   including None.
        self._isBound = False

    # If this Variable is unbound, then just return this Variable.
    # Otherwise, if this has been bound to a value with unify, return the value.
    # If the bound value is another Variable, this follows the "variable chain"
    # to the end and returns the final value, or the final Variable if it is unbound.
    # For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
    def getValue(self):
        if not self._isBound:
            return self

        result = self._value
        while isinstance(result, Variable):
            if not result._isBound:
                return result

            # Keep following the Variable chain.
            result = result._value

        return result

    # If this Variable is bound, then just call YP.unify to unify this with arg.
    # (Note that if arg is an unbound Variable, then YP.unify will bind it to
    # this Variable's value.)
    # Otherwise, bind this Variable to YP.getValue(arg) and yield once.  After the
    # yield, return this Variable to the unbound state.
    # For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
    def unify(self, arg):
        if not self._isBound:
            self._value = YP.getValue(arg)
            if self._value == self:
                # We are unifying this unbound variable with itself, so leave it unbound.
                yield False
            else:
                self._isBound = True
                try:
                    yield False
                finally:
                    # Remove the binding.
                    self._isBound = False
        else:
            for l1 in YP.unify(self, arg):
                yield False

    # If bound, call YP.addUniqueVariables on the value.  Otherwise, if this unbound
    # variable is not already in variableSet, add it.
    def addUniqueVariables(self, variableSet):
        if self._isBound:
            YP.addUniqueVariables(self.getValue(), variableSet)
        else:
            if variableSet.count(self) == 0:
                variableSet.append(self)

    def __str__(self):
        value = self.getValue()
        if value == self:
            return "_Variable"
        else:
            return value.__str__()

    # If bound, return YP.makeCopy for the value, else return copyStore.getCopy(this).
    # However, if copyStore is None, just return this.
    def makeCopy(self, copyStore):
        if self._isBound:
            return YP.makeCopy(self.getValue(), copyStore)
        else:
            if copyStore == None:
                return self
            else:
                return copyStore.getCopy(self)

    def termEqual(self, term):
        if self._isBound:
            return YP.termEqual(self.getValue(), term)
        else:
            return self == YP.getValue(term)

    def ground(self):
        if this._isBound:
            # This is usually called by YP.ground which already did getValue, so this
            # should never be reached, but check anyway.
            return YP.ground(self.getValue())
        else:
            return False

    # A CopyStore is used by makeCopy to track which Variable objects have
    # been copied.
    class CopyStore(object):
        def __init__(self):
            self._inVariableList = []
            self._outVariableList = []

        # If inVariable has already been copied, return its copy. Otherwise,
        # return a fresh Variable associated with inVariable.
        def getCopy(self, inVariable):
            if self._inVariableList.count(inVariable) > 0:
                return self._outVariableList[self._inVariableList.index(inVariable)]
            else:
                outVariable = Variable()
                self._inVariableList.append(inVariable)
                self._outVariableList.append(outVariable)
                return outVariable

        # Return the number of unique variables that have been copied.
        def getNUniqueVariables(self):
            return len(self._inVariableList)

