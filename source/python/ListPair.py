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
from Functor2 import *

class ListPair(Functor2):
    def __init__(self, head, tail):
        Functor2.__init__(self, Atom.DOT, head, tail)

    @staticmethod
    def make(arg1, arg2 = None, arg3 = None):
        if arg3 != None:
            return ListPair(arg1, ListPair(arg2, ListPair(arg3, Atom.NIL)))
        if arg2 != None:
            return ListPair(arg1, ListPair(arg2, Atom.NIL))

        if isinstance(arg1, list):
            if len(arg1) <= 0:
                return Atom.NIL

            result = Atom.NIL
            # Start from the end.
            for i in range(len(arg1) - 1, -1, -1):
                result = ListPair(arg1[i], result)
            return result
        else:
            return ListPair(arg1, Atom.NIL)

    # Return a ListPair version of array, where repeated elements (according to YP.termEqual) are removed.
    @staticmethod
    def makeWithoutRepeatedTerms(array):
        if len(array) <= 0:
            return Atom.NIL

        # Start from the end.
        previousTerm = array[len(array) - 1]
        result = ListPair(previousTerm, Atom.NIL)
        for i in range(len(array) - 2, -1, -1):
            term = array[i]
            if YP.termEqual(term, previousTerm):
                continue
            result = ListPair(term, result)
            previousTerm = term

        return result

    # Return an array of the elements in list or null if it is not
    # a proper list.  If list is Atom.NIL, return an array of zero elements.
    # If the list or one of the tails of the list is Variable, raise an instantiation_error.
    # This does not call YP.getValue on each element.
    @staticmethod
    def toArray(list):
        list = YP.getValue(list)
        if list == Atom.NIL:
            return []

        result = []
        element = list
        while True:
            if element == Atom.NIL:
                break
            if isinstance(element, Variable):
                raise PrologException(Atom.a("instantiation_error"), "List tail is an unbound variable")
            if not (isinstance(element, Functor2) and element._name == Atom.DOT):
                # Not a proper list.
                return None
            result.append(element._arg1)
            element = YP.getValue(element._arg2)

        if len(result) <= 0:
            return None
        return result
