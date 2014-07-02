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
from Atom import *
from Functor1 import *
from Functor2 import *
from Functor3 import *
from Variable import *

class Functor(IUnifiable):
    def __init__(self, name, args):
        if len(args) <= 3:
            if len(args) == 0:
                raise Exception("For arity 0 functor, just use name as an Atom")
            elif len(args) == 1:
                raise Exception("For arity 1 functor, use Functor1")
            elif len(args) == 2:
                raise Exception("For arity 2 functor, use Functor2")
            elif len(args) == 3:
                raise Exception("For arity 3 functor, use Functor3")
            else:
                # (This shouldn't happen, but include it for completeness.
                raise Exception("Cannot create a Functor of arity " + str(len(args)))

        if isinstance(name, Atom):
            self._name = name
        else:
            # Assume name is a string.
            self._name = Atom.a(name)
        self._args = args


    # Return an Atom, Functor1, Functor2, Functor3 or Functor depending on the
    # length of args.  
    # Note that this is different than the Functor constructor which requires
    # the length of args to be greater than 3.
    @staticmethod
    def make(name, args):
        if not isinstance(name, Atom):
            # Assume name is a string.
            name = Atom.a(name)

        if len(args) <= 0:
            return name
        elif len(args) == 1:
            return Functor1(name, args[0])
        elif len(args) == 2:
            return Functor2(name, args[0], args[1])
        elif len(args) == 3:
            return Functor3(name, args[0], args[1], args[2])
        else:
            return Functor(name, args)

    # If arg is another Functor, then succeed (yield once) if this and arg have the
    # same name and all functor args unify, otherwise fail (don't yield).
    # If arg is a Variable, then call its unify to unify with this.
    # Otherwise fail (don't yield).
    def unify(self, arg):
        arg = YP.getValue(arg)
        if isinstance(arg, Functor):
            if self._name.equals(arg._name):
                return YP.unifyArrays(self._args, arg._args)
            else:
                return YP.fail()
        elif isinstance(arg, Variable):
            return arg.unify(self)
        else:
            return YP.fail()

    def __str__(self):
        result = str(self._name) + "(" + str(YP.getValue(self._args[0]))
        for i in range(1, len(self._args)):
            result += ", " + str(YP.getValue(self._args[i]))
        result += ")"
        return result

    def termEqual(self, term):
        term = YP.getValue(term)
        if isinstance(term, Functor1):
            if self._name.equals(term._name) and len(self._args) == len(term._arg):
                for i in range(0, len(self._args)):
                    if not YP.termEqual(self._args[i], term._args[i]):
                        return False
                return True
        return False

    def lessThan(self, functor):
        # Do the equal check first since it is faster.
        if not self._name.equals(functor._name):
            return _name.lessThan(functor._name)

        if len(self._args) != len(functor._args):
            return len(self._args) < len(functor._args)

        for i in range(0, len(self._args)):
            if not YP.termEqual(self._args[i], functor._args[i]):
                return YP.termLessThan(self._args[i], functor._args[i])
        return False

    def ground(self):
        for i in range(0, len(self._args)):
            if not YP.ground(self._args[i]):
                return False
        return True

    def addUniqueVariables(self, variableSet):
        for i in range(0, len(self._args)):
            YP.addUniqueVariables(self._args[i], variableSet)

    def makeCopy(self, copyStore):
        argsCopy = [YP.makeCopy(arg, copyStore) for arg in self._args]
        return Functor(self._name, argsCopy)
