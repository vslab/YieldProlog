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
from Variable import *
from Atom import *

class Functor1(IUnifiable):
    def __init__(self, name, arg1):
        if isinstance(name, Atom):
            self._name = name
        else:
            self._name = Atom.a(name)
        self._arg1 = arg1

    # If arg is another Functor1, then succeed (yield once) if this and arg have the
    # same name and the functor args unify, otherwise fail (don't yield).
    # If arg is a Variable, then call its unify to unify with this.
    # Otherwise fail (don't yield).
    def unify(self, arg):
        arg = YP.getValue(arg)
        if isinstance(arg, Functor1):
            if self._name.equals(arg._name):
                for l1 in YP.unify(self._arg1, arg._arg1):
                    yield False
        elif isinstance(arg, Variable):
            for l1 in arg.unify(self):
                yield False

    def __str__(self):
        return str(self._name) + "(" + str(YP.getValue(self._arg1)) + ")"

    def termEqual(self, term):
        term = YP.getValue(term)
        if isinstance(term, Functor1):
            return self._name.equals(term._name) and YP.termEqual(self._arg1, term._arg1)
        return False

    def lessThan(self, functor):
        # Do the equal check first since it is faster.
        if not self._name.equals(functor._name):
            return self._name.lessThan(functor._name)

        return YP.termLessThan(self._arg1, functor._arg1)

    def ground(self):
        return YP.ground(self._arg1)

    def addUniqueVariables(self, variableSet):
        YP.addUniqueVariables(self._arg1, variableSet)

    def makeCopy(self, copyStore):
        return Functor1(self._name, YP.makeCopy(self._arg1, copyStore))
