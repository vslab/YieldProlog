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

import StringIO
from YP import *
from Variable import *

class Atom(IUnifiable):
    # You should not call this constructor, but use Atom.a(name) instead.
    def __init__(self, name, module = None):
        self._name = name
        self._module = module

    # Return an Atom object with the name and module.  If module is null or Atom.NIL,
    # this behaves like Atom.a(name, None) and returns the unique object where the module is None.  
    # If module is None or Atom.NIL, return a unique Atom object.
    # If module is not None or Atom.NIL, this may or may not be the same object as another Atom
    # with the same name and module.
    # You should use this to create an Atom instead of calling the Atom constructor. 
    @staticmethod
    def a(name, module = None):
        if module == None or module == Atom.NIL:
            atom = Atom._atomStore.get(name, False)
            if atom == False:
                atom = Atom(name)
                Atom._atomStore[name] = atom
            return atom
        else:
            return Atom(name, module)

    # If Obj is an Atom unify its _module with Module.  If the Atom's _module is None, use Atom.NIL.
    @staticmethod
    def module(Obj, Module):
        Obj = YP.getValue(Obj)
        if isinstance(Obj, Atom):
            if Obj._module == None:
                return YP.unify(Module, Atom.NIL)
            else:
                return YP.unify(Module, Obj._module)

        return YP.fail()

    def unify(self, arg):
        arg = YP.getValue(arg)
        if isinstance(arg, Atom):
            if self.equals(arg):
                return YP.succeed()
            else:
                return YP.fail()
        elif isinstance(arg, Variable):
            return arg.unify(self)
        else:
            return YP.fail()

    def addUniqueVariables(self, variableSet):
        # Atom does not contain variables.
        pass

    def makeCopy(self, copyStore):
        # Atom does not contain variables that need to be copied.
        return self

    def termEqual(self, term):
        return self.equals(YP.getValue(term))

    def ground(self):
        # Atom is always ground.
        return True
                                                          
    def equals(self, obj):
        if isinstance(obj, Atom):
            if self._module == None and obj._module == None:
                # When _declaringClass is None, we always use an identical object from _atomStore.
                return self == obj
            # Otherwise, ignore _declaringClass and do a normal string compare on the _name.
            return self._name == obj._name

        return False

    def __str__(self):
        return self._name

    def __hash__(self):
        return hash(self._name)

    def toQuotedString(self):
        if len(self._name) == 0:
            return "''"
        elif self == Atom.NIL:
            return "[]"

        result = StringIO.StringIO()
        useQuotes = False
        for c in self._name:
            cInt = ord(c)
            if c == '\'':
                result.write("''")
                useQuotes = True
            elif c == '_' or cInt >= ord('a') and cInt <= ord('z') or \
                cInt >= ord('A') and cInt <= ord('Z') or cInt >= ord('0') and cInt <= ord('9'):
                    result.write(c)
            else:
                # Debug: Need to handle non-printable chars.
                result.write(c)
                useQuotes = True

        if not useQuotes and ord(self._name[0]) >= ord('a') and ord(self._name[0]) <= ord('z'):
            return result.getvalue()
        else:
            # Surround in single quotes.
            result.write('\'')
            return "'" + result.getvalue()

    # Return true if _name is lexicographically less than atom._name.
    def lessThan(self, atom):
        return self._name < atom._name

    _atomStore = {}

    # We will set these below after Atom is defined
    NIL = None
    DOT = None
    F = None
    SLASH = None
    HAT = None
    RULE = None
    TRUE = None

Atom.NIL = Atom.a("[]")
Atom.DOT = Atom.a(".")
Atom.F = Atom.a("f")
Atom.SLASH = Atom.a("/")
Atom.HAT = Atom.a("^")
Atom.RULE = Atom.a(":-")
Atom.TRUE = Atom.a("true")
