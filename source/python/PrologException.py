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

# A PrologException is used as the exception thrown by YP.throw(Term).
# One argument: Create a PrologException with the given arg1 term.  The printable exception message is the full Term.
# Two arguments: Create a PrologException where the Term is error(arg1, arg2). If arg2 is a string, this
#   converts it to an Atom so that Prolog code can use it.
#   This uses YP.makeCopy to copy the arguments so that they are valid after unbinding.
class PrologException(Exception):
    def __init__(self, arg1, arg2 = None):
        if arg2 != None:
            arg2 = YP.getValue(arg2);
            Exception.__init__(self, str(arg2))
            if isinstance(arg2, str):
                arg2 = Atom.a(arg2)            
            self._term = YP.makeCopy(Functor2(Atom.a("error"), arg1, arg2), Variable.CopyStore())
        else:
            Exception.__init__(self, str(YP.getValue(arg1)))
            self._term = YP.makeCopy(arg1, Variable.CopyStore())

    class TypeErrorInfo(object):
        def __init__(self, Type, Culprit, Message):
            self._Type = Type
            self._Culprit = Culprit
            self._Message = Message

    # Return the TypeErrorInfo for this exception, or None if _term does not match
    #   error(type_error(Type, Culprit), Message).
    def getTypeErrorInfo(self):
        if not (isinstance(self._term, Functor2) and self._term._name._name == "error"):
            return None
        errorTerm = self._term._arg1
        if not (isinstance(errorTerm, Functor2) and errorTerm._name._name == "type_error"):
            return None
        if not isinstance(errorTerm._arg1, Atom):
            return None
        return PrologException.TypeErrorInfo(errorTerm._arg1, errorTerm._arg2, self._term._arg2)

    class ExistenceErrorInfo(object):
        def __init__(self, Type, Culprit, Message):
            self._Type = Type
            self._Culprit = Culprit
            self._Message = Message

        # If _Type is procedure and _Culprit is name/artity, return the name.  Otherwise return null.
        def getProcedureName(self):
            if not (self._Type._name == "procedure" and \
                    isinstance(self._Culprit, Functor2) and self._Culprit._name == Atom.SLASH):
                return None
            return self._Culprit._arg1

        # If _Type is procedure and _Culprit is name/arity and arity is an integer, return the arity.  
        # Otherwise return -1.
        def getProcedureArity(self):
            if not (self._Type._name == "procedure" and \
                    isinstance(self._Culprit, Functor2) and self._Culprit._name == Atom.SLASH):
                return -1
            if not isinstance(self._Culprit._arg2, int):
                return -1
            return self._Culprit._arg2

    # Return the ExistenceErrorInfo for this exception, or None if _term does not match
    #   error(existence_error(Type, Culprit), Message).  If the returned ExistenceErrorInfo _Culprit is
    #   procedure, you can use its getProcedureName and getProcedureArity.
    def getExistenceErrorInfo(self):
        if not (isinstance(self._term, Functor2) and self._term._name._name == "error"):
            return None
        errorTerm = self._term._arg1
        if not (isinstance(errorTerm, Functor2) and errorTerm._name._name == "existence_error"):
            return None
        if not isinstance(errorTerm._arg1,  Atom):
            return None
        return PrologException.ExistenceErrorInfo(errorTerm._arg1, errorTerm._arg2, self._term._arg2)
