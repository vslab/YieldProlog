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

import sys
from datetime import datetime
import math

class YP(object):
    # If value is a Variable, then return its getValue.  Otherwise, just
    # return value.  You should call YP.getValue on any object that
    # may be a Variable to get the value to pass to other functions in
    # your system that are not part of Yield Prolog, such as math functions
    # or file I/O.
    # For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
    @staticmethod
    def getValue(value):
        if isinstance(value, Variable):
            return value.getValue()
        else:
            return value

    # If arg1 or arg2 is an object with a unify method (such as Variable or
    # Functor) then just call its unify with the other argument.  The object's
    # unify method will bind the values or check for equals as needed.
    # Otherwise, both arguments are "normal" (atomic) values so if they
    # are equal then succeed (yield once), else fail (don't yield).
    # For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
    @staticmethod
    def unify(arg1, arg2):
        arg1 = YP.getValue(arg1)
        arg2 = YP.getValue(arg2)
        if isinstance(arg1, IUnifiable):
            return arg1.unify(arg2)
        elif isinstance(arg2, IUnifiable):
            return arg2.unify(arg1)
        else:
            # Arguments are "normal" types.
            if arg1 == arg2:
                return YP.Succeed()
            else:
                return YP._fail
        
    # This is used for the lookup key in _predicatesStore.
    class NameArity(object):
        def __init__(self, name, arity):
            self._name = name
            self._arity = arity

        def __hash__(self):
            return hash(self._name) ^ self._arity

        def __eq__(self, obj):
            if not isinstance(obj, YP.NameArity):
                return False
            return (obj._name.equals(self._name) and obj._arity == self._arity)

        def __ne__(self, obj):
            return not self.__eq__(obj)

    # Convert term to an int.
    # If term is a single-element List, use its first element
    # (to handle the char types like "a").  If can't convert, raise an exception.
    @staticmethod
    def convertInt(term):
        term = YP.getValue(term)
        if isinstance(term, Functor2) and term._name == Atom.DOT and \
              YP.getValue(term._arg2) == Atom.NIL:
            # Assume it is a char type like "a".
            term = YP.getValue(term._arg1)
        if isinstance(term, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                    "Expected a number but the argument is an unbound variable")

        try:
            return int(term)
        except:
            raise PrologException \
                    (Functor2
                     ("type_error", Atom.a("evaluable"),
                      Functor2(Atom.SLASH, YP.getFunctorName(term), len(YP.getFunctorArgs(term)))),
                     "Term must be a float")

    # Convert term to a float.  This may convert an int to a float, etc.
    # If term is a single-element List, use its first element
    # (to handle the char types like "a").  If can't convert, raise an exception.
    @staticmethod
    def convertDouble(term):
        term = YP.getValue(term)
        if isinstance(term, Functor2) and term._name == Atom.DOT and \
              YP.getValue(term._arg2) == Atom.NIL:
            # Assume it is a char type like "a".
            term = YP.getValue(term._arg1)
        if isinstance(term, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                    "Expected a number but the argument is an unbound variable")

        try:
            return float(term)
        except:
            raise PrologException \
                    (Functor2
                     ("type_error", Atom.a("evaluable"),
                      Functor2(Atom.SLASH, YP.getFunctorName(term), len(YP.getFunctorArgs(term)))),
                     "Term must be a float")

    # If term is an integer, return it.
    # If term is a single-element List, use its first element
    # (to handle the char types like "a").  If can't convert, return None.
    @staticmethod
    def getInt(term):
        term = YP.getValue(term)
        if isinstance(term, Functor2) and term._name == Atom.DOT and \
              YP.getValue(term._arg2) == Atom.NIL:
            # Assume it is a char type like "a".
            term = YP.getValue(term._arg1)

        if isinstance(term, int):
            return term

        return None

    @staticmethod
    def equal(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x == YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) == YP.convertDouble(y)

    @staticmethod
    def notEqual(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x != YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) != YP.convertDouble(y)

    @staticmethod
    def greaterThan(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x > YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) > YP.convertDouble(y)

    @staticmethod
    def lessThan(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x < YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) < YP.convertDouble(y)

    @staticmethod
    def greaterThanOrEqual(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x >= YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) >= YP.convertDouble(y)

    @staticmethod
    def lessThanOrEqual(x, y):
        x = YP.getValue(x)
        if isinstance(x, datetime):
            return x <= YP.getValue(y)
        # Assume convertDouble converts an int to a double perfectly.
        return YP.convertDouble(x) <= YP.convertDouble(y)

    @staticmethod
    def negate(x):
        intX = YP.getInt(x)
        if intX != None:
            return -intX
        return -YP.convertDouble(x)

    @staticmethod
    def abs(x):
        intX = YP.getInt(x)
        if intX != None:
            return abs(intX)
        return abs(YP.convertDouble(x))

    @staticmethod
    def sign(x):
        intX = YP.getInt(x)
        if intX != None:
            if intX < 0:
                return -1
            elif intX > 0:
                return 1
            else:
                return 0

        num = YP.convertDouble(x)
        if num < 0:
            return -1
        elif num > 0:
            return 1
        else:
            return 0

    # Use toFloat instead of float because it is a reserved keyword.
    @staticmethod
    def toFloat(x):
        return YP.convertDouble(x)

    # The ISO standard returns an int.
    @staticmethod
    def floor(x):
        return int(math.floor(YP.convertDouble(x)))

    # The ISO standard returns an int.
    @staticmethod
    def truncate(x):
        num = YP.convertDouble(x)
        if num > 0:
            return math.floor(num)
        else:
            return math.ceil(num)

    # The ISO standard returns an int.
    @staticmethod
    def round(x):
        return int(round(YP.convertDouble(x)))

    # The ISO standard returns an int.
    @staticmethod
    def ceiling(x):
        return int(math.ceil(YP.convertDouble(x)))

    @staticmethod
    def sin(x):
        return math.sin(YP.convertDouble(x))

    @staticmethod
    def cos(x):
        return math.cos(YP.convertDouble(x))

    @staticmethod
    def atan(x):
        return math.atan(YP.convertDouble(x))

    @staticmethod
    def exp(x):
        return math.exp(YP.convertDouble(x))

    @staticmethod
    def log(x):
        return math.log(YP.convertDouble(x))

    @staticmethod
    def sqrt(x):
        return math.sqrt(YP.convertDouble(x))

    @staticmethod
    def bitwiseComplement(x):
        return ~YP.convertInt(x)

    @staticmethod
    def add(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return intX + intY
        return YP.convertDouble(x) + YP.convertDouble(y)

    @staticmethod
    def subtract(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return intX - intY
        return YP.convertDouble(x) - YP.convertDouble(y)

    @staticmethod
    def multiply(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return intX * intY
        return YP.convertDouble(x) * YP.convertDouble(y)

    # Return floating point, even if both arguments are integer.
    @staticmethod
    def divide(x, y):
        return YP.convertDouble(x) / YP.convertDouble(y)

    @staticmethod
    def intDivide(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return intX / intY
        # Still allow passing a double, but treat as an int.
        return int(YP.convertDouble(x)) / int(YP.convertDouble(y))

    @staticmethod
    def mod(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return intX % intY
        # Still allow passing a double, but treat as an int.
        return int(YP.convertDouble(x)) % int(YP.convertDouble(y))

    @staticmethod
    def pow(x, y):
        return math.pow(YP.convertDouble(x), YP.convertDouble(y))

    @staticmethod
    def bitwiseShiftRight(x, y):
        return YP.convertInt(x) >> YP.convertInt(y)

    @staticmethod
    def bitwiseShiftLeft(x, y):
        return YP.convertInt(x) << YP.convertInt(y)

    @staticmethod
    def bitwiseAnd(x, y):
        return YP.convertInt(x) & YP.convertInt(y)

    @staticmethod
    def bitwiseOr(x, y):
        return YP.convertInt(x) | YP.convertInt(y)

    @staticmethod
    def min(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return min(intX, intY)
        return min(convertDouble(x), convertDouble(y))

    @staticmethod
    def max(x, y):
        intX = YP.getInt(x)
        intY = YP.getInt(y)
        if intX != None and intY != None:
            return max(intX, intY)
        return max(convertDouble(x), convertDouble(y))

    @staticmethod
    def copy_term(inTerm, outTerm):
        return YP.unify(outTerm, YP.makeCopy(inTerm, Variable.CopyStore()))

    @staticmethod
    def addUniqueVariables(term, variableSet):
        term = YP.getValue(term)
        if isinstance(term, IUnifiable):
            term.addUniqueVariables(variableSet)

    @staticmethod
    def makeCopy(term, copyStore):
        term = YP.getValue(term)
        if isinstance(term, IUnifiable):
            return term.makeCopy(copyStore)
        else:
            # term is a "normal" type. Assume it is ground.
            return term

    # Sort the array in place according to termLessThan.  This does not remove duplicates.
    @staticmethod
    def sortArray(array):
        array.sort(YP.compareTerms)

    # Sort List according to termLessThan, remove duplicates and unify with Sorted.
    @staticmethod
    def sort(List, Sorted):
        array = ListPair.toArray(List)
        if array == None:
            return YP.fail()
        if len(array) > 1:
            YP.sortArray(array)
        return YP.unify(Sorted, ListPair.makeWithoutRepeatedTerms(array))

    # Use YP.unify to unify each of the elements of the two arrays, and yield
    # once if they all unify.
    @staticmethod
    def unifyArrays(array1, array2):
        if (len(array1) != len(array2)):
            return
        
        iterators = len(array1)*[None]
        gotMatch = True
        nIterators = 0
        # Try to bind all the arguments.
        for i in range(len(array1)):
            iterator = iter(YP.unify(array1[i], array2[i]))
            iterators[nIterators] = iterator
            nIterators += 1
            # next() returns if YP.unify succeeds.
            try:
                iterator.next()
            except StopIteration:
                gotMatch = False
                break

        try:
            if gotMatch:
                yield False
        finally:
            # Manually finalize all the iterators.
            for i in range(nIterators):
                iterators[i].close()

    # Return an iterator (which you can use in a for-in loop) which does
    # zero iterations.  This returns a pre-existing iterator which is
    # more efficient than letting the compiler generate a new one.
    @staticmethod
    def fail():
        return YP._fail

    # Return an iterator (which you can use in a for-in loop) which does
    # one iteration.  This returns a pre-existing iterator which is
    # more efficient than letting the compiler generate a new one.
    @staticmethod
    def succeed():
        return YP.Succeed()

    # Return an iterator (which you can use in a for-in loop) which repeats
    # indefinitely.  This returns a pre-existing iterator which is
    # more efficient than letting the compiler generate a new one.
    @staticmethod
    def repeat():
        return YP._repeat

    @staticmethod
    def univ(Term, List):        
        Term = YP.getValue(Term)
        List = YP.getValue(List)

        if YP.nonvar(Term):
            return YP.unify(ListPair \
                (YP.getFunctorName(Term), ListPair.make(YP.getFunctorArgs(Term))), List)
                            
        Name = Variable()
        ArgList = Variable()
        for l1 in ListPair(Name, ArgList).unify(List):
            args = ListPair.toArray(ArgList)
            if args == None:
                raise PrologException \
                    (Functor2("type_error", Atom.a("list"), ArgList), \
                     "Expected a list. Got: " + str(ArgList.getValue()))
            if len(args) == 0:
                # Return the Name, even if it is not an Atom.
                return YP.unify(Term, Name)
            if len(args) > YP.MAX_ARITY:
                raise PrologException \
                    (Functor1("representation_error", Atom.a("max_arity")), \
                     "Functor arity " + str(len(args)) + " may not be greater than " + str(YP.MAX_ARITY))

            if not YP.atom(Name):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), Name), \
                     "Expected an atom. Got: " + str(Name.getValue()))

            return YP.unify(Term, Functor.make(YP.getValue(Name), args))	

        return YP.fail()

    @staticmethod
    def functor(Term, FunctorName, Arity):
        Term = YP.getValue(Term)
        FunctorName = YP.getValue(FunctorName)
        Arity = YP.getValue(Arity)

        if isinstance(Term, Variable):
            if isinstance(FunctorName, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 2 FunctorName is an unbound variable")
            if isinstance(Arity, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 3 Arity is an unbound variable")
            if not isinstance(Arity, int):
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), Arity), "Arity is not an integer")
            if not YP.atomic(FunctorName):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atomic"), FunctorName), "FunctorName is not atomic")

            if Arity < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), Arity), \
                               "Arity may not be less than zero")
            elif Arity == 0:
                # Just unify Term with the atomic FunctorName.
                for l1 in YP.unify(Term, FunctorName):
                    yield False
            else:
                if Arity > YP.MAX_ARITY:
                    raise PrologException \
                        (Functor1("representation_error", Atom.a("max_arity")), \
                         "Functor arity " + str(Arity) + " may not be greater than " + str(YP.MAX_ARITY))
                if not isinstance(FunctorName, Atom):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("atom"), FunctorName), "FunctorName is not an atom")
                # Construct a functor with unbound variables.
                args = [Variable() for x in range(Arity)]
                for l1 in YP.unify(Term, Functor.make(FunctorName, args)):
                    yield False
        else:
            for l1 in YP.unify(FunctorName, YP.getFunctorName(Term)):
                for l2 in YP.unify(Arity, len(YP.getFunctorArgs(Term))):
                    yield False

    @staticmethod
    def arg(ArgNumber, Term, Value):
        if YP.var(ArgNumber):
            raise PrologException(Atom.a("instantiation_error"), "Arg 1 ArgNumber is an unbound variable")
        argNumberInt = YP.getInt(ArgNumber)
        if argNumberInt == None:
            raise PrologException \
                (Functor2("type_error", Atom.a("integer"), ArgNumber), "Arg 1 ArgNumber must be integer")
        if argNumberInt < 0:
            raise PrologException \
                (Functor2("domain_error", Atom.a("not_less_than_zero"), argNumberInt), \
                "ArgNumber may not be less than zero") 
                
        if YP.var(Term):
            raise PrologException(Atom.a("instantiation_error"), \
                "Arg 2 Term is an unbound variable")
        if not YP.compound(Term):
            raise PrologException \
                (Functor2("type_error", Atom.a("compound"), Term), "Arg 2 Term must be compound")
                
        termArgs = YP.getFunctorArgs(Term)
        # Silently fail if argNumberInt is out of range.
        if argNumberInt >= 1 and argNumberInt <= len(termArgs):
            # The first ArgNumber is at 1, not 0.
            for l1 in YP.unify(Value, termArgs[argNumberInt - 1]):
                yield False

    @staticmethod
    def termEqual(Term1, Term2):
        Term1 = YP.getValue(Term1)
        if isinstance(Term1, IUnifiable):
            return Term1.termEqual(Term2)
        return Term1 == YP.getValue(Term2)

    @staticmethod
    def termNotEqual(Term1, Term2):
        return not YP.termEqual(Term1, Term2)

    @staticmethod
    def termLessThan(Term1, Term2):
        Term1 = YP.getValue(Term1)
        Term2 = YP.getValue(Term2)
        term1TypeCode = YP.getTypeCode(Term1)
        term2TypeCode = YP.getTypeCode(Term2)
        if term1TypeCode != term2TypeCode:
            return term1TypeCode < term2TypeCode

        # The terms are the same type code.
        if term1TypeCode == -2:
            # Variable.
            # We always check for equality first because we want to be sure 
            #   that less than returns False if the terms are equal, in 
            #   case that the less than check really behaves like less than or equal.
            if Term1 != Term2:
                # The hash code should be unique to a Variable object.
                return hash(Term1) < hash(Term2)

            return False

        if term1TypeCode == 0:
            return Term1._name < Term2._name
        if term1TypeCode == 1:
            return Term1.lessThan(Term2)
        if term1TypeCode == 2:
            return Term1.lessThan(Term2)
        if term1TypeCode == 3:
            return Term1.lessThan(Term2)
        if term1TypeCode == 4:
            return Term1.lessThan(Term2)

        # Type code is -1 for general objects.  First compare their type names.
        # Note that this puts float before int as required by ISO Prolog.
        term1TypeName = type(Term1).__name__
        term2TypeName = type(Term2).__name__
        if term1TypeName != term2TypeName:
            return term1TypeName < term2TypeName

        # The terms are the same type name.
        if isinstance(Term1, int):
            return Term1 < Term2
        elif isinstance(Term1, float):
            return Term1 < Term2
        elif isinstance(Term1, datetime):
            return Term1 < Term2
        elif isinstance(Term1, str):
            return Term1 < Term2
        # Debug: Should we try arrays, etc.?

        if Term1 != Term2:
            # Could be equal or greater than.
            return hash(Term1) < hash(Term2)
        return False

    # Type code is -2 if term is a Variable, 0 if it is an Atom, 
    # 1 if it is a Functor1, 2 if it is a Functor2, 3 if it is a Functor3, 
    # 4 if it is Functor.
    # Otherwise, type code is -1.
    # This does not call YP.getValue(term).
    @staticmethod
    def getTypeCode(term):
        if isinstance(term, Variable):
            return -2
        elif isinstance(term, Atom):
            return 0
        elif isinstance(term, Functor1):
            return 1
        elif isinstance(term, Functor2):
            return 2
        elif isinstance(term, Functor3):
            return 3
        elif isinstance(term, Functor):
            return 4
        else:
            return -1

    @staticmethod
    def termLessThanOrEqual(Term1, Term2):
        if YP.termEqual(Term1, Term2):
            return True
        return YP.termLessThan(Term1, Term2)

    @staticmethod
    def termGreaterThan(Term1, Term2):
        return not YP.termLessThanOrEqual(Term1, Term2)

    @staticmethod
    def termGreaterThanOrEqual(Term1, Term2):
        # termLessThan should ensure that it returns False if terms are equal,
        #   so that this would return true.
        return not YP.termLessThan(Term1, Term2)

    @staticmethod
    def compareTerms(Term1, Term2):
        if YP.termEqual(Term1, Term2):
            return 0
        elif YP.termLessThan(Term1, Term2):
            return -1
        else:
            return 1

    @staticmethod
    def ground(Term):
        Term = YP.getValue(Term)
        if isinstance(Term, IUnifiable):
            return Term.ground()
        return True

    _operatorTable = None
    
    @staticmethod
    def current_op(Priority, Specifier, Operator):
        if YP._operatorTable == None:
            # Initialize.
            YP._operatorTable = IndexedAnswers(3)
            YP._operatorTable.addAnswer([1200, Atom.a("xfx"), Atom.a(":-")])
            YP._operatorTable.addAnswer([1200, Atom.a("xfx"), Atom.a("-->")])
            YP._operatorTable.addAnswer([1200, Atom.a("fx"), Atom.a(":-")])
            YP._operatorTable.addAnswer([1200, Atom.a("fx"), Atom.a("?-")])
            YP._operatorTable.addAnswer([1100, Atom.a("xfy"), Atom.a(";")])
            YP._operatorTable.addAnswer([1050, Atom.a("xfy"), Atom.a("->")])
            YP._operatorTable.addAnswer([1000, Atom.a("xfy"), Atom.a(",")])
            YP._operatorTable.addAnswer([900, Atom.a("fy"), Atom.a("\\+")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("\\=")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("==")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("\\==")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@<")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@=<")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@>")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@>=")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=..")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("is")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=:=")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=\\=")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("<")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=<")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a(">")])
            YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a(">=")])
            YP._operatorTable.addAnswer([600, Atom.a("xfy"), Atom.a(":")])
            YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("+")])
            YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("-")])
            YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("/\\")])
            YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("\\/")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("*")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("/")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("//")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("rem")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("mod")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("<<")])
            YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a(">>")])
            YP._operatorTable.addAnswer([200, Atom.a("xfx"), Atom.a("**")])
            YP._operatorTable.addAnswer([200, Atom.a("xfy"), Atom.a("^")])
            YP._operatorTable.addAnswer([200, Atom.a("fy"), Atom.a("-")])
            YP._operatorTable.addAnswer([200, Atom.a("fy"), Atom.a("\\")])
            # Debug: This is hacked in to run the Prolog test suite until we implement op/3.
            YP._operatorTable.addAnswer([20, Atom.a("xfx"), Atom.a("<--")])

        return YP._operatorTable.match([Priority, Specifier, Operator])

    @staticmethod
    def atom_length(atom, Length):
        atom = YP.getValue(atom)
        Length = YP.getValue(Length)
        if isinstance(atom, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                "Expected atom(Arg1) but it is an unbound variable")
        if not isinstance(atom, Atom):
            raise PrologException \
                (Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not an atom")
        if not isinstance(Length, Variable):
            if not isinstance(Length, int):
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), Length), "Length must be var or integer")
            if Length < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), Length), \
                    "Length must not be less than zero")

        return YP.unify(Length, len(atom._name))

    @staticmethod
    def atom_concat(Start, End, Whole):
        # Debug: Should we try to preserve the _declaringClass?
        Start = YP.getValue(Start)
        End = YP.getValue(End)
        Whole = YP.getValue(Whole)
        if isinstance(Whole, Variable):
            if isinstance(Start, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Start and arg 3 Whole are both var")
            if isinstance(End, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 2 End and arg 3 Whole are both var")
            if not isinstance(Start, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), Start), "Arg 1 Start is not an atom")
            if not isinstance(End, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), End), "Arg 2 End is not an atom")

            for  l1 in YP.unify(Whole, Atom.a(Start._name + End._name)):
                yield False
        else:
            if not isinstance(Whole, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), Whole), "Arg 3 Whole is not an atom")
            gotStartLength = False
            startLength = 0
            if not isinstance(Start, Variable):
                if not isinstance(Start, Atom):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("atom"), Start), "Arg 1 Start is not var or atom")
                startLength = len(Start._name)
                gotStartLength = True

            gotEndLength = False
            endLength = 0
            if not isinstance(End, Variable):
                if not isinstance(End, Atom):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("atom"), End), "Arg 2 End is not var or atom")
                endLength = len(End._name)
                gotEndLength = True

            # We are doing a search through all possible Start and End which concatenate to Whole.
            wholeString = Whole._name
            for i in range(len(wholeString) + 1):
                # If we got either startLength or endLength, we know the lengths have to match so check
                #   the lengths instead of constructing an Atom to do it.
                if gotStartLength and startLength != i:
                    continue
                if gotEndLength and endLength != len(wholeString) - i:
                    continue
                for l1 in YP.unify(Start, Atom.a(wholeString[:i])):
                    for l2 in YP.unify(End, Atom.a(wholeString[i:])):
                        yield False

    @staticmethod
    def sub_atom(atom, Before, Length, After, Sub_atom):
        # Debug: Should we try to preserve the _declaringClass?
        atom = YP.getValue(atom)
        Before = YP.getValue(Before)
        Length = YP.getValue(Length)
        After = YP.getValue(After)
        Sub_atom = YP.getValue(Sub_atom)
        if isinstance(atom, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                "Expected atom(Arg1) but it is an unbound variable")
        if not isinstance(atom, Atom):
            raise PrologException \
                (Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not an atom")
        if not isinstance(Sub_atom, Variable):
            if not isinstance(Sub_atom, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), Sub_atom), "Sub_atom is not var or atom")

        beforeIsInt = False
        lengthIsInt = False
        afterIsInt = False
        if not isinstance(Before, Variable):
            if not isinstance(Before, int):
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), Before), "Before must be var or integer")
            beforeIsInt = True
            if Before < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), Before), \
                    "Before must not be less than zero")
        if not isinstance(Length, Variable):
            if not isinstance(Length, int):
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), Length), "Length must be var or integer")
            lengthIsInt = True
            if Length < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), Length), \
                    "Length must not be less than zero")
        if not isinstance(After, Variable):
            if not isinstance(After, int):
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), After), "After must be var or integer")
            afterIsInt = True
            if After < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), After), \
                    "After must not be less than zero")

        atomLength = len(atom._name)
        if beforeIsInt and lengthIsInt:
            # Special case: the caller is just trying to extract a substring, so do it quickly.
            xAfter = atomLength - Before - Length
            if xAfter >= 0:
                for l1 in YP.unify(After, xAfter):
                    for l2 in YP.unify(Sub_atom, Atom.a(atom._name[Before:(Before + Length)])):
                        yield False
        elif afterIsInt and lengthIsInt:
            # Special case: the caller is just trying to extract a substring, so do it quickly.
            xBefore = atomLength - After - Length
            if xBefore >= 0:
                for l1 in YP.unify(Before, xBefore):
                    for l2 in YP.unify(Sub_atom, Atom.a(atom._name[xBefore:(xBefore + Length)])):
                        yield False
        else:
            # We are underconstrained and doing a search, so go through all possibilities.
            for xBefore in range(atomLength + 1):
                for l1 in YP.unify(Before, xBefore):
                    for xLength in range(atomLength - xBefore + 1):
                        for l2 in YP.unify(Length, xLength):
                            for l3 in YP.unify(After, atomLength - (xBefore + xLength)):
                                for l4 in YP.unify(Sub_atom, Atom.a(atom._name[xBefore:(xBefore + xLength)])):
                                    yield False

    @staticmethod
    def atom_chars(atom, List):
        atom = YP.getValue(atom)
        List = YP.getValue(List)

        if isinstance(atom, Variable):
            if isinstance(List, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Atom and arg 2 List are both unbound variables")
            codeArray = ListPair.toArray(List)
            if codeArray == None:
                raise PrologException \
                    (Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list")

            charArray = len(codeArray)*[None]
            for i in range(len(codeArray)):
                listAtom = YP.getValue(codeArray[i])
                if isinstance(listAtom, Variable):
                    raise PrologException(Atom.a("instantiation_error"), \
                        "Arg 2 List has an element which is an unbound variable")
                if not (isinstance(listAtom, Atom) and len(listAtom._name) == 1):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("character"), listAtom), \
                         "Arg 2 List has an element which is not a one character atom")
                charArray[i] = listAtom._name[0]
            return YP.unify(atom, Atom.a("".join(charArray)))
        else:
            if not isinstance(atom, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not var or atom")

            atomString = atom._name
            charList = Atom.NIL
            # Start from the back to make the list.
            for i in range(len(atomString) - 1, -1, -1):
                charList = ListPair(Atom.a(atomString[i]), charList)
            return YP.unify(List, charList)

    @staticmethod
    def atom_codes(atom, List):
        atom = YP.getValue(atom)
        List = YP.getValue(List)

        if isinstance(atom, Variable):
            if isinstance(List, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Atom and arg 2 List are both unbound variables")
            codeArray = ListPair.toArray(List)
            if codeArray == None:
                raise PrologException \
                    (Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list")

            charArray = len(codeArray)*[None]
            for i in range(len(codeArray)):
                codeInt = YP.getInt(codeArray[i])
                if codeInt == None or codeInt < 0:
                    raise PrologException \
                        (Functor1("representation_error", Atom.a("character_code")), \
                         "Element of Arg 2 List is not a character code")
                charArray[i] = chr(codeInt)
            return YP.unify(atom, Atom.a("".join(charArray)))
        else:
            if not isinstance(atom, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not var or atom")

            atomString = atom._name
            codeList = Atom.NIL
            # Start from the back to make the list.
            for i in range(len(atomString) - 1, -1, -1):
                codeList = ListPair(ord(atomString[i]), codeList)
            return YP.unify(List, codeList)

    @staticmethod
    def number_chars(Number, List):
        Number = YP.getValue(Number)
        List = YP.getValue(List)

        if isinstance(Number, Variable):
            if isinstance(List, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Number and arg 2 List are both unbound variables")
            codeArray = ListPair.toArray(List)
            if codeArray == None:
                raise PrologException \
                    (Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list")

            charArray = len(codeArray)*[None]
            for i in range(len(codeArray)):
                listAtom = YP.getValue(codeArray[i])
                if isinstance(listAtom, Variable):
                    raise PrologException(Atom.a("instantiation_error"), \
                        "Arg 2 List has an element which is an unbound variable")
                if not (isinstance(listAtom, Atom) and len(listAtom._name) == 1):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("character"), listAtom), \
                         "Arg 2 List has an element which is not a one character atom")
                charArray[i] = listAtom._name[0]
            return YP.unify(Number, YP.parseNumberString("".join(charArray)))
        else:
            # Try converting to an int first.
            intNumber = YP.getInt(Number)
            if intNumber != None:
                numberString = str(intNumber)
            else:
                if not YP.number(Number):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("number"), Number), \
                        "Arg 1 Number is not var or number")
                # We just checked, so convertDouble shouldn't throw an exception.
                numberString = str(YP.convertDouble(Number))

            charList = Atom.NIL
            # Start from the back to make the list.
            for i in range(len(numberString) - 1, -1, -1):
                charList = ListPair(Atom.a(numberString[i]), charList)
            return YP.unify(List, charList)

    @staticmethod
    def number_codes(Number, List):
        Number = YP.getValue(Number)
        List = YP.getValue(List)

        if isinstance(Number, Variable):
            if isinstance(List, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Number and arg 2 List are both unbound variables")
            codeArray = ListPair.toArray(List)
            if codeArray == None:
                 raise PrologException \
                    (Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list")

            charArray = len(codeArray)*[None]
            for i in range(len(codeArray)):
                codeInt = YP.getInt(codeArray[i])
                if codeInt == None or codeInt < 0:
                    raise PrologException \
                        (Functor1("representation_error", Atom.a("character_code")), \
                         "Element of Arg 2 List is not a character code")
                charArray[i] = chr(codeInt)
            return YP.unify(Number, YP.parseNumberString("".join(charArray)))
        else:
            # Try converting to an int first.
            intNumber = YP.getInt(Number)
            if intNumber != None:
                numberString = str(intNumber)
            else:
                if not YP.number(Number):
                    raise PrologException \
                        (Functor2("type_error", Atom.a("number"), Number), \
                        "Arg 1 Number is not var or number")
                # We just checked, so convertDouble shouldn't throw an exception.
                numberString = str(YP.convertDouble(Number))

            codeList = Atom.NIL
            # Start from the back to make the list.
            for i in range(len(numberString) - 1, -1, -1):
                codeList = ListPair(ord(numberString[i]), codeList)
            return YP.unify(List, codeList)
                                     
    # Used by number_chars and number_codes.  Return the number in numberString or
    # throw an exception if can't parse.
    @staticmethod
    def parseNumberString(numberString):
        if len(numberString) == 3 and numberString[0:2] == "0'":
            # This is a char code.
            return ord(numberString[2])
        if len(numberString) >= 2 and numberString[0:2] == "0x":
            try:
                return int(numberString[2:], 16)
            except ValueError:
                raise PrologException \
                    (Functor1("syntax_error", Atom.a("number_format: " + numberString)), \
                     "Arg 2 List is not a list for a hexadecimal number")

        # Debug: Is there a way in Python to ask if a string parses as int without throwing an exception?
        try:
            # Try an int first.
            return int(numberString)
        except ValueError:
            pass
        try:
            return float(numberString)
        except ValueError:
            raise PrologException \
                (Functor1("syntax_error", Atom.a("number_format: " + numberString)),
                 "Arg 2 List is not a list for a number")

    @staticmethod
    def char_code(Char, Code):
        Char = YP.getValue(Char)
        Code = YP.getValue(Code)

        codeInt = 0
        if not isinstance(Code, Variable):
            # Get codeInt now so we type check it whether or not Char is Variable.
            codeInt = YP.getInt(Code)
            if codeInt == None:
                raise PrologException \
                    (Functor2("type_error", Atom.a("integer"), Code), \
                     "Arg 2 Code is not var or a character code")
            if codeInt < 0:
                raise PrologException \
                    (Functor1("representation_error", Atom.a("character_code")), \
                     "Arg 2 Code is not a character code")

        if isinstance(Char, Variable):
            if isinstance(Code, Variable):
                raise PrologException(Atom.a("instantiation_error"), \
                    "Arg 1 Char and arg 2 Code are both unbound variables")

            return YP.unify(Char, Atom.a(chr(codeInt)))
        else:
            if not isinstance(Char, Atom) or len(Char._name) != 1:
                raise PrologException \
                    (Functor2("type_error", Atom.a("character"), Char), \
                     "Arg 1 Char is not var or one-character atom")

            if isinstance(Code, Variable):
                return YP.unify(Code, ord(Char._name[0]))
            else:
                # Use codeInt to handle whether Code is supplied as, e.g., 97 or 0'a .
                return YP.unify(codeInt, ord(Char._name[0]))
                                 
    # If term is an Atom or functor type, return its name.
    # Otherwise, return term.
    @staticmethod
    def getFunctorName(term):
        term = YP.getValue(term)
        if isinstance(term, Functor1):
            return term._name
        elif isinstance(term, Functor2):
            return term._name
        elif isinstance(term, Functor3):
            return term._name
        elif isinstance(term, Functor):
            return term._name
        else:
            return term

    # If term is an Atom or functor type, return an array of its args.
    # Otherwise, return an empty array.
    @staticmethod
    def getFunctorArgs(term):
        term = YP.getValue(term)
        if isinstance(term, Functor1):
            return [term._arg1]
        elif isinstance(term, Functor2):
            return [term._arg1, term._arg2]
        elif isinstance(term, Functor3):
            return [term._arg1, term._arg2, term._arg3]
        elif isinstance(term, Functor):
            return term._args
        else:
            return []

    @staticmethod
    def var(Term):
        return isinstance(YP.getValue(Term), Variable)

    @staticmethod
    def nonvar(Term):
        return not YP.var(Term)

    @staticmethod
    def atom(Term):
        return isinstance(YP.getValue(Term), Atom)

    @staticmethod
    def integer(Term):
        return isinstance(YP.getValue(Term), int)
                               
    # Use isFloat instead of float because it is a reserved keyword.                           
    @staticmethod
    def isFloat(Term):
        return isinstance(YP.getValue(Term), float)

    @staticmethod
    def number(Term):
        return YP.integer(Term) or YP.isFloat(Term)

    @staticmethod
    def atomic(Term):
        return YP.atom(Term) or YP.number(Term)

    @staticmethod
    def compound(Term):
        Term = YP.getValue(Term)
        return isinstance(Term, Functor1) or isinstance(Term, Functor2) or isinstance(Term, Functor3) or \
            isinstance(Term, Functor)

    _inputStream = None

    # If input is an Atom or String, create a StreamReader with the input as the filename.
    # If input is a Prolog list, then read character codes from it.
    # Otherwise, input must be an object with read and close methods.
    @staticmethod
    def see(input):
        input = YP.getValue(input)
        if isinstance(input, Variable):
            raise PrologException(Atom.a("instantiation_error"), "Arg is an unbound variable")

        if input == None:
            YP._inputStream = None
        elif isinstance(input, Atom):
            YP._inputStream = open(input._name, 'r')
        elif isinstance(input, str):
            YP._inputStream = open(input, 'r')
        elif isinstance(input, Functor2) and input._name == Atom.DOT:
            YP._inputStream = YP.CodeListReader(input)
        else:
            try:
                # Try to access the read and close methods
                #readFunc = output.read
                #closeFunc = output.close
                YP._inputStream = input
            except AttributeError:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("stream_or_alias"), input), \
                     "Input stream specifier not recognized")

    @staticmethod
    def seen():
        if YP._inputStream == None:
            return
        YP._inputStream.close()
        YP._inputStream = None

    @staticmethod
    def current_input(Stream):
        return YP.unify(Stream, YP._inputStream)

    _outputStream = sys.stdout

    # If output is an Atom or a String, create a StreamWriter with the output as the filename.
    # Otherwise, output must be an object with write and close functions.
    @staticmethod
    def tell(output):
        output = YP.getValue(output)
        if isinstance(output, Variable):
            raise PrologException(Atom.a("instantiation_error"), "Arg is an unbound variable")

        if output == None:
            YP._outputStream = None
        elif isinstance(output, Atom):
            YP._outputStream = open(output._name, 'w')
        elif isinstance(output, str):
            YP._outputStream = open(output, 'w')
        else:
            try:
                # Try to access the write and close methods
                #writeFunc = output.write
                #closeFunc = output.close
                YP._outputStream = output
            except AttributeError:
                raise PrologException(Functor2("domain_error", Atom.a("stream_or_alias"), output), \
                    "Can't open stream for " + str(output))

    @staticmethod
    def told():
        if YP._outputStream == None:
            return
        if YP._outputStream == sys.stdout:
            return
        
        YP._outputStream.close()
        YP._outputStream = sys.stdout
                            
    @staticmethod
    def current_output(Stream):
       return YP.unify(Stream, YP._outputStream)

    @staticmethod
    def write(x):
        if YP._outputStream == None:
            return
        YP._outputStream.write(str(YP.getValue(x)))

    @staticmethod
    def put_code(x):
        if YP._outputStream == None:
            return
        if (YP.var(x)):
            raise PrologException(Atom.a("instantiation_error"), "Arg 1 is an unbound variable")
        xInt = YP.getInt(x)
        if xInt == None:
            raise PrologException(Functor2("type_error", Atom.a("integer"), x), "Arg 1 must be integer");
        YP._outputStream.write(chr(xInt))

    @staticmethod
    def nl():
        if YP._outputStream == None:
            return
        YP._outputStream.write("\n")

    @staticmethod
    def get_code(code):
        if YP._inputStream == None:
            return YP.unify(code, -1)
        else:
            inputString = YP._inputStream.read(1)
            if len(inputString) <= 0:
                # End of file
                return YP.unify(code, -1)
            else:
                return YP.unify(code, ord(inputString[0]))

    @staticmethod
    def asserta(Term, declaringClass):
        YP.assertDynamic(Term, declaringClass, True)

    @staticmethod
    def assertz(Term, declaringClass):
        YP.assertDynamic(Term, declaringClass, False)

    @staticmethod
    def assertDynamic(Term, declaringClass, prepend):
        Term = YP.getValue(Term)
        if isinstance(Term, Variable):
            raise PrologException("instantiation_error", "Term to assert is an unbound variable")

        copyStore = Variable.CopyStore()
        TermCopy = YP.makeCopy(Term, copyStore)
        if isinstance(TermCopy, Functor2) and TermCopy._name == Atom.RULE:
            Head = YP.getValue(TermCopy._arg1)
            Body = YP.getValue(TermCopy._arg2)
            if isinstance(Head, Variable):
                raise PrologException("instantiation_error", "Head to assert is an unbound variable")
            if isinstance(Body, Variable):
                raise PrologException("instantiation_error", "Body to assert is an unbound variable")
        else:
            Head = TermCopy
            Body = Atom.TRUE

        name = YP.getFunctorName(Head)
        if not isinstance(name, Atom):
            # name is a non-Atom, such as a number.
            raise PrologException \
                (Functor2("type_error", Atom.a("callable"), Head), "Term to assert is not callable")
        args = YP.getFunctorArgs(Head)
        if YP.isSystemPredicate(name, len(args)):
            raise PrologException \
                (Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), \
                              Functor2(Atom.SLASH, name, len(args))), \
                 "Assert cannot modify static predicate " + str(name) + "/" + str(len(args)))

        if copyStore.getNUniqueVariables() == 0 and Body == Atom.TRUE:
            # This is a fact with no unbound variables
            # assertFact and prependFact use IndexedAnswers, so don't we don't need to compile.
            if prepend:
                YP.prependFact(name, args)
            else:
                YP.assertFact(name, args)
            return

        clause = Compiler.compileAnonymousClause(Head, Body)
        # We expect clause to be a ClauseHeadAndBody (from Compiler.compileAnonymousFunction)
        #   so we can set the Head and Body.
        if isinstance(clause, YP.ClauseHeadAndBody):
            clause.setHeadAndBody(Head, Body)

        # Add the clause to the entry in _predicatesStore.
        nameAndArity = YP.NameArity(name, len(args))
        try:
            clauses = YP._predicatesStore[nameAndArity]
        except KeyError:
            # Create an entry for the nameAndArity.
            clauses = []
            YP._predicatesStore[nameAndArity] = clauses

        if (prepend):
            clauses.insert(0, clause)
        else:
            clauses.append(clause)

    @staticmethod
    def isSystemPredicate(name, arity):
        if arity == 2 and (name == Atom.a(",") or name == Atom.a(";") or name == Atom.DOT):
            return True
        # Use the same mapping to static predicates in YP as the compiler.
        for l1 in functorCallYPFunctionName(name, arity, Variable()):
            return True
        # Debug: Do we need to check if name._module is null?
        return False

    # Assert values at the end of the set of facts for the predicate with the
    # name and with arity len(values).
    # "name" must be an Atom.
    # "values" is the array of arguments to the fact predicate.
    # It is an error if an value has an unbound variable.</param>
    @staticmethod
    def assertFact(name, values):
        nameAndArity = YP.NameArity(name, len(values))
        try:
            clauses = YP._predicatesStore[nameAndArity]
            indexedAnswers = clauses[len(clauses) - 1]
            if not isinstance(indexedAnswers, IndexedAnswers):
                # The latest clause is not an IndexedAnswers, so add one.
                indexedAnswers = IndexedAnswers(len(values))
                clauses.append(indexedAnswers)
        except KeyError:
            # Create an IndexedAnswers as the first clause of the predicate.
            indexedAnswers = IndexedAnswers(len(values))
            clauses = [indexedAnswers]
            YP._predicatesStore[nameAndArity] = clauses

        indexedAnswers.addAnswer(values)

    # Assert values, prepending to the set of facts for the predicate with the
    # name and with arity len(values).
    # "name" must be an Atom.
    # "values" is the array of arguments to the fact predicate.
    # It is an error if an value has an unbound variable.</param>
    @staticmethod
    def prependFact(name, values):
        nameAndArity = YP.NameArity(name, len(values))
        try:
            clauses = YP._predicatesStore[nameAndArity]
            indexedAnswers = clauses[0]
            if not isinstance(indexedAnswers, IndexedAnswers):
                # The first clause is not an IndexedAnswers, so prepend one.
                indexedAnswers = IndexedAnswers(len(values))
                clauses.insert(0, indexedAnswers)
        except KeyError:
            # Create an IndexedAnswers as the first clause of the predicate.
            indexedAnswers = IndexedAnswers(len(values))
            clauses = [indexedAnswers]
            YP._predicatesStore[nameAndArity] = clauses

        indexedAnswers.prependAnswer(values)

    # Match all clauses of the dynamic predicate with the name and with arity
    # len(arguments).
    # It is an error if the predicate is not defined.
    # "name" must be an Atom.
    # "arguments" is an array of arity number of arguments
    # Returns an iterator which you can use in foreach.
    @staticmethod
    def matchDynamic(name, arguments):
        nameAndArity = YP.NameArity(name, len(arguments))
        try:
            clauses = YP._predicatesStore[nameAndArity]
        except KeyError:
            return YP.unknownPredicate \
                (name, len(arguments), \
                 "Undefined dynamic predicate: " + str(name) + "/" + str(len(arguments)))

        if len(clauses) == 1:
            # Usually there is only one clause, so return it without needing to wrap it in an iterator.
            return clauses[0].match(arguments)
        else:
            return YP.matchAllClauses(clauses, arguments)

    # Call match(arguments) for each IClause in clauses.  We make this a separate
    # function so that matchDynamic itself does not need to be an iterator object.
    @staticmethod
    def matchAllClauses(clauses, arguments):
        for clause in clauses:
            for lastCall in clause.match(arguments):
                yield False
                if lastCall:
                    # This happens after a cut in a clause.
                    return

    # If _prologFlags["unknown"] is fail then return fail(), else if 
    #   _prologFlags["unknown"] is warning then write the message to YP.write and
    #   return fail(), else raise a PrologException for existence_error.  .
    @staticmethod
    def unknownPredicate(name, arity, message):
        YP.establishPrologFlags()

        if YP._prologFlags["unknown"] == Atom.a("fail"):
            return YP.fail()
        elif YP._prologFlags["unknown"] == Atom.a("warning"):
            YP.write(message)
            YP.nl()
            return YP.fail()
        else:
            raise PrologException \
                (Functor2(Atom.a("existence_error"), Atom.a("procedure"), \
                          Functor2(Atom.SLASH, name, arity)), message)

    # This is deprecated and just calls matchDynamic. This matches all clauses, 
    # not just the ones defined with assertFact.
    @staticmethod
    def matchFact(name, arguments):
        return YP.matchDynamic(name, arguments)

    @staticmethod
    def clause(Head, Body):
        Head = YP.getValue(Head)
        Body = YP.getValue(Body)
        if isinstance(Head, Variable):
            raise PrologException("instantiation_error", "Head is an unbound variable")

        name = YP.getFunctorName(Head)
        if not isinstance(name, Atom):
            # name is a non-Atom, such as a number.
            raise PrologException \
                (Functor2("type_error", Atom.a("callable"), Head), "Head is not callable")
        args = YP.getFunctorArgs(Head)
        if YP.isSystemPredicate(name, len(args)):
            raise PrologException \
                (Functor3("permission_error", Atom.a("access"), Atom.a("private_procedure"), \
                          Functor2(Atom.SLASH, name, len(args))), \
                 "clause cannot access private predicate " + str(name) + "/" + str(len(args)))
        if not (isinstance(Body, Variable) and not isinstance(YP.getFunctorName(Body), Atom)):
            raise PrologException \
                (Functor2("type_error", Atom.a("callable"), Body), "Body is not callable")

        nameAndArity = YP.NameArity(name, len(args))
        try:
            clauses = YP._predicatesStore[nameAndArity]
        except KeyError:
            return
        
        # The caller can assert another clause into this same predicate during yield, so we have to
        #   make a copy of the clauses.
        for predicateClause in [x for x in clauses]:
            for l1 in predicateClause.clause(Head, Body):
                yield False

    @staticmethod
    def retract(Term):
        Term = YP.getValue(Term)
        if isinstance(Term, Variable):
            raise PrologException("instantiation_error", "Term to retract is an unbound variable")

        if isinstance(Term, Functor2) and Term._name == Atom.RULE:
            Head = YP.getValue(Term._arg1)
            Body = YP.getValue(Term._arg2)
        else:
            Head = Term
            Body = Atom.TRUE

        if isinstance(Head, Variable):
            raise PrologException("instantiation_error", "Head is an unbound variable")

        name = YP.getFunctorName(Head)
        if not isinstance(name, Atom):
            # name is a non-Atom, such as a number.
            raise PrologException \
                (Functor2("type_error", Atom.a("callable"), Head), "Head is not callable")
        args = YP.getFunctorArgs(Head)
        if YP.isSystemPredicate(name, len(args)):
            raise PrologException \
                (Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), \
                   Functor2(Atom.SLASH, name, len(args))), \
                    "clause cannot access private predicate " + str(name) + "/" + str(len(args)))
        if not isinstance(Body, Variable) and not isinstance(YP.getFunctorName(Body), Atom):
            raise PrologException \
                (Functor2("type_error", Atom.a("callable"), Body), "Body is not callable")

        nameAndArity = YP.NameArity(name, len(args))
        try:
            clauses = YP._predicatesStore[nameAndArity]
        except KeyError:
            return

        # The caller can assert another clause into this same predicate during yield, so we have to
        #   make a copy of the clauses.
        for predicateClause in [x for x in clauses]:
            if isinstance(predicateClause, IndexedAnswers):
                # IndexedAnswers handles its own retract.  Even if it removes all of its
                #   answers, it is OK to leave it empty as one of the elements in clauses.
                for l1 in predicateClause.retract(Head, Body):
                    yield False
            else:
                for l1 in predicateClause.clause(Head, Body):
                    # Remove predicateClause.
                    clauses.remove(predicateClause)
                    yield False
    
    # This is deprecated for backward compatibility.  You should use retractall.
    # "name" must be an Atom.
    # "arguments" is an array of arity number of arguments.
    @staticmethod
    def retractFact(name, arguments):
       YP.retractall(Functor.make(name, arguments))

    # Retract all dynamic clauses which unify with Head.  If this matches all clauses in a predicate,
    # the predicate is still defined.  To completely remove the predicate, see abolish.
    @staticmethod
    def retractall(Head):
        name = YP.getFunctorName(Head)
        arguments = YP.getFunctorArgs(Head)
        if not isinstance(name, Atom):
            return
        nameAndArity = YP.NameArity(name, len(arguments))
        try:
            clauses = YP._predicatesStore[nameAndArity]
        except KeyError:
            # Can't find, so ignore.
            return

        for arg in arguments:
            if not YP.var(arg):
                raise "Until matching retractall is supported, all arguments must be unbound to retract all clauses"

        # Set to a fresh empty IndexedAnswers.
        YP._predicatesStore[nameAndArity] = [IndexedAnswers(len(arguments))]

    # If NameSlashArity is var, match with all the dynamic predicates using the
    # Name/Artity form.
    # If NameSlashArity is not var, check if the Name/Arity exists as a static or
    # dynamic predicate.
    # declaringClass: if not null, the dictionary of globals used to resolve references to the default 
    #   module Atom.a("") 
    @staticmethod
    def current_predicate(NameSlashArity, declaringClass):
        NameSlashArity = YP.getValue(NameSlashArity)
        # First check if Name and Arity are nonvar so we can do a direct lookup.
        if YP.ground(NameSlashArity):
            if not (isinstance(NameSlashArity, Functor2) and NameSlashArity._name == Atom.SLASH):
                raise PrologException \
                    (Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity), \
                     "Must be a name/arity predicate indicator")
            name = YP.getValue(NameSlashArity._arg1)
            arity = YP.getValue(NameSlashArity._arg2)
            if isinstance(name, Variable) or isinstance(arity, Variable):
                raise PrologException \
                    ("instantiation_error", "Predicate indicator name or arity is an unbound variable")
            if not(isinstance(name, Atom) and isinstance(arity, int)):
                raise PrologException \
                    (Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity), \
                     "Must be a name/arity predicate indicator")
            if arity < 0:
                raise PrologException \
                    (Functor2("domain_error", Atom.a("not_less_than_zero"), arity), \
                     "Arity may not be less than zero")

            if Compiler.isCurrentPredicate(name, arity, declaringClass):
                # The predicate is defined.
                yield False
        else:
            for key in YP._predicatesStore:
                for l1 in YP.unify(Functor2(Atom.SLASH, key._name, key._arity), NameSlashArity):
                    yield False

    # Return true if the dynamic predicate store has an entry for the predicate
    # with name and arity.
    @staticmethod
    def isDynamicCurrentPredicate(name, arity):
        nameAndArity = YP.NameArity(name, arity)
        return YP._predicatesStore.has_key(nameAndArity)

    @staticmethod
    def abolish(NameSlashArity):
        NameSlashArity = YP.getValue(NameSlashArity)
        if isinstance(NameSlashArity, Variable):
            raise PrologException \
                ("instantiation_error", "Predicate indicator is an unbound variable")
        if not (isinstance(NameSlashArity, Functor2) and NameSlashArity._name == Atom.SLASH):
            raise PrologException \
                (Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity), \
                 "Must be a name/arity predicate indicator")
        name = YP.getValue(NameSlashArity._arg1)
        arity = YP.getValue(NameSlashArity._arg2)
        if isinstance(name, Variable) or isinstance(arity, Variable):
            raise PrologException \
                ("instantiation_error", "Predicate indicator name or arity is an unbound variable")
        if not isinstance(name, Atom):
            raise PrologException \
                (Functor2("type_error", Atom.a("atom"), name), \
                 "Predicate indicator name must be an atom")
        if not isinstance(arity, int):
            raise PrologException \
                (Functor2("type_error", Atom.a("integer"), arity), \
                 "Predicate indicator arity must be an integer")
        if arity < 0:
            raise PrologException \
                (Functor2("domain_error", Atom.a("not_less_than_zero"), arity), \
                 "Arity may not be less than zero")
        if arity > YP.MAX_ARITY:
            raise PrologException \
                (Functor1("representation_error", Atom.a("max_arity")), \
                 "Arity may not be greater than " + str(YP.MAX_ARITY))

        if YP.isSystemPredicate(name, arity):
            raise PrologException \
                (Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), \
                          Functor2(Atom.SLASH, name, arity)), \
                 "Abolish cannot modify static predicate " + str(name) + "/" + str(arity))
                 
        nameAndArity = YP.NameArity(name, arity)
        # Remove the entry.
        del YP._predicatesStore[nameAndArity]

    # If Goal is a simple predicate, call YP.getFunctorName(Goal) using arguments from 
    # YP.getFunctorArgs(Goal). If not found, this throws a PrologException for existence_error.
    # Otherwise, compile the goal as a single clause predicate and invoke it. 
    # declaringClass: if not None, the dictionary of globals used to resolve references to the default 
    #   module Atom.a("") 
    @staticmethod
    def getIterator(Goal, declaringClass):
        while True:
            Goal = YP.getValue(Goal)
            if isinstance(Goal, Variable):
                raise PrologException("instantiation_error", "Goal to call is an unbound variable")
            name = YP.getFunctorName(Goal)
            if not isinstance(name, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("callable"), Goal), "Goal to call is not callable")
            args = YP.getFunctorArgs(Goal)
            if name == Atom.HAT and len(args) == 2:
                # Assume this is called from a bagof operation.  Skip the leading qualifiers.
                Goal = YP.getValue(Goal._arg2)
            else:
                break

        simpleIterator = Compiler.getSimpleIterator(name, args, declaringClass)
        if simpleIterator != None:
            # We don't need to compile since the goal is a simple predicate which we call directly.
            return simpleIterator

        # Compile the goal as a clause.
        variableSet = []
        YP.addUniqueVariables(Goal, variableSet)

        # Use Atom.F since it is ignored.
        return Compiler.compileAnonymousClause(Functor.make(Atom.F, variableSet), Goal).match(variableSet)

    @staticmethod
    def throwException(Term):
        raise PrologException(Term)

    # This must be called by any function that uses YP._prologFlags to make sure
    # the initial defaults are loaded.
    @staticmethod
    def establishPrologFlags():
        if YP._prologFlags.has_key("bounded"):
            # Already established.
            return

        # List these in the order they appear in the ISO standard.
        YP._prologFlags["bounded"] = Atom.TRUE
        YP._prologFlags["max_integer"] = 2147483647
        YP._prologFlags["min_integer"] = -2147483648
        YP._prologFlags["integer_rounding_function"] = Atom.a("toward_zero")
        YP._prologFlags["char_conversion"] = Atom.a("off")
        YP._prologFlags["debug"] = Atom.a("off")
        YP._prologFlags["max_arity"] = YP.MAX_ARITY
        YP._prologFlags["unknown"] = Atom.a("error")
        YP._prologFlags["double_quotes"] = Atom.a("codes")

    @staticmethod
    def current_prolog_flag(Key, Value):
        YP.establishPrologFlags()

        Key = YP.getValue(Key)
        Value = YP.getValue(Value)

        if isinstance(Key, Variable):
            # Bind all key values.
            for key in YP._prologFlags:
                for l1 in YP.unify(Key, Atom.a(key)):
                    for l2 in YP.unify(Value, YP._prologFlags[key]):
                        yield False
        else:
            if not isinstance(Key, Atom):
                raise PrologException \
                    (Functor2("type_error", Atom.a("atom"), Key), "Arg 1 Key is not an atom")
            if not YP._prologFlags.has_key(Key._name):
                raise PrologException \
                    (Functor2("domain_error", Atom.a("prolog_flag"), Key), \
                     "Arg 1 Key is not a recognized flag")

            for l1 in YP.unify(Value, YP._prologFlags[Key._name]):
                yield False

    @staticmethod
    def set_prolog_flag(Key, Value):
        YP.establishPrologFlags()

        Key = YP.getValue(Key)
        Value = YP.getValue(Value)

        if isinstance(Key, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                "Arg 1 Key is an unbound variable")
        if isinstance(Value, Variable):
            raise PrologException(Atom.a("instantiation_error"), \
                "Arg 1 Key is an unbound variable")
        if not isinstance(Key, Atom):
            raise PrologException \
                (Functor2("type_error", Atom.a("atom"), Key), "Arg 1 Key is not an atom")

        keyName = Key._name
        if not YP._prologFlags.has_key(keyName):
            raise PrologException \
                (Functor2("domain_error", Atom.a("prolog_flag"), Key),
                "Arg 1 Key " + str(Key) + " is not a recognized flag")

        valueIsOK = False
        if keyName == "char_conversion":
            valueIsOK = (Value == YP._prologFlags[keyName])
        elif keyName == "debug":
            valueIsOK = (Value == YP._prologFlags[keyName])
        elif keyName == "unknown":
            valueIsOK = (Atom.a("fail").equals(Value) or Atom.a("warning").equals(Value) or \
                Atom.a("error").equals(Value))
        elif keyName == "double_quotes":
            valueIsOK = (Value == Atom.a("codes") or Value == Atom.a("chars") or \
                Value == Atom.a("atom"))
        else:
            raise PrologException \
                (Functor3("permission_error", Atom.a("modify"), Atom.a("flag"), Key), \
                 "May not modify Prolog flag " + str(Key))

        if not valueIsOK:
            raise PrologException \
                (Functor2("domain_error", Atom.a("flag_value"), Functor2("+", Key, Value)), \
                "May not set arg 1 Key " + str(Key) + " to arg 2 Value" + str(Value))

        YP._prologFlags[keyName] = Value

    # An iterator that does zero loops.
    class Fail(object):
        def __iter__(self):
            return self
        
        def next(self):
            raise StopIteration
        
        def close(self):
            pass

    # An iterator that does one loop.
    class Succeed(object):
        def __init__(self):
            self._didIteration = False

        def __iter__(self):
            return self

        def next(self):
            if not self._didIteration:
                self._didIteration = True
                return False
            else:
                raise StopIteration
        
        def close(self):
            pass

    # An iterator that repeats forever.
    class Repeat(object):
        def __iter__(self):
            return self
        
        def next(self):
            return False
        
        def close(self):
            pass

    # YP.Catch is an iterator that wraps another iterator in order to catch a PrologException.
    class Catch(object):
        # Call YP.getIterator(Goal, declaringClass) and save the returned iterator.
        # If getIterator throws an exception, save it the same as next().
        def __init__(self, Goal, declaringClass):
            self._exception = None
            try:
                self._enumerator = iter(YP.getIterator(Goal, declaringClass))
            except PrologException, exception:
                # next() will check this.
                self._exception = exception
        
        def __iter__(self):
            return self

        # Call _enumerator.next().  If it throws a PrologException, set _exception
        # and throw StopIteration.  After this raises StopIteration, call unifyExceptionOrThrow.
        def next(self):
            if self._exception != None:
                raise StopIteration
                
            try:
                return self._enumerator.next()
            except PrologException, exception:
                self._exception = exception
                raise StopIteration
            # Else allow the exception which includes StopIteration.

        # Call this after next() returns false to check for an exception.  If
        # next did not get a PrologException, don't yield.
        # Otherwise, unify the exception with Catcher and yield so the caller can
        # do the handler code.  However, if can't unify with Catcher then throw the exception.
        def unifyExceptionOrThrow(self, Catcher):
            if self._exception != None:
                didUnify = False
                for l1 in YP.unify(self._exception._term, Catcher):
                    didUnify = True
                    yield False

                if not didUnify:
                    raise self._exception

        def close(self):
            self._enumerator.close()

    # A ClauseHeadAndBody is used in Compiler.compileAnonymousFunction as a base class
    # in order to implement YP.IClause.  After creating the object, you must call setHeadAndBody.
    class ClauseHeadAndBody(object):
        def __init__(self):
            self._Head = None
            self._Body = None

        def setHeadAndBody(self, Head, Body):
            self._Head = Head
            self._Body = Body

        def clause(self, Head, Body):
            if self._Head == None or self._Body == None:
                return YP.fail()

            # First, check if we have a match without the cost of makeCopy.
            gotMatch = false
            for l1 in YP.unify(Head, self._Head):
                gotMatch = True
                break

            if gotMatch:
                # We have to return a copy of _Body where the variables from _Head are bound properly.
                copyStore = Variable.CopyStore()
                RuleCopy = YP.makeCopy(Functor2(Atom.RULE, self._Head, self._Body), copyStore)

                return YP.unify(Functor2(Atom.RULE, Head, Body), RuleCopy)
            else:
                return YP.fail()

    class StringReader(object):
        def __init__(self, inputString):
            self._inputString = inputString
            self._inputReadIndex = 0

        # Return the next character in the inputString or '' if past the end.
        # nCharacters must be 1.
        def read(self, nCharacters):
            if nCharacters != 1:
                raise "StringReader.read: nCharacters must be 1"
            if self._inputReadIndex >= len(self._inputString):
                return ''
            result = self._inputString[self._inputReadIndex]
            self._inputReadIndex += 1
            return result

        def close(self):
            pass

    # A YP.StringWriter has write and writeLine to append to a string buffer.
    # To get the result, call toString.
    # A YP.StringWriter object can be used in YP.tell.
    class StringWriter(object):
        def __init__(self):
            self._stringArray = []

        def write(self, text):
            self._stringArray.append(str(text))

        def writeLine(self, text = None):
            if text != None:
                self._stringArray.append(str(text))
            self._stringArray.append("\n")

        # Convert the results so far to a string and return it.
        def toString(self):
            return "".join(self._stringArray)

        def close(self):
            pass

    # CodeListReader has a method to read the next code from
    # the CodeList which is a Prolog list of integer character codes.
    class CodeListReader(object):
        def __init__(self, CodeList):
            self._CodeList = YP.getValue(CodeList)

        # If the head of _CodeList is an integer, return its chr and advance the list.  Otherwise,
        # return '' for end of file.
        # nCharacters must be 1.
        def read(self, nCharacters):
            if nCharacters != 1:
                raise "StringReader.read: nCharacters must be 1"
            if not (isinstance(self._CodeList, Functor2) and self._CodeList._name == Atom.DOT):
                self._CodeList = Atom.NIL
                return ''
            code = YP.getInt(self._CodeList._arg1)
            if code == None:
                self._CodeList = Atom.NIL
                return ''

            # Advance.
            self._CodeList = YP.getValue(self._CodeList._arg2)
            return chr(code)

        def close(self):
            pass

    _fail = Fail()
    _repeat = Repeat()
    _predicatesStore = {}
    _prologFlags = {}
    MAX_ARITY = 255
    
# Put these down here to avoid import loops.
from PrologException import *
from Variable import *
from IndexedAnswers import *
from Atom import *
from Compiler import *
from Functor1 import *
from Functor2 import *
from Functor3 import *
from Functor import *
from ListPair import *
from PrologException import *
