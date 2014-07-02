# Copyright (C) 2008, Jeff Thompson
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
from BagofAnswers import *
from FindallAnswers import *
from Functor1 import *
from Functor2 import *
from Functor3 import *
from Functor import *
from ListPair import *
from Parser import *
from Variable import *
import types

class CompilerState(object):
    def __init__(self):
        self._pred = IndexedAnswers(4)
        self._moduleForNameArity = {}

    # Make these static functions that explicitly take the State so Prolog can call it.

    # Make a new CompilerState and bind it to State.
    @staticmethod
    def make(State):
        return YP.unify(State, CompilerState())

    @staticmethod
    def assertPred(State, Pred, Determinism):
        State = YP.getValue(State)
        functorName = YP.getFunctorName(Pred)
        functorArgs = YP.getFunctorArgs(Pred)
        # Debug: Should check if it's already asserted and is the same.
        State._pred.addAnswer([functorName, len(functorArgs), Pred, YP.getValue(Determinism)])

    @staticmethod
    def assertModuleForNameArity(State, Name, Arity, Module):
        State = YP.getValue(State)
        Name = YP.getValue(Name)
        Arity = YP.getValue(Arity)
        Module = YP.getValue(Module)
        # If the Module Atom comes from the parser, it always has null _declaringClass.
        if isinstance(Module, Atom) and Module._module == None and isinstance(Name, Atom) and isinstance(Arity, int):
            # Replace a previous entry if it exists.
            State._moduleForNameArity[str(Name) + "/" + str(Arity)] = Module

    @staticmethod
    def startFunction(State, Head):
        State = YP.getValue(State)
        State._gensymCounter = 0
        State._useFinalCutCode = False
        State._finalCutCode = Variable()
        State._codeUsesYield = False
        if CompilerState.isDetNoneOut(State, Head):
            State._determinism = Atom.a("detNoneOut")
        elif CompilerState.isSemidetNoneOut(State, Head):
            State._determinism = Atom.a("semidetNoneOut")
        else:
            State._determinism = Atom.a("nondet")

    @staticmethod
    def setCodeUsesYield(State):
        State = YP.getValue(State)
        State._codeUsesYield = True

    @staticmethod
    def codeUsesYield(State):
        State = YP.getValue(State)
        return State._codeUsesYield

    @staticmethod
    def determinismEquals(State, Term):
        State = YP.getValue(State)
        return YP.termEqual(State._determinism, Term)

    # Set _variableNames to a new list of (Name = Variable) for each unique variable in rule.
    # If the variable is in variableNameSuggestions, use it, otherwise use x1, x2, etc.
    @staticmethod
    def newVariableNames(State, Rule, VariableNameSuggestions):
        State = YP.getValue(State)
        variablesSet = []
        YP.addUniqueVariables(Rule, variablesSet)

        State._variableNames = []
        xCounter = 0
        for variable in variablesSet:
            xCounter += 1
            State._variableNames.append \
                (Functor2(Atom.a("="), CompilerState.makeVariableName(variable, VariableNameSuggestions, xCounter), \
                          variable))

    @staticmethod
    def makeVariableName(variable, variableNameSuggestions, xCounter):
        # Debug: should require named variables to start with _ or capital. Should
        #   check for duplicates and clashes with keywords.
        element = YP.getValue(variableNameSuggestions)
        while isinstance(element, Functor2) and element._name == Atom.DOT:
            suggestionPair = YP.getValue(element._arg1)
            if sameVariable(variable, suggestionPair._arg2):
                suggestion = YP.getValue(suggestionPair._arg1)
                if suggestion == Atom.a("Atom"):
                    suggestion = Atom.a("Atom_1")
                if suggestion == Atom.a("Variable"):
                    suggestion = Atom.a("Variable_1")
                if suggestion == Atom.a("Functor"):
                    suggestion = Atom.a("Functor_1")
                return suggestion
        
            element = YP.getValue(element._arg2)

        return Atom.a("x" + str(xCounter))

    # Unify Result with the name assigned by CompilerState.newVariableNames in State._variableNames
    #   for variable.
    @staticmethod
    def getVariableName(State, variable, Result):
        State = YP.getValue(State)
        for variableInfo in State._variableNames:
            if isinstance(variableInfo, Functor2) and variableInfo._name == Atom.a("="):
                if sameVariable(variable, variableInfo._arg2):
                    return YP.unify(Result, variableInfo._arg1)

        # We set up names for all unique variables, so this should never happen.
        raise PrologException(Atom.a("Can't find entry in _variableNames"))

    @staticmethod
    def variableNamesList(State, VariableNamesList):
        State = YP.getValue(State)
        return YP.unify(VariableNamesList, ListPair.make(State._variableNames))

    @staticmethod
    def gensym(State, Base, Symbol):
        State = YP.getValue(State)
        Base = YP.getValue(Base)
        State._gensymCounter += 1
        return YP.unify(Symbol, Atom.a(str(Base) + str(State._gensymCounter)))

    @staticmethod
    def isDetNoneOut(State, Term):
        State = YP.getValue(State)
        functorName = YP.getFunctorName(Term)
        functorArgs = YP.getFunctorArgs(Term)

        pred = Variable()
        for l1 in State._pred.match([functorName, len(functorArgs), pred, Atom.a("det")]):
            if CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue())):
                return True

        return False

    @staticmethod
    def isSemidetNoneOut(State, Term):
        State = YP.getValue(State)
        functorName = YP.getFunctorName(Term)
        functorArgs = YP.getFunctorArgs(Term)

        pred = Variable()
        for l1 in State._pred.match([functorName, len(functorArgs), pred, Atom.a("semidet")]):
            if CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue())):
                return True

        return False

    # Return False if any of args is out, otherwise True.
    # args is an array of ::(Type,Mode) where Mode is in or out.
    @staticmethod
    def isNoneOut(args):
        for arg in args:
            if isinstance(arg, Functor2) and arg._name == Atom.a("::") and arg._arg2 == Atom.a("out"):
                return False

        return True

    @staticmethod
    def nameArityHasModule(State, Name, Arity, Module):
        State = YP.getValue(State)
        Name = YP.getValue(Name)
        Arity = YP.getValue(Arity)
        Module = YP.getValue(Module)
        if isinstance(Name, Atom) and isinstance(Arity, int):
            nameArity = str(Name) + "/" + str(Arity)
            if not State._moduleForNameArity.has_key(nameArity):
                return False
            return State._moduleForNameArity[nameArity] == Module

        return False

class Compiler(object):
    # Use makeFunctionPseudoCode, convertFunctionPython and compileAnonymousFunction
    # to return an anonymous YP.IClause for the Head and Body of a rule clause.
    # Head is a prolog term such as new Functor2("test1", X, Y).
    # Note that the name of the head is ignored.
    # Body is a prolog term such as 
    # new Functor2(",", new Functor1(Atom.a("test2", Atom.a("")), X), 
    #              new Functor2("=", Y, X)).
    # This may not be None.  (For a head-only clause, set the Body to Atom.a("true"). 
    # (This has no declaringClass because it is assumed that predicates with default module Atom.a("")
    #  are in the global scope.)
    # Returns a new object on which you can call match(args) where
    # args length is the arity of the Head.
    @staticmethod
    def compileAnonymousClause(Head, Body):
        args = YP.getFunctorArgs(Head)
        # compileAnonymousFunction wants "function".
        Rule = Functor2(Atom.RULE, Functor.make("function", args), Body)
        RuleList = ListPair.make(Functor2(Atom.F, Rule, Atom.NIL))

        functionCode = YP.StringWriter()
        SaveOutputStream = Variable()
        for l1 in YP.current_output(SaveOutputStream):
            try:
                YP.tell(functionCode)
                PseudoCode = Variable()
                for l2 in makeFunctionPseudoCode(RuleList, PseudoCode):
                    if YP.termEqual(PseudoCode, Atom.a("getDeclaringClass")):
                        # Ignore getDeclaringClass since we have access to the one passed in.
                        continue

                    convertFunctionPython(PseudoCode)

                YP.told()
            finally:
                # Restore after calling tell.
                YP.tell(SaveOutputStream.getValue())

        return Compiler.compileAnonymousFunction(functionCode.toString(), len(args))
    
    # Use compile() to compile the functionCode and return a YP.ClauseHeadAndBody object which implements
    #   match(args) which is called with an array of the arguments to match the clause.
    # functionCode is the code for the iterator, such as
    # "def function():\n  yield False".
    # The function name must be "function" and have nArgs arguments.
    @staticmethod
    def compileAnonymousFunction(functionCode, nArgs):
        # Define match to explicitly call self._function with the right number of arguments
        #   instead of using apply which is less efficient.
        classCode = YP.StringWriter()
        classCode.writeLine("class Temp(YP.ClauseHeadAndBody):")
        classCode.writeLine("    def __init__(self, function):")
        classCode.writeLine("        YP.ClauseHeadAndBody.__init__(self)")
        classCode.writeLine("        self._function = function")
        classCode.writeLine("    def match(self, args):")
        classCode.write(    "        return self._function(")
        if nArgs >= 1:
            classCode.write("args[0]")
        for i in range(1, nArgs):
            classCode.write(", args[" + str(i) + "]")
        classCode.writeLine(")")

        # This defines the function in the local scope.
        exec compile(functionCode, 'anonymous', 'single') in globals(), locals()
        # This defines the class Temp in the local scope.
        exec compile(classCode.toString(), 'anonymous', 'single') in globals(), locals()
        # Return a Temp object whose match calls function.
        return Temp(function)

    # If the functor with name and args can be called directly as determined by
    #   functorCallFunctionName, then call it and return its iterator.  If the predicate is
    #   dynamic and undefined, or if static and the method cannot be found, return
    #   the result of YP.unknownPredicate.
    # declaringClass: the dictionary of globals used to resolve references to the default 
    # module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
    #   None, this looks in the global scope.  If not found, this raises a 
    #   PrologException for existence_error.
    # This returns None if the functor has a special form than needs to be compiled 
    #   (including ,/2 and ;/2).
    @staticmethod
    def getSimpleIterator(name, args, declaringClass):
        state = CompilerState()
        FunctionName = Variable()
        for l1 in functorCallFunctionName(state, name, len(args), FunctionName):
            functionNameAtom = FunctionName.getValue()
            if functionNameAtom == Atom.NIL:
                # name is for a dynamic predicate.
                return YP.matchDynamic(name, args)

            methodName = functionNameAtom._name
            # Set the default for the method to call.
            methodDictionary = declaringClass
            methodClassName = "the module global scope"

            checkMode = False
            if methodName[0:3] == "YP.":
                # Assume we only check mode in calls to standard Prolog predicates in YP.
                checkMode = True

                # Use the method in class YP.
                methodName = methodName[3:]
                methodDictionary = YP.__dict__
                methodClassName = "YP"
            if methodName.find(".") >= 0:
                # We don't support calling inner classes, etc.
                return None

            func = None
            if methodDictionary.has_key(methodName):
                func = methodDictionary[methodName]
                if isinstance(func, staticmethod):
                    func = func.__get__(func)
                if not isinstance(func, types.FunctionType):
                    func = None

            if func == None:
                raise PrologException \
                    (Functor2 \
                     (Atom.a("existence_error"), Atom.a("procedure"), \
                      Functor2(Atom.a("/"), name, len(args))), \
                     "Cannot find predicate function " + methodName + " for " + str(name) + "/" + str(len(args)) + \
                     " in " + methodClassName)
            
            if checkMode:
                assertYPPred(state)
                functor = Functor.make(name, args)
                if CompilerState.isDetNoneOut(state, functor):
                    func(*args)
                    return YP.succeed()
                if CompilerState.isSemidetNoneOut(state, functor):
                    if func(*args):
                        return YP.succeed()
                    else:
                        return YP.fail()

            return iter(func(*args))

        return None

    # Return True if there is a dynamic or static predicate with name and arity.
    # This returns False for built-in predicates.
    # declaringClass: the dictionary of globals used to resolve references to the default 
    # module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
    #   None, return False.
    @staticmethod
    def isCurrentPredicate(name, arity, declaringClass):
        state = CompilerState()
        FunctionName = Variable()
        for l1 in functorCallFunctionName(state, name, arity, FunctionName):
            functionNameAtom = FunctionName.getValue()
            if functionNameAtom == Atom.NIL:
                # name is for a dynamic predicate.
                return YP.isDynamicCurrentPredicate(name, arity)

            methodName = functionNameAtom._name

            if methodName[0:2] == "YP.":
                # current_predicate/1 should fail for built-ins.
                return False
            if methodName.find(".") >= 0:
                # We don't support calling inner classes, etc.
                return False
                
            try:
                # Look in the global scope.
                func = eval(methodName)
                # Note that Python doesn't let us check the arity, but make sure it's a function.
                return isinstance(func, types.FunctionType)
            except:
                # eval didn't find it.
                return False

        return False

# Compiler output follows.

def getDeclaringClass():
  return globals()

def repeatWrite(arg1, N):
  _Value = arg1
  if YP.termEqual(N, 0):
    return
  Value = arg1
  NextN = Variable()
  YP.write(Value)
  for l1 in YP.unify(NextN, YP.subtract(N, 1)):
    repeatWrite(Value, NextN)
    return

def sameVariable(Variable1, Variable2):
  if YP.var(Variable1):
    if YP.var(Variable2):
      if YP.termEqual(Variable1, Variable2):
        return True
  return False

def makeFunctionPseudoCode(RuleList, FunctionCode):
  State = Variable()
  for l1 in CompilerState.make(State):
    assertYPPred(State)
    processCompilerDirectives(RuleList, State)
    for l2 in YP.unify(FunctionCode, Atom.a("getDeclaringClass")):
      yield False
    for l2 in makeFunctionPseudoCode3(RuleList, State, FunctionCode):
      yield False

def assertYPPred(State):
  CompilerState.assertPred(State, Atom.a("nl"), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("write", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("put_code", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("see", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Atom.a("seen"), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("tell", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Atom.a("told"), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("throw", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("abolish", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("retractall", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor2("set_prolog_flag", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"))
  CompilerState.assertPred(State, Functor1("var", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("nonvar", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("atom", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("integer", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("float", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("number", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("atomic", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("compound", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor1("ground", Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("==", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("\\==", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("@<", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("@=<", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("@>", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  CompilerState.assertPred(State, Functor2("@>=", Functor2("::", Atom.a("univ"), Atom.a("in")), Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"))
  return

def processCompilerDirectives(arg1, arg2):
  _State = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    return
  State = arg2
  Pred = Variable()
  Determinism = Variable()
  x3 = Variable()
  RestRules = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", Functor1(":-", Functor1("pred", Functor2("is", Pred, Determinism))), x3), RestRules)):
    CompilerState.assertPred(State, Pred, Determinism)
    processCompilerDirectives(RestRules, State)
    return
  State = arg2
  Module = Variable()
  PredicateList = Variable()
  x3 = Variable()
  RestRules = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", Functor1(":-", Functor2("import", Module, PredicateList)), x3), RestRules)):
    for l2 in importPredicateList(State, Module, PredicateList):
      processCompilerDirectives(RestRules, State)
      return
  State = arg2
  x1 = Variable()
  x2 = Variable()
  RestRules = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", Functor1(":-", x1), x2), RestRules)):
    processCompilerDirectives(RestRules, State)
    return
  State = arg2
  Head = Variable()
  _Body = Variable()
  x3 = Variable()
  RestRules = Variable()
  Name = Variable()
  Arity = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", Functor2(":-", Head, _Body), x3), RestRules)):
    for l2 in YP.functor(Head, Name, Arity):
      CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""))
      processCompilerDirectives(RestRules, State)
      return
  State = arg2
  Fact = Variable()
  x2 = Variable()
  RestRules = Variable()
  Name = Variable()
  Arity = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", Fact, x2), RestRules)):
    for l2 in YP.functor(Fact, Name, Arity):
      CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""))
      processCompilerDirectives(RestRules, State)
      return
  State = arg2
  x1 = Variable()
  RestRules = Variable()
  for l1 in YP.unify(arg1, ListPair(x1, RestRules)):
    processCompilerDirectives(RestRules, State)
    return

def importPredicateList(arg1, arg2, arg3):
  _State = arg1
  _Module = arg2
  for l1 in YP.unify(arg3, Atom.NIL):
    yield True
    return
  State = arg1
  Module = arg2
  Name = Variable()
  Arity = Variable()
  Rest = Variable()
  for l1 in YP.unify(arg3, ListPair(Functor2("/", Name, Arity), Rest)):
    CompilerState.assertModuleForNameArity(State, Name, Arity, Module)
    for l2 in importPredicateList(State, Module, Rest):
      yield True
      return
  State = arg1
  Module = arg2
  x3 = Variable()
  Rest = Variable()
  for l1 in YP.unify(arg3, ListPair(x3, Rest)):
    for l2 in importPredicateList(State, Module, Rest):
      yield True
      return

def makeFunctionPseudoCode3(RuleList, State, FunctionCode):
  SamePredicateRuleList = Variable()
  RestRules = Variable()
  for l1 in samePredicateRuleList(RuleList, SamePredicateRuleList, RestRules):
    if YP.termNotEqual(SamePredicateRuleList, Atom.NIL):
      for l3 in compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode):
        yield False
      for l3 in makeFunctionPseudoCode3(RestRules, State, FunctionCode):
        yield False

def compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode):
  doBreak = False
  for _ in [1]:
    FirstRule = Variable()
    x5 = Variable()
    x6 = Variable()
    x7 = Variable()
    Head = Variable()
    x9 = Variable()
    ArgAssignments = Variable()
    Calls = Variable()
    Rule = Variable()
    VariableNameSuggestions = Variable()
    ClauseBag = Variable()
    Name = Variable()
    ArgsList = Variable()
    FunctionArgNames = Variable()
    MergedArgName = Variable()
    ArgName = Variable()
    MergedArgNames = Variable()
    FunctionArgs = Variable()
    BodyCode = Variable()
    ReturnType = Variable()
    BodyWithReturn = Variable()
    for l2 in YP.unify(ListPair(Functor2("f", FirstRule, x5), x6), SamePredicateRuleList):
      cutIf1 = False
      for _ in [1]:
        for l4 in YP.unify(FirstRule, Functor1(":-", x7)):
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        cutIf2 = False
        for _ in [1]:
          for l5 in YP.unify(Functor2(":-", Head, x9), FirstRule):
            CompilerState.startFunction(State, Head)
            findallAnswers3 = FindallAnswers(Functor2("f", ArgAssignments, Calls))
            for l6 in member(Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList):
              for l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls):
                findallAnswers3.add()
              if doBreak:
                break
            if doBreak:
              break
            for l6 in findallAnswers3.result(ClauseBag):
              for l7 in YP.univ(Head, ListPair(Name, ArgsList)):
                for l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames):
                  findallAnswers4 = FindallAnswers(MergedArgName)
                  for l9 in member(ArgName, FunctionArgNames):
                    cutIf5 = False
                    for _ in [1]:
                      for l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName):
                        findallAnswers4.add()
                        cutIf5 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(MergedArgName, ArgName):
                        findallAnswers4.add()
                      if doBreak:
                        break
                    if cutIf5:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  for l9 in findallAnswers4.result(MergedArgNames):
                    for l10 in maplist_arg(MergedArgNames, FunctionArgs):
                      for l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode):
                        cutIf6 = False
                        for _ in [1]:
                          if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                            for l14 in YP.unify(ReturnType, Atom.a("void")):
                              cutIf7 = False
                              for _ in [1]:
                                if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                  for l17 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                    for l18 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                      yield False
                                    if doBreak:
                                      break
                                  if doBreak:
                                    break
                                  cutIf7 = True
                                  doBreak = True
                                  break
                                cutIf8 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                    for l18 in YP.unify(BodyWithReturn, BodyCode):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf8 = True
                                    doBreak = True
                                    break
                                  cutIf9 = False
                                  for _ in [1]:
                                    if CompilerState.codeUsesYield(State):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf9 = True
                                      doBreak = True
                                      break
                                    for l18 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                  if cutIf9:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf8:
                                  doBreak = False
                                if doBreak:
                                  break
                              if cutIf7:
                                doBreak = False
                              if doBreak:
                                break
                            if doBreak:
                              break
                            cutIf6 = True
                            doBreak = True
                            break
                          cutIf10 = False
                          for _ in [1]:
                            if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                              for l15 in YP.unify(ReturnType, Atom.a("bool")):
                                cutIf11 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                    for l18 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf11 = True
                                    doBreak = True
                                    break
                                  cutIf12 = False
                                  for _ in [1]:
                                    if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf12 = True
                                      doBreak = True
                                      break
                                    cutIf13 = False
                                    for _ in [1]:
                                      if CompilerState.codeUsesYield(State):
                                        for l20 in YP.unify(BodyWithReturn, BodyCode):
                                          for l21 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                            yield False
                                          if doBreak:
                                            break
                                        if doBreak:
                                          break
                                        cutIf13 = True
                                        doBreak = True
                                        break
                                      for l19 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                    if cutIf13:
                                      doBreak = False
                                    if doBreak:
                                      break
                                  if cutIf12:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf11:
                                  doBreak = False
                                if doBreak:
                                  break
                              if doBreak:
                                break
                              cutIf10 = True
                              doBreak = True
                              break
                            for l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>")):
                              cutIf14 = False
                              for _ in [1]:
                                if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                  for l17 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                    for l18 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                      yield False
                                    if doBreak:
                                      break
                                  if doBreak:
                                    break
                                  cutIf14 = True
                                  doBreak = True
                                  break
                                cutIf15 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                    for l18 in YP.unify(BodyWithReturn, BodyCode):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf15 = True
                                    doBreak = True
                                    break
                                  cutIf16 = False
                                  for _ in [1]:
                                    if CompilerState.codeUsesYield(State):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf16 = True
                                      doBreak = True
                                      break
                                    for l18 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                  if cutIf16:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf15:
                                  doBreak = False
                                if doBreak:
                                  break
                              if cutIf14:
                                doBreak = False
                              if doBreak:
                                break
                            if doBreak:
                              break
                          if cutIf10:
                            doBreak = False
                          if doBreak:
                            break
                        if cutIf6:
                          doBreak = False
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
                if doBreak:
                  break
              if doBreak:
                break
            if doBreak:
              break
            cutIf2 = True
            doBreak = True
            break
          if doBreak:
            break
          for l5 in YP.unify(Head, FirstRule):
            CompilerState.startFunction(State, Head)
            findallAnswers17 = FindallAnswers(Functor2("f", ArgAssignments, Calls))
            for l6 in member(Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList):
              for l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls):
                findallAnswers17.add()
              if doBreak:
                break
            if doBreak:
              break
            for l6 in findallAnswers17.result(ClauseBag):
              for l7 in YP.univ(Head, ListPair(Name, ArgsList)):
                for l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames):
                  findallAnswers18 = FindallAnswers(MergedArgName)
                  for l9 in member(ArgName, FunctionArgNames):
                    cutIf19 = False
                    for _ in [1]:
                      for l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName):
                        findallAnswers18.add()
                        cutIf19 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(MergedArgName, ArgName):
                        findallAnswers18.add()
                      if doBreak:
                        break
                    if cutIf19:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  for l9 in findallAnswers18.result(MergedArgNames):
                    for l10 in maplist_arg(MergedArgNames, FunctionArgs):
                      for l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode):
                        cutIf20 = False
                        for _ in [1]:
                          if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                            for l14 in YP.unify(ReturnType, Atom.a("void")):
                              cutIf21 = False
                              for _ in [1]:
                                if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                  for l17 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                    for l18 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                      yield False
                                    if doBreak:
                                      break
                                  if doBreak:
                                    break
                                  cutIf21 = True
                                  doBreak = True
                                  break
                                cutIf22 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                    for l18 in YP.unify(BodyWithReturn, BodyCode):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf22 = True
                                    doBreak = True
                                    break
                                  cutIf23 = False
                                  for _ in [1]:
                                    if CompilerState.codeUsesYield(State):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf23 = True
                                      doBreak = True
                                      break
                                    for l18 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                  if cutIf23:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf22:
                                  doBreak = False
                                if doBreak:
                                  break
                              if cutIf21:
                                doBreak = False
                              if doBreak:
                                break
                            if doBreak:
                              break
                            cutIf20 = True
                            doBreak = True
                            break
                          cutIf24 = False
                          for _ in [1]:
                            if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                              for l15 in YP.unify(ReturnType, Atom.a("bool")):
                                cutIf25 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                    for l18 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf25 = True
                                    doBreak = True
                                    break
                                  cutIf26 = False
                                  for _ in [1]:
                                    if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf26 = True
                                      doBreak = True
                                      break
                                    cutIf27 = False
                                    for _ in [1]:
                                      if CompilerState.codeUsesYield(State):
                                        for l20 in YP.unify(BodyWithReturn, BodyCode):
                                          for l21 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                            yield False
                                          if doBreak:
                                            break
                                        if doBreak:
                                          break
                                        cutIf27 = True
                                        doBreak = True
                                        break
                                      for l19 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                    if cutIf27:
                                      doBreak = False
                                    if doBreak:
                                      break
                                  if cutIf26:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf25:
                                  doBreak = False
                                if doBreak:
                                  break
                              if doBreak:
                                break
                              cutIf24 = True
                              doBreak = True
                              break
                            for l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>")):
                              cutIf28 = False
                              for _ in [1]:
                                if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
                                  for l17 in append(BodyCode, ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn):
                                    for l18 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                      yield False
                                    if doBreak:
                                      break
                                  if doBreak:
                                    break
                                  cutIf28 = True
                                  doBreak = True
                                  break
                                cutIf29 = False
                                for _ in [1]:
                                  if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
                                    for l18 in YP.unify(BodyWithReturn, BodyCode):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                    cutIf29 = True
                                    doBreak = True
                                    break
                                  cutIf30 = False
                                  for _ in [1]:
                                    if CompilerState.codeUsesYield(State):
                                      for l19 in YP.unify(BodyWithReturn, BodyCode):
                                        for l20 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf30 = True
                                      doBreak = True
                                      break
                                    for l18 in append(BodyCode, ListPair(Functor1("blockScope", ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.fail"), Atom.NIL), ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn):
                                      for l19 in YP.unify(FunctionCode, Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn])):
                                        yield False
                                      if doBreak:
                                        break
                                    if doBreak:
                                      break
                                  if cutIf30:
                                    doBreak = False
                                  if doBreak:
                                    break
                                if cutIf29:
                                  doBreak = False
                                if doBreak:
                                  break
                              if cutIf28:
                                doBreak = False
                              if doBreak:
                                break
                            if doBreak:
                              break
                          if cutIf24:
                            doBreak = False
                          if doBreak:
                            break
                        if cutIf20:
                          doBreak = False
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
                if doBreak:
                  break
              if doBreak:
                break
            if doBreak:
              break
          if doBreak:
            break
        if cutIf2:
          doBreak = False
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def samePredicateRuleList(arg1, arg2, arg3):
  doBreak = False
  for _ in [1]:
    for l2 in YP.unify(arg1, Atom.NIL):
      for l3 in YP.unify(arg2, Atom.NIL):
        for l4 in YP.unify(arg3, Atom.NIL):
          yield True
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    First = Variable()
    for l2 in YP.unify(arg1, ListPair(First, Atom.NIL)):
      for l3 in YP.unify(arg2, ListPair(First, Atom.NIL)):
        for l4 in YP.unify(arg3, Atom.NIL):
          yield True
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    SamePredicateRuleList = arg2
    RestRules = arg3
    First = Variable()
    Rest = Variable()
    FirstRule = Variable()
    x6 = Variable()
    SecondRule = Variable()
    x8 = Variable()
    x9 = Variable()
    FirstHead = Variable()
    x11 = Variable()
    SecondHead = Variable()
    x13 = Variable()
    Name = Variable()
    Arity = Variable()
    RestSamePredicates = Variable()
    for l2 in YP.unify(arg1, ListPair(First, Rest)):
      for l3 in YP.unify(Functor2("f", FirstRule, x6), First):
        for l4 in YP.unify(ListPair(Functor2("f", SecondRule, x8), x9), Rest):
          cutIf1 = False
          for _ in [1]:
            for l6 in YP.unify(Functor2(":-", FirstHead, x11), FirstRule):
              cutIf2 = False
              for _ in [1]:
                for l8 in YP.unify(Functor2(":-", SecondHead, x13), SecondRule):
                  for l9 in YP.functor(FirstHead, Name, Arity):
                    cutIf3 = False
                    for _ in [1]:
                      for l11 in YP.functor(SecondHead, Name, Arity):
                        for l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules):
                          for l13 in YP.unify(SamePredicateRuleList, ListPair(First, RestSamePredicates)):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                        cutIf3 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(SamePredicateRuleList, ListPair(First, Atom.NIL)):
                        for l12 in YP.unify(RestRules, Rest):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if cutIf3:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf2 = True
                  doBreak = True
                  break
                if doBreak:
                  break
                for l8 in YP.unify(SecondHead, SecondRule):
                  for l9 in YP.functor(FirstHead, Name, Arity):
                    cutIf4 = False
                    for _ in [1]:
                      for l11 in YP.functor(SecondHead, Name, Arity):
                        for l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules):
                          for l13 in YP.unify(SamePredicateRuleList, ListPair(First, RestSamePredicates)):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                        cutIf4 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(SamePredicateRuleList, ListPair(First, Atom.NIL)):
                        for l12 in YP.unify(RestRules, Rest):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if cutIf4:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                if doBreak:
                  break
              if cutIf2:
                doBreak = False
              if doBreak:
                break
              cutIf1 = True
              doBreak = True
              break
            if doBreak:
              break
            for l6 in YP.unify(FirstHead, FirstRule):
              cutIf5 = False
              for _ in [1]:
                for l8 in YP.unify(Functor2(":-", SecondHead, x13), SecondRule):
                  for l9 in YP.functor(FirstHead, Name, Arity):
                    cutIf6 = False
                    for _ in [1]:
                      for l11 in YP.functor(SecondHead, Name, Arity):
                        for l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules):
                          for l13 in YP.unify(SamePredicateRuleList, ListPair(First, RestSamePredicates)):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                        cutIf6 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(SamePredicateRuleList, ListPair(First, Atom.NIL)):
                        for l12 in YP.unify(RestRules, Rest):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if cutIf6:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf5 = True
                  doBreak = True
                  break
                if doBreak:
                  break
                for l8 in YP.unify(SecondHead, SecondRule):
                  for l9 in YP.functor(FirstHead, Name, Arity):
                    cutIf7 = False
                    for _ in [1]:
                      for l11 in YP.functor(SecondHead, Name, Arity):
                        for l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules):
                          for l13 in YP.unify(SamePredicateRuleList, ListPair(First, RestSamePredicates)):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                        cutIf7 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(SamePredicateRuleList, ListPair(First, Atom.NIL)):
                        for l12 in YP.unify(RestRules, Rest):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if cutIf7:
                      doBreak = False
                    if doBreak:
                      break
                  if doBreak:
                    break
                if doBreak:
                  break
              if cutIf5:
                doBreak = False
              if doBreak:
                break
            if doBreak:
              break
          if cutIf1:
            doBreak = False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def maplist_compileClause(arg1, arg2, arg3):
  _MergedArgNames = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield True
      return
  MergedArgNames = arg2
  ArgAssignments = Variable()
  Calls = Variable()
  Rest = Variable()
  ClauseCode = Variable()
  RestResults = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("f", ArgAssignments, Calls), Rest)):
    for l2 in YP.unify(arg3, ListPair(Functor1("blockScope", ClauseCode), RestResults)):
      for l3 in prependArgAssignments(ArgAssignments, Calls, MergedArgNames, ClauseCode):
        for l4 in maplist_compileClause(Rest, MergedArgNames, RestResults):
          yield True
          return

def prependArgAssignments(arg1, arg2, arg3, arg4):
  doBreak = False
  for _ in [1]:
    _MergedArgNames = arg3
    In = Variable()
    for l2 in YP.unify(arg1, Atom.NIL):
      for l3 in YP.unify(arg2, In):
        for l4 in YP.unify(arg4, In):
          yield True
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    In = arg2
    MergedArgNames = arg3
    ClauseCode = arg4
    VariableName = Variable()
    ArgName = Variable()
    RestArgAssignments = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("f", VariableName, ArgName), RestArgAssignments)):
      cutIf1 = False
      for _ in [1]:
        for l4 in member(VariableName, MergedArgNames):
          for l5 in prependArgAssignments(RestArgAssignments, In, MergedArgNames, ClauseCode):
            yield True
            return
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        for l4 in prependArgAssignments(RestArgAssignments, ListPair(Functor3("declare", Atom.a("object"), VariableName, Functor1("var", ArgName)), In), MergedArgNames, ClauseCode):
          yield True
          return
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def argAssignedAll(arg1, arg2, VariableName):
  _ArgName = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    if YP.nonvar(VariableName):
      yield True
      return
  ArgName = arg1
  ArgAssignments = Variable()
  _Calls = Variable()
  RestClauseBag = Variable()
  for l1 in YP.unify(arg2, ListPair(Functor2("f", ArgAssignments, _Calls), RestClauseBag)):
    for l2 in member(Functor2("f", VariableName, ArgName), ArgAssignments):
      for l3 in argAssignedAll(ArgName, RestClauseBag, VariableName):
        yield False

def maplist_arg(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield True
      return
  First = Variable()
  Rest = Variable()
  RestResults = Variable()
  for l1 in YP.unify(arg1, ListPair(First, Rest)):
    for l2 in YP.unify(arg2, ListPair(Functor1("arg", First), RestResults)):
      for l3 in maplist_arg(Rest, RestResults):
        yield True
        return

def getFunctionArgNames(arg1, arg2, arg3):
  _StartArgNumber = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield True
      return
  StartArgNumber = arg2
  x1 = Variable()
  Rest = Variable()
  ArgName = Variable()
  RestFunctionArgs = Variable()
  NumberCodes = Variable()
  NumberAtom = Variable()
  NextArgNumber = Variable()
  for l1 in YP.unify(arg1, ListPair(x1, Rest)):
    for l2 in YP.unify(arg3, ListPair(ArgName, RestFunctionArgs)):
      for l3 in YP.number_codes(StartArgNumber, NumberCodes):
        for l4 in YP.atom_codes(NumberAtom, NumberCodes):
          for l5 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName):
            for l6 in YP.unify(NextArgNumber, YP.add(StartArgNumber, 1)):
              for l7 in getFunctionArgNames(Rest, NextArgNumber, RestFunctionArgs):
                yield True
                return

def compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls):
  Head = Variable()
  Body = Variable()
  x8 = Variable()
  HeadArgs = Variable()
  CompiledHeadArgs = Variable()
  BodyCode = Variable()
  VariableNamesList = Variable()
  ArgUnifications = Variable()
  for l1 in YP.unify(Functor2(":-", Head, Body), Rule):
    CompilerState.newVariableNames(State, Rule, VariableNameSuggestions)
    for l2 in YP.univ(Head, ListPair(x8, HeadArgs)):
      for l3 in maplist_compileTerm(HeadArgs, State, CompiledHeadArgs):
        for l4 in compileRuleBody(Body, State, BodyCode):
          for l5 in CompilerState.variableNamesList(State, VariableNamesList):
            for l6 in compileArgUnifications(HeadArgs, CompiledHeadArgs, 1, HeadArgs, BodyCode, ArgUnifications):
              for l7 in compileDeclarations(VariableNamesList, HeadArgs, Atom.NIL, ArgAssignments, ArgUnifications, Calls):
                yield True
                return
  for l1 in compileBodyWithHeadBindings(Functor2(":-", Rule, Atom.a("true")), VariableNameSuggestions, State, ArgAssignments, Calls):
    yield True
    return

def compileArgUnifications(arg1, arg2, arg3, arg4, arg5, arg6):
  x1 = arg2
  x2 = arg3
  x3 = arg4
  BodyCode = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg5, BodyCode):
      for l3 in YP.unify(arg6, BodyCode):
        yield True
        return
  Index = arg3
  AllHeadArgs = arg4
  BodyCode = arg5
  ArgUnifications = arg6
  HeadArg = Variable()
  RestHeadArgs = Variable()
  x3 = Variable()
  RestCompiledHeadArgs = Variable()
  _ArgIndex1 = Variable()
  NextIndex = Variable()
  for l1 in YP.unify(arg1, ListPair(HeadArg, RestHeadArgs)):
    for l2 in YP.unify(arg2, ListPair(x3, RestCompiledHeadArgs)):
      for l3 in getVariableArgIndex1(HeadArg, AllHeadArgs, _ArgIndex1):
        for l4 in YP.unify(NextIndex, YP.add(Index, 1)):
          for l5 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, ArgUnifications):
            yield True
            return
  Index = arg3
  AllHeadArgs = arg4
  BodyCode = arg5
  _HeadArg = Variable()
  RestHeadArgs = Variable()
  CompiledHeadArg = Variable()
  RestCompiledHeadArgs = Variable()
  ArgName = Variable()
  RestArgUnifications = Variable()
  NumberCodes = Variable()
  NumberAtom = Variable()
  NextIndex = Variable()
  for l1 in YP.unify(arg1, ListPair(_HeadArg, RestHeadArgs)):
    for l2 in YP.unify(arg2, ListPair(CompiledHeadArg, RestCompiledHeadArgs)):
      for l3 in YP.unify(arg6, ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.unify"), ListPair(Functor1("var", ArgName), ListPair(CompiledHeadArg, Atom.NIL))), RestArgUnifications), Atom.NIL)):
        for l4 in YP.number_codes(Index, NumberCodes):
          for l5 in YP.atom_codes(NumberAtom, NumberCodes):
            for l6 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName):
              for l7 in YP.unify(NextIndex, YP.add(Index, 1)):
                for l8 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, RestArgUnifications):
                  yield True
                  return

def compileDeclarations(arg1, arg2, arg3, arg4, arg5, arg6):
  _HeadArgs = arg2
  ArgAssignmentsIn = Variable()
  DeclarationsIn = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg3, ArgAssignmentsIn):
      for l3 in YP.unify(arg4, ArgAssignmentsIn):
        for l4 in YP.unify(arg5, DeclarationsIn):
          for l5 in YP.unify(arg6, DeclarationsIn):
            yield True
            return
  HeadArgs = arg2
  ArgAssignmentsIn = arg3
  ArgAssignmentsOut = arg4
  DeclarationsIn = arg5
  DeclarationsOut = arg6
  VariableName = Variable()
  Var = Variable()
  RestVariableNames = Variable()
  ArgIndex1 = Variable()
  NumberCodes = Variable()
  NumberAtom = Variable()
  ArgName = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("=", VariableName, Var), RestVariableNames)):
    for l2 in getVariableArgIndex1(Var, HeadArgs, ArgIndex1):
      for l3 in YP.number_codes(ArgIndex1, NumberCodes):
        for l4 in YP.atom_codes(NumberAtom, NumberCodes):
          for l5 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName):
            for l6 in compileDeclarations(RestVariableNames, HeadArgs, ListPair(Functor2("f", VariableName, ArgName), ArgAssignmentsIn), ArgAssignmentsOut, DeclarationsIn, DeclarationsOut):
              yield True
              return
  HeadArgs = arg2
  ArgAssignmentsIn = arg3
  ArgAssignmentsOut = arg4
  DeclarationsIn = arg5
  VariableName = Variable()
  _Var = Variable()
  RestVariableNames = Variable()
  DeclarationsOut = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("=", VariableName, _Var), RestVariableNames)):
    for l2 in YP.unify(arg6, ListPair(Functor3("declare", Atom.a("Variable"), VariableName, Functor2("new", Atom.a("Variable"), Atom.NIL)), DeclarationsOut)):
      for l3 in compileDeclarations(RestVariableNames, HeadArgs, ArgAssignmentsIn, ArgAssignmentsOut, DeclarationsIn, DeclarationsOut):
        yield True
        return

def getVariableArgIndex1(Var, arg2, arg3):
  doBreak = False
  for _ in [1]:
    FirstHeadArgs = Variable()
    RestHeadArgs = Variable()
    x4 = Variable()
    for l2 in YP.unify(arg2, ListPair(FirstHeadArgs, RestHeadArgs)):
      for l3 in YP.unify(arg3, 1):
        if sameVariable(Var, FirstHeadArgs):
          cutIf1 = False
          for _ in [1]:
            for l6 in getVariableArgIndex1(Var, RestHeadArgs, x4):
              cutIf1 = True
              doBreak = True
              break
            if doBreak:
              break
            yield False
          if cutIf1:
            doBreak = False
          if doBreak:
            break
          return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Index = arg3
    x2 = Variable()
    RestHeadArgs = Variable()
    RestIndex = Variable()
    for l2 in YP.unify(arg2, ListPair(x2, RestHeadArgs)):
      for l3 in getVariableArgIndex1(Var, RestHeadArgs, RestIndex):
        for l4 in YP.unify(Index, YP.add(1, RestIndex)):
          yield True
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def compileRuleBody(arg1, arg2, arg3):
  A = arg1
  State = arg2
  PseudoCode = arg3
  if YP.var(A):
    for l2 in compileRuleBody(Functor2(",", Functor1("call", A), Atom.a("true")), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", A, B)):
    if YP.var(A):
      for l3 in compileRuleBody(Functor2(",", Functor1("call", A), B), State, PseudoCode):
        yield True
        return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  ACode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", A, B)):
    for l2 in compileFunctorCall(A, State, ACode):
      if CompilerState.isDetNoneOut(State, A):
        for l4 in compileRuleBody(B, State, BCode):
          for l5 in YP.unify(PseudoCode, ListPair(ACode, BCode)):
            yield True
            return
      if CompilerState.isSemidetNoneOut(State, A):
        for l4 in compileRuleBody(B, State, BCode):
          for l5 in YP.unify(PseudoCode, ListPair(Functor2("if", ACode, BCode), Atom.NIL)):
            yield True
            return
      for l3 in compileRuleBody(B, State, BCode):
        for l4 in YP.unify(PseudoCode, ListPair(Functor2("foreach", ACode, BCode), Atom.NIL)):
          yield True
          return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  T = Variable()
  B = Variable()
  C = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2(";", Functor2("->", A, T), B), C)):
    for l2 in compileRuleBody(Functor2(";", Functor2("->", A, Functor2(",", T, C)), Functor2(",", B, C)), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  C = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2(";", A, B), C)):
    for l2 in compileRuleBody(Functor2(";", Functor2(",", A, C), Functor2(",", B, C)), State, PseudoCode):
      yield True
      return
  State = arg2
  A = Variable()
  B = Variable()
  ACode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("\\+", A), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("if", Functor1("not", ACode), BCode), Atom.NIL)):
      if CompilerState.isSemidetNoneOut(State, A):
        for l4 in compileFunctorCall(A, State, ACode):
          for l5 in compileRuleBody(B, State, BCode):
            yield True
            return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("\\+", A), B)):
    for l2 in compileRuleBody(Functor2(",", Functor2(";", Functor2("->", A, Atom.a("fail")), Atom.a("true")), B), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("once", A), B)):
    for l2 in compileRuleBody(Functor2(",", Functor2(";", Functor2("->", A, Atom.a("true")), Atom.a("fail")), B), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  T = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2("->", A, T), B)):
    for l2 in compileRuleBody(Functor2(",", Functor2(";", Functor2("->", A, T), Atom.a("fail")), B), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  C = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2("\\=", A, B), C)):
    for l2 in compileRuleBody(Functor2(",", Functor1("\\+", Functor2("=", A, B)), C), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  ACode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Atom.a("!"), A)):
    for l2 in compileRuleBody(A, State, ACode):
      for l3 in append(ACode, ListPair(Atom.a("yieldbreak"), Atom.NIL), PseudoCode):
        yield True
        return
  State = arg2
  PseudoCode = arg3
  Name = Variable()
  A = Variable()
  ACode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("$CUTIF", Name), A)):
    for l2 in compileRuleBody(A, State, ACode):
      for l3 in append(ACode, ListPair(Functor1("breakBlock", Name), Atom.NIL), PseudoCode):
        yield True
        return
  _State = arg2
  x1 = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Atom.a("fail"), x1)):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Atom.a("true"), A)):
    for l2 in compileRuleBody(A, State, PseudoCode):
      yield True
      return
  State = arg2
  A = Variable()
  Term = Variable()
  B = Variable()
  ACode = Variable()
  TermCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2("is", A, Term), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.unify"), ListPair(ACode, ListPair(TermCode, Atom.NIL))), BCode), Atom.NIL)):
      for l3 in compileTerm(A, State, ACode):
        for l4 in compileExpression(Term, State, TermCode):
          for l5 in compileRuleBody(B, State, BCode):
            yield True
            return
  State = arg2
  ACode = Variable()
  B = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("$DET_NONE_OUT", ACode), B)):
    for l2 in YP.unify(arg3, ListPair(ACode, BCode)):
      for l3 in compileRuleBody(B, State, BCode):
        yield True
        return
  State = arg2
  A = Variable()
  B = Variable()
  FunctionName = Variable()
  X1Code = Variable()
  X2Code = Variable()
  BCode = Variable()
  Name = Variable()
  X1 = Variable()
  X2 = Variable()
  for l1 in YP.unify(arg1, Functor2(",", A, B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("if", Functor2("call", FunctionName, ListPair(X1Code, ListPair(X2Code, Atom.NIL))), BCode), Atom.NIL)):
      for l3 in YP.univ(A, ListPair.make([Name, X1, X2])):
        for l4 in binaryExpressionConditional(Name, FunctionName):
          for l5 in compileExpression(X1, State, X1Code):
            for l6 in compileExpression(X2, State, X2Code):
              for l7 in compileRuleBody(B, State, BCode):
                yield True
                return
  State = arg2
  PseudoCode = arg3
  Template = Variable()
  Goal = Variable()
  Bag = Variable()
  B = Variable()
  TemplateCode = Variable()
  FindallAnswers = Variable()
  GoalAndAddCode = Variable()
  BagCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor3("findall", Template, Goal, Bag), B)):
    for l2 in compileTerm(Template, State, TemplateCode):
      for l3 in CompilerState.gensym(State, Atom.a("findallAnswers"), FindallAnswers):
        for l4 in compileRuleBody(Functor2(",", Goal, Functor2(",", Functor1("$DET_NONE_OUT", Functor3("callMember", Functor1("var", FindallAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode):
          for l5 in compileTerm(Bag, State, BagCode):
            for l6 in compileRuleBody(B, State, BCode):
              for l7 in append(ListPair(Functor3("declare", Atom.a("FindallAnswers"), FindallAnswers, Functor2("new", Atom.a("FindallAnswers"), ListPair(TemplateCode, Atom.NIL))), GoalAndAddCode), ListPair(Functor2("foreach", Functor3("callMember", Functor1("var", FindallAnswers), Atom.a("result"), ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode):
                yield True
                return
  State = arg2
  PseudoCode = arg3
  Template = Variable()
  Goal = Variable()
  Bag = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor3("bagof", Template, Goal, Bag), B)):
    for l2 in compileBagof(Atom.a("result"), Template, Goal, Bag, B, State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  Template = Variable()
  Goal = Variable()
  Bag = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor3("setof", Template, Goal, Bag), B)):
    for l2 in compileBagof(Atom.a("resultSet"), Template, Goal, Bag, B, State, PseudoCode):
      yield True
      return
  State = arg2
  A = Variable()
  B = Variable()
  ATermCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("call", A), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.getIterator"), ListPair(ATermCode, ListPair(Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL)):
      for l3 in compileTerm(A, State, ATermCode):
        for l4 in compileRuleBody(B, State, BCode):
          yield True
          return
  State = arg2
  A = Variable()
  B = Variable()
  ATermCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("current_predicate", A), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("foreach", Functor2("call", Atom.a("YP.current_predicate"), ListPair(ATermCode, ListPair(Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL)):
      for l3 in compileTerm(A, State, ATermCode):
        for l4 in compileRuleBody(B, State, BCode):
          yield True
          return
  State = arg2
  A = Variable()
  B = Variable()
  ATermCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("asserta", A), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("call", Atom.a("YP.asserta"), ListPair(ATermCode, ListPair(Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode)):
      for l3 in compileTerm(A, State, ATermCode):
        for l4 in compileRuleBody(B, State, BCode):
          yield True
          return
  State = arg2
  A = Variable()
  B = Variable()
  ATermCode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("assertz", A), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("call", Atom.a("YP.assertz"), ListPair(ATermCode, ListPair(Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode)):
      for l3 in compileTerm(A, State, ATermCode):
        for l4 in compileRuleBody(B, State, BCode):
          yield True
          return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor1("assert", A), B)):
    for l2 in compileRuleBody(Functor2(",", Functor1("assertz", A), B), State, PseudoCode):
      yield True
      return
  State = arg2
  Goal = Variable()
  Catcher = Variable()
  Handler = Variable()
  B = Variable()
  CatchGoal = Variable()
  GoalTermCode = Variable()
  BCode = Variable()
  CatcherTermCode = Variable()
  HandlerAndBCode = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor3("catch", Goal, Catcher, Handler), B)):
    for l2 in YP.unify(arg3, ListPair.make([Functor3("declare", Atom.a("YP.Catch"), CatchGoal, Functor2("new", Atom.a("YP.Catch"), ListPair(GoalTermCode, ListPair(Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL)))), Functor2("foreach", Functor1("var", CatchGoal), BCode), Functor2("foreach", Functor3("callMember", Functor1("var", CatchGoal), Atom.a("unifyExceptionOrThrow"), ListPair(CatcherTermCode, Atom.NIL)), HandlerAndBCode)])):
      for l3 in CompilerState.gensym(State, Atom.a("catchGoal"), CatchGoal):
        for l4 in compileTerm(Goal, State, GoalTermCode):
          for l5 in compileTerm(Catcher, State, CatcherTermCode):
            for l6 in compileRuleBody(B, State, BCode):
              for l7 in compileRuleBody(Functor2(",", Handler, B), State, HandlerAndBCode):
                yield True
                return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  C = Variable()
  for l1 in YP.unify(arg1, Functor2(",", Functor2(",", A, B), C)):
    for l2 in compileRuleBody(Functor2(",", A, Functor2(",", B, C)), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Functor2(";", A, B)):
    if YP.var(A):
      for l3 in compileRuleBody(Functor2(";", Functor1("call", A), B), State, PseudoCode):
        yield True
        return
  State = arg2
  A = Variable()
  T = Variable()
  B = Variable()
  CutIfLabel = Variable()
  Code = Variable()
  for l1 in YP.unify(arg1, Functor2(";", Functor2("->", A, T), B)):
    for l2 in YP.unify(arg3, ListPair(Functor2("breakableBlock", CutIfLabel, Code), Atom.NIL)):
      for l3 in CompilerState.gensym(State, Atom.a("cutIf"), CutIfLabel):
        for l4 in compileRuleBody(Functor2(";", Functor2(",", A, Functor2(",", Functor1("$CUTIF", CutIfLabel), T)), B), State, Code):
          yield True
          return
  State = arg2
  PseudoCode = arg3
  _B = Variable()
  for l1 in YP.unify(arg1, Functor2(";", Atom.a("!"), _B)):
    for l2 in compileRuleBody(Atom.a("!"), State, PseudoCode):
      yield True
      return
  State = arg2
  PseudoCode = arg3
  A = Variable()
  B = Variable()
  ACode = Variable()
  BCode = Variable()
  for l1 in YP.unify(arg1, Functor2(";", A, B)):
    for l2 in compileRuleBody(A, State, ACode):
      for l3 in compileRuleBody(B, State, BCode):
        for l4 in append(ACode, BCode, PseudoCode):
          yield True
          return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("!")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("return"), Atom.NIL)):
      if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
        yield True
        return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("!")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("returntrue"), Atom.NIL)):
      if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
        yield True
        return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("!")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("yieldtrue"), ListPair(Atom.a("yieldbreak"), Atom.NIL))):
      CompilerState.setCodeUsesYield(State)
      yield True
      return
  _State = arg2
  Name = Variable()
  for l1 in YP.unify(arg1, Functor1("$CUTIF", Name)):
    for l2 in YP.unify(arg3, ListPair(Functor1("breakBlock", Name), Atom.NIL)):
      yield True
      return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("true")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("return"), Atom.NIL)):
      if CompilerState.determinismEquals(State, Atom.a("detNoneOut")):
        yield True
        return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("true")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("returntrue"), Atom.NIL)):
      if CompilerState.determinismEquals(State, Atom.a("semidetNoneOut")):
        yield True
        return
  State = arg2
  for l1 in YP.unify(arg1, Atom.a("true")):
    for l2 in YP.unify(arg3, ListPair(Atom.a("yieldfalse"), Atom.NIL)):
      CompilerState.setCodeUsesYield(State)
      yield True
      return
  A = arg1
  State = arg2
  PseudoCode = arg3
  for l1 in compileRuleBody(Functor2(",", A, Atom.a("true")), State, PseudoCode):
    yield True
    return

def compileBagof(ResultMethod, Template, Goal, Bag, B, State, PseudoCode):
  TemplateCode = Variable()
  GoalTermCode = Variable()
  UnqualifiedGoal = Variable()
  BagofAnswers = Variable()
  GoalAndAddCode = Variable()
  BagCode = Variable()
  BCode = Variable()
  for l1 in compileTerm(Template, State, TemplateCode):
    for l2 in compileTerm(Goal, State, GoalTermCode):
      for l3 in unqualifiedGoal(Goal, UnqualifiedGoal):
        for l4 in CompilerState.gensym(State, Atom.a("bagofAnswers"), BagofAnswers):
          for l5 in compileRuleBody(Functor2(",", UnqualifiedGoal, Functor2(",", Functor1("$DET_NONE_OUT", Functor3("callMember", Functor1("var", BagofAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode):
            for l6 in compileTerm(Bag, State, BagCode):
              for l7 in compileRuleBody(B, State, BCode):
                for l8 in append(ListPair(Functor3("declare", Atom.a("BagofAnswers"), BagofAnswers, Functor2("new", Atom.a("BagofAnswers"), ListPair(TemplateCode, ListPair(GoalTermCode, Atom.NIL)))), GoalAndAddCode), ListPair(Functor2("foreach", Functor3("callMember", Functor1("var", BagofAnswers), ResultMethod, ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode):
                  yield True
                  return

def unqualifiedGoal(arg1, arg2):
  Goal = arg1
  for l1 in YP.unify(arg2, Functor1("call", Goal)):
    if YP.var(Goal):
      yield True
      return
  UnqualifiedGoal = arg2
  x1 = Variable()
  Goal = Variable()
  for l1 in YP.unify(arg1, Functor2("^", x1, Goal)):
    for l2 in unqualifiedGoal(Goal, UnqualifiedGoal):
      yield True
      return
  UnqualifiedGoal = Variable()
  for l1 in YP.unify(arg1, UnqualifiedGoal):
    for l2 in YP.unify(arg2, UnqualifiedGoal):
      yield True
      return

def binaryExpressionConditional(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.a("=:=")):
    for l2 in YP.unify(arg2, Atom.a("YP.equal")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("=\\=")):
    for l2 in YP.unify(arg2, Atom.a("YP.notEqual")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a(">")):
    for l2 in YP.unify(arg2, Atom.a("YP.greaterThan")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("<")):
    for l2 in YP.unify(arg2, Atom.a("YP.lessThan")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a(">=")):
    for l2 in YP.unify(arg2, Atom.a("YP.greaterThanOrEqual")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("=<")):
    for l2 in YP.unify(arg2, Atom.a("YP.lessThanOrEqual")):
      yield True
      return

def compileFunctorCall(Functor_1, State, PseudoCode):
  doBreak = False
  for _ in [1]:
    FunctorName = Variable()
    FunctorArgs = Variable()
    x6 = Variable()
    Arity = Variable()
    FunctionName = Variable()
    CompiledArgs = Variable()
    for l2 in YP.univ(Functor_1, ListPair(FunctorName, FunctorArgs)):
      for l3 in YP.functor(Functor_1, x6, Arity):
        for l4 in functorCallFunctionName(State, FunctorName, Arity, FunctionName):
          for l5 in maplist_compileTerm(FunctorArgs, State, CompiledArgs):
            cutIf1 = False
            for _ in [1]:
              if YP.termEqual(FunctionName, Atom.NIL):
                for l8 in YP.unify(PseudoCode, Functor2("call", Atom.a("YP.matchDynamic"), ListPair(Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", FunctorName), Atom.NIL)), ListPair(Functor1("objectArray", CompiledArgs), Atom.NIL)))):
                  yield True
                  return
                if doBreak:
                  break
                cutIf1 = True
                doBreak = True
                break
              for l7 in YP.unify(PseudoCode, Functor3("functorCall", FunctionName, FunctorArgs, CompiledArgs)):
                yield True
                return
              if doBreak:
                break
            if cutIf1:
              doBreak = False
            if doBreak:
              break
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def functorCallFunctionName(arg1, arg2, arg3, arg4):
  _State = arg1
  Name = arg2
  Arity = arg3
  x4 = arg4
  if functorCallIsSpecialForm(Name, Arity):
    return
  x1 = arg1
  Name = arg2
  Arity = arg3
  FunctionName = arg4
  for l1 in functorCallYPFunctionName(Name, Arity, FunctionName):
    yield True
    return
  State = arg1
  Arity = arg3
  Name = Variable()
  for l1 in YP.unify(arg2, Name):
    for l2 in YP.unify(arg4, Name):
      if CompilerState.nameArityHasModule(State, Name, Arity, Atom.a("")):
        yield True
        return
  _State = arg1
  _Arity = arg3
  Name = Variable()
  for l1 in YP.unify(arg2, Name):
    for l2 in YP.unify(arg4, Name):
      for l3 in Atom.module(Name, Atom.a("")):
        yield True
        return
  _State = arg1
  Name = arg2
  _Arity = arg3
  for l1 in YP.unify(arg4, Atom.NIL):
    for l2 in Atom.module(Name, Atom.NIL):
      yield True
      return
  _State = arg1
  Name = arg2
  Arity = arg3
  x4 = arg4
  Module = Variable()
  Message = Variable()
  for l1 in Atom.module(Name, Module):
    for l2 in YP.atom_concat(Atom.a("Not supporting calls to external module: "), Module, Message):
      YP.throwException(Functor2("error", Functor2("type_error", Atom.a("callable"), Functor2("/", Name, Arity)), Message))
      yield True
      return
  _State = arg1
  Name = arg2
  _Arity = arg3
  x4 = arg4
  YP.throwException(Functor2("error", Functor2("type_error", Atom.a("callable"), Name), Atom.a("Term is not callable")))
  yield True
  return

def functorCallIsSpecialForm(Name, Arity):
  x3 = Variable()
  if YP.termEqual(Arity, 0):
    if YP.termEqual(Name, Atom.a("!")):
      return True
    if YP.termEqual(Name, Atom.a("fail")):
      return True
    if YP.termEqual(Name, Atom.a("true")):
      return True
  if YP.termEqual(Arity, 1):
    if YP.termEqual(Name, Atom.a("\\+")):
      return True
    if YP.termEqual(Name, Atom.a("once")):
      return True
    if YP.termEqual(Name, Atom.a("$CUTIF")):
      return True
    if YP.termEqual(Name, Atom.a("$DET_NONE_OUT")):
      return True
    if YP.termEqual(Name, Atom.a("call")):
      return True
    if YP.termEqual(Name, Atom.a("current_predicate")):
      return True
    if YP.termEqual(Name, Atom.a("asserta")):
      return True
    if YP.termEqual(Name, Atom.a("assertz")):
      return True
    if YP.termEqual(Name, Atom.a("assert")):
      return True
  if YP.termEqual(Arity, 2):
    if YP.termEqual(Name, Atom.a(";")):
      return True
    if YP.termEqual(Name, Atom.a(",")):
      return True
    if YP.termEqual(Name, Atom.a("->")):
      return True
    if YP.termEqual(Name, Atom.a("\\=")):
      return True
    if YP.termEqual(Name, Atom.a("is")):
      return True
    for l2 in binaryExpressionConditional(Name, x3):
      return True
  if YP.termEqual(Arity, 3):
    if YP.termEqual(Name, Atom.a("findall")):
      return True
    if YP.termEqual(Name, Atom.a("bagof")):
      return True
    if YP.termEqual(Name, Atom.a("setof")):
      return True
    if YP.termEqual(Name, Atom.a("catch")):
      return True
  return False

def functorCallYPFunctionName(arg1, arg2, arg3):
  for l1 in YP.unify(arg1, Atom.a("=")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.unify")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("=..")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.univ")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("var")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.var")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.nonvar")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, 3):
      for l3 in YP.unify(arg3, Atom.a("YP.arg")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, 3):
      for l3 in YP.unify(arg3, Atom.a("YP.functor")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("repeat")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, Atom.a("YP.repeat")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("get_code")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.get_code")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("current_op")):
    for l2 in YP.unify(arg2, 3):
      for l3 in YP.unify(arg3, Atom.a("YP.current_op")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.atom_length")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, 3):
      for l3 in YP.unify(arg3, Atom.a("YP.atom_concat")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, 5):
      for l3 in YP.unify(arg3, Atom.a("YP.sub_atom")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.atom_chars")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.atom_codes")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.char_code")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.number_chars")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.number_codes")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.copy_term")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("sort")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.sort")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("nl")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, Atom.a("YP.nl")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("write")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.write")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("put_code")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.put_code")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("see")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.see")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("seen")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, Atom.a("YP.seen")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("tell")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.tell")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("told")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, Atom.a("YP.told")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.clause")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("retract")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.retract")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.abolish")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("retractall")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.retractall")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.atom")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.integer")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.isFloat")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.number")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.atomic")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.compound")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("ground")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.ground")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("==")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termEqual")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("\\==")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termNotEqual")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("@<")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termLessThan")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("@=<")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termLessThanOrEqual")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("@>")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termGreaterThan")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("@>=")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.termGreaterThanOrEqual")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("throw")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.throwException")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.current_prolog_flag")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("YP.set_prolog_flag")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("current_input")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.current_input")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("current_output")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("YP.current_output")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("read_term")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("Parser.read_term2")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("read_term")):
    for l2 in YP.unify(arg2, 3):
      for l3 in YP.unify(arg3, Atom.a("Parser.read_term3")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("read")):
    for l2 in YP.unify(arg2, 1):
      for l3 in YP.unify(arg3, Atom.a("Parser.read1")):
        yield True
        return
  for l1 in YP.unify(arg1, Atom.a("read")):
    for l2 in YP.unify(arg2, 2):
      for l3 in YP.unify(arg3, Atom.a("Parser.read2")):
        yield True
        return

def compileTerm(arg1, arg2, arg3):
  doBreak = False
  for _ in [1]:
    Term = arg1
    State = arg2
    VariableName = Variable()
    for l2 in YP.unify(arg3, Functor1("var", VariableName)):
      if YP.var(Term):
        for l4 in CompilerState.getVariableName(State, Term, VariableName):
          yield True
          return
        if doBreak:
          break
    if doBreak:
      break
  for _ in [1]:
    _State = arg2
    for l2 in YP.unify(arg1, Atom.NIL):
      for l3 in YP.unify(arg3, Functor1("var", Atom.a("Atom.NIL"))):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg1
    State = arg2
    Code = arg3
    ModuleCode = Variable()
    if YP.atom(Term):
      cutIf1 = False
      for _ in [1]:
        for l4 in compileAtomModule(Term, 0, State, ModuleCode):
          for l5 in YP.unify(Code, Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", Term), ListPair(ModuleCode, Atom.NIL)))):
            yield True
            return
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        for l4 in YP.unify(Code, Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", Term), Atom.NIL))):
          yield True
          return
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
  for _ in [1]:
    State = arg2
    First = Variable()
    Rest = Variable()
    CompiledList = Variable()
    x5 = Variable()
    Rest2 = Variable()
    for l2 in YP.unify(arg1, ListPair(First, Rest)):
      for l3 in YP.unify(arg3, Functor2("call", Atom.a("ListPair.make"), ListPair(Functor1("objectArray", CompiledList), Atom.NIL))):
        if YP.nonvar(Rest):
          for l5 in YP.unify(Rest, ListPair(x5, Rest2)):
            if YP.termNotEqual(Rest2, Atom.NIL):
              for l7 in maplist_compileTerm(ListPair(First, Rest), State, CompiledList):
                yield True
                return
              if doBreak:
                break
          if doBreak:
            break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    State = arg2
    First = Variable()
    Rest = Variable()
    Arg1 = Variable()
    Arg2 = Variable()
    for l2 in YP.unify(arg1, ListPair(First, Rest)):
      for l3 in YP.unify(arg3, Functor2("new", Atom.a("ListPair"), ListPair(Arg1, ListPair(Arg2, Atom.NIL)))):
        for l4 in compileTerm(First, State, Arg1):
          for l5 in compileTerm(Rest, State, Arg2):
            yield True
            return
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg1
    State = arg2
    Result = arg3
    Name = Variable()
    TermArgs = Variable()
    x6 = Variable()
    Arity = Variable()
    ModuleCode = Variable()
    NameCode = Variable()
    X1 = Variable()
    Arg1 = Variable()
    X2 = Variable()
    Arg2 = Variable()
    X3 = Variable()
    Arg3 = Variable()
    Args = Variable()
    for l2 in YP.univ(Term, ListPair(Name, TermArgs)):
      cutIf2 = False
      for _ in [1]:
        if YP.termEqual(TermArgs, Atom.NIL):
          for l5 in YP.unify(Result, Functor1("object", Name)):
            yield True
            return
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
          break
        for l4 in YP.functor(Term, x6, Arity):
          cutIf3 = False
          for _ in [1]:
            for l6 in compileAtomModule(Name, Arity, State, ModuleCode):
              for l7 in YP.unify(NameCode, Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", Name), ListPair(ModuleCode, Atom.NIL)))):
                cutIf4 = False
                for _ in [1]:
                  for l9 in YP.unify(TermArgs, ListPair(X1, Atom.NIL)):
                    for l10 in compileTerm(X1, State, Arg1):
                      for l11 in YP.unify(Result, Functor2("new", Atom.a("Functor1"), ListPair(NameCode, ListPair(Arg1, Atom.NIL)))):
                        yield True
                        return
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf4 = True
                    doBreak = True
                    break
                  if doBreak:
                    break
                  cutIf5 = False
                  for _ in [1]:
                    for l10 in YP.unify(TermArgs, ListPair(X1, ListPair(X2, Atom.NIL))):
                      for l11 in compileTerm(X1, State, Arg1):
                        for l12 in compileTerm(X2, State, Arg2):
                          for l13 in YP.unify(Result, Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2]))):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                      if doBreak:
                        break
                      cutIf5 = True
                      doBreak = True
                      break
                    if doBreak:
                      break
                    for l10 in YP.unify(TermArgs, ListPair.make([X1, X2, X3])):
                      for l11 in compileTerm(X1, State, Arg1):
                        for l12 in compileTerm(X2, State, Arg2):
                          for l13 in compileTerm(X3, State, Arg3):
                            for l14 in YP.unify(Result, Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3]))):
                              yield True
                              return
                            if doBreak:
                              break
                          if doBreak:
                            break
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                    for l10 in maplist_compileTerm(TermArgs, State, Args):
                      for l11 in YP.unify(Result, Functor2("new", Atom.a("Functor"), ListPair(NameCode, ListPair(Functor1("objectArray", Args), Atom.NIL)))):
                        yield True
                        return
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if cutIf5:
                    doBreak = False
                  if doBreak:
                    break
                if cutIf4:
                  doBreak = False
                if doBreak:
                  break
              if doBreak:
                break
              cutIf3 = True
              doBreak = True
              break
            if doBreak:
              break
            for l6 in YP.unify(NameCode, Functor1("object", Name)):
              cutIf6 = False
              for _ in [1]:
                for l8 in YP.unify(TermArgs, ListPair(X1, Atom.NIL)):
                  for l9 in compileTerm(X1, State, Arg1):
                    for l10 in YP.unify(Result, Functor2("new", Atom.a("Functor1"), ListPair(NameCode, ListPair(Arg1, Atom.NIL)))):
                      yield True
                      return
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf6 = True
                  doBreak = True
                  break
                if doBreak:
                  break
                cutIf7 = False
                for _ in [1]:
                  for l9 in YP.unify(TermArgs, ListPair(X1, ListPair(X2, Atom.NIL))):
                    for l10 in compileTerm(X1, State, Arg1):
                      for l11 in compileTerm(X2, State, Arg2):
                        for l12 in YP.unify(Result, Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2]))):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf7 = True
                    doBreak = True
                    break
                  if doBreak:
                    break
                  for l9 in YP.unify(TermArgs, ListPair.make([X1, X2, X3])):
                    for l10 in compileTerm(X1, State, Arg1):
                      for l11 in compileTerm(X2, State, Arg2):
                        for l12 in compileTerm(X3, State, Arg3):
                          for l13 in YP.unify(Result, Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3]))):
                            yield True
                            return
                          if doBreak:
                            break
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
                  for l9 in maplist_compileTerm(TermArgs, State, Args):
                    for l10 in YP.unify(Result, Functor2("new", Atom.a("Functor"), ListPair(NameCode, ListPair(Functor1("objectArray", Args), Atom.NIL)))):
                      yield True
                      return
                    if doBreak:
                      break
                  if doBreak:
                    break
                if cutIf7:
                  doBreak = False
                if doBreak:
                  break
              if cutIf6:
                doBreak = False
              if doBreak:
                break
            if doBreak:
              break
          if cutIf3:
            doBreak = False
          if doBreak:
            break
        if doBreak:
          break
      if cutIf2:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def compileAtomModule(Name, arg2, arg3, ModuleCode):
  Arity = arg2
  State = arg3
  if CompilerState.nameArityHasModule(State, Name, Arity, Atom.a("")):
    for l2 in YP.unify(ModuleCode, Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", Atom.a("")), Atom.NIL))):
      yield True
      return
  _Arity = arg2
  _State = arg3
  Module = Variable()
  for l1 in Atom.module(Name, Module):
    if YP.termNotEqual(Module, Atom.NIL):
      for l3 in YP.unify(ModuleCode, Functor2("call", Atom.a("Atom.a"), ListPair(Functor1("object", Module), Atom.NIL))):
        yield True
        return

def maplist_compileTerm(arg1, arg2, arg3):
  _State = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield True
      return
  State = arg2
  First = Variable()
  Rest = Variable()
  FirstResult = Variable()
  RestResults = Variable()
  for l1 in YP.unify(arg1, ListPair(First, Rest)):
    for l2 in YP.unify(arg3, ListPair(FirstResult, RestResults)):
      if YP.nonvar(Rest):
        for l4 in compileTerm(First, State, FirstResult):
          for l5 in maplist_compileTerm(Rest, State, RestResults):
            yield True
            return

def compileExpression(Term, State, Result):
  doBreak = False
  for _ in [1]:
    Name = Variable()
    TermArgs = Variable()
    X1 = Variable()
    FunctionName = Variable()
    Arg1 = Variable()
    x9 = Variable()
    X2 = Variable()
    Arg2 = Variable()
    x12 = Variable()
    Arity = Variable()
    if YP.nonvar(Term):
      for l3 in YP.univ(Term, ListPair(Name, TermArgs)):
        if YP.atom(Name):
          cutIf1 = False
          for _ in [1]:
            for l6 in YP.unify(TermArgs, ListPair(X1, Atom.NIL)):
              for l7 in unaryFunction(Name, FunctionName):
                for l8 in compileExpression(X1, State, Arg1):
                  for l9 in YP.unify(Result, Functor2("call", FunctionName, ListPair(Arg1, Atom.NIL))):
                    yield True
                    return
                  if doBreak:
                    break
                if doBreak:
                  break
                cutIf1 = True
                doBreak = True
                break
              if doBreak:
                break
            if doBreak:
              break
            cutIf2 = False
            for _ in [1]:
              for l7 in YP.unify(Term, ListPair(x9, Atom.NIL)):
                for l8 in compileTerm(Term, State, Result):
                  yield True
                  return
                if doBreak:
                  break
                cutIf2 = True
                doBreak = True
                break
              if doBreak:
                break
              cutIf3 = False
              for _ in [1]:
                for l8 in YP.unify(TermArgs, ListPair(X1, ListPair(X2, Atom.NIL))):
                  for l9 in binaryFunction(Name, FunctionName):
                    for l10 in compileExpression(X1, State, Arg1):
                      for l11 in compileExpression(X2, State, Arg2):
                        for l12 in YP.unify(Result, Functor2("call", FunctionName, ListPair(Arg1, ListPair(Arg2, Atom.NIL)))):
                          yield True
                          return
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf3 = True
                    doBreak = True
                    break
                  if doBreak:
                    break
                if doBreak:
                  break
                for l8 in YP.functor(Term, x12, Arity):
                  YP.throwException(Functor2("error", Functor2("type_error", Atom.a("evaluable"), Functor2("/", Name, Arity)), Atom.a("Not an expression function")))
                  yield False
                if doBreak:
                  break
              if cutIf3:
                doBreak = False
              if doBreak:
                break
            if cutIf2:
              doBreak = False
            if doBreak:
              break
          if cutIf1:
            doBreak = False
          if doBreak:
            break
      if doBreak:
        break
  for _ in [1]:
    for l2 in compileTerm(Term, State, Result):
      yield True
      return
    if doBreak:
      break

def unaryFunction(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.a("-")):
    for l2 in YP.unify(arg2, Atom.a("YP.negate")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("abs")):
    for l2 in YP.unify(arg2, Atom.a("YP.abs")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("sign")):
    for l2 in YP.unify(arg2, Atom.a("YP.sign")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, Atom.a("YP.toFloat")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("floor")):
    for l2 in YP.unify(arg2, Atom.a("YP.floor")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("truncate")):
    for l2 in YP.unify(arg2, Atom.a("YP.truncate")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("round")):
    for l2 in YP.unify(arg2, Atom.a("YP.round")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("ceiling")):
    for l2 in YP.unify(arg2, Atom.a("YP.ceiling")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("sin")):
    for l2 in YP.unify(arg2, Atom.a("YP.sin")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("cos")):
    for l2 in YP.unify(arg2, Atom.a("YP.cos")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("atan")):
    for l2 in YP.unify(arg2, Atom.a("YP.atan")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("exp")):
    for l2 in YP.unify(arg2, Atom.a("YP.exp")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("log")):
    for l2 in YP.unify(arg2, Atom.a("YP.log")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("sqrt")):
    for l2 in YP.unify(arg2, Atom.a("YP.sqrt")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("\\")):
    for l2 in YP.unify(arg2, Atom.a("YP.bitwiseComplement")):
      yield True
      return

def binaryFunction(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.a("+")):
    for l2 in YP.unify(arg2, Atom.a("YP.add")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("-")):
    for l2 in YP.unify(arg2, Atom.a("YP.subtract")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("*")):
    for l2 in YP.unify(arg2, Atom.a("YP.multiply")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("/")):
    for l2 in YP.unify(arg2, Atom.a("YP.divide")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("//")):
    for l2 in YP.unify(arg2, Atom.a("YP.intDivide")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("mod")):
    for l2 in YP.unify(arg2, Atom.a("YP.mod")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("**")):
    for l2 in YP.unify(arg2, Atom.a("YP.pow")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a(">>")):
    for l2 in YP.unify(arg2, Atom.a("YP.bitwiseShiftRight")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("<<")):
    for l2 in YP.unify(arg2, Atom.a("YP.bitwiseShiftLeft")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("/\\")):
    for l2 in YP.unify(arg2, Atom.a("YP.bitwiseAnd")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("\\/")):
    for l2 in YP.unify(arg2, Atom.a("YP.bitwiseOr")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("min")):
    for l2 in YP.unify(arg2, Atom.a("YP.min")):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.a("max")):
    for l2 in YP.unify(arg2, Atom.a("YP.max")):
      yield True
      return

def convertFunctionCSharp(arg1):
  for l1 in YP.unify(arg1, Atom.a("getDeclaringClass")):
    YP.write(Atom.a("public class YPInnerClass {}"))
    YP.nl()
    YP.write(Atom.a("public static Type getDeclaringClass() { return typeof(YPInnerClass).DeclaringType; }"))
    YP.nl()
    YP.nl()
    return
  ReturnType = Variable()
  Name = Variable()
  ArgList = Variable()
  Body = Variable()
  Level = Variable()
  for l1 in YP.unify(arg1, Functor("function", [ReturnType, Name, ArgList, Body])):
    YP.write(Atom.a("public static "))
    YP.write(ReturnType)
    YP.write(Atom.a(" "))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListCSharp(ArgList)
    YP.write(Atom.a(") {"))
    YP.nl()
    for l2 in YP.unify(Level, 1):
      convertStatementListCSharp(Body, Level)
      YP.write(Atom.a("}"))
      YP.nl()
      YP.nl()
      return

def convertStatementListCSharp(arg1, x1, x2):
  for l1 in YP.unify(arg1, Atom.NIL):
    yield True
    return

def convertStatementListCSharp(arg1, Level):
  doBreak = False
  for _ in [1]:
    Name = Variable()
    Body = Variable()
    RestStatements = Variable()
    NewStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("breakableBlock", Name, Body), RestStatements)):
      for l3 in append(Body, ListPair(Functor1("label", Name), RestStatements), NewStatements):
        convertStatementListCSharp(NewStatements, Level)
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Type = Variable()
    Name = Variable()
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("declare", Type, Name, Expression), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Type)
      YP.write(Atom.a(" "))
      YP.write(Name)
      YP.write(Atom.a(" = "))
      convertExpressionCSharp(Expression)
      YP.write(Atom.a(";"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Name = Variable()
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("assign", Name, Expression), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Name)
      YP.write(Atom.a(" = "))
      convertExpressionCSharp(Expression)
      YP.write(Atom.a(";"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldtrue"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("yield return true;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldfalse"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("yield return false;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldbreak"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("yield break;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("return"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("return;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("returntrue"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("return true;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("returnfalse"), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("return false;"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Name = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("label", Name), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Name)
      YP.write(Atom.a(":"))
      YP.nl()
      cutIf1 = False
      for _ in [1]:
        if YP.termEqual(RestStatements, Atom.NIL):
          convertIndentationCSharp(Level)
          YP.write(Atom.a("{}"))
          YP.nl()
          convertStatementListCSharp(RestStatements, Level)
          return
          cutIf1 = True
          doBreak = True
          break
        convertStatementListCSharp(RestStatements, Level)
        return
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Name = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("breakBlock", Name), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("goto "))
      YP.write(Name)
      YP.write(Atom.a(";"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Name = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("call", Name, ArgList), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Name)
      YP.write(Atom.a("("))
      convertArgListCSharp(ArgList)
      YP.write(Atom.a(");"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Name = Variable()
    _FunctorArgs = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements)):
      convertStatementListCSharp(ListPair(Functor2("call", Name, ArgList), RestStatements), Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Obj = Variable()
    Name = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("callMember", Functor1("var", Obj), Name, ArgList), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Obj)
      YP.write(Atom.a("."))
      YP.write(Name)
      YP.write(Atom.a("("))
      convertArgListCSharp(ArgList)
      YP.write(Atom.a(");"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break
  for _ in [1]:
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("blockScope", Body), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("{"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        convertStatementListCSharp(Body, NextLevel)
        convertIndentationCSharp(Level)
        YP.write(Atom.a("}"))
        YP.nl()
        convertStatementListCSharp(RestStatements, Level)
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Expression = Variable()
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("if", Expression, Body), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("if ("))
      convertExpressionCSharp(Expression)
      YP.write(Atom.a(") {"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        convertStatementListCSharp(Body, NextLevel)
        convertIndentationCSharp(Level)
        YP.write(Atom.a("}"))
        YP.nl()
        convertStatementListCSharp(RestStatements, Level)
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Expression = Variable()
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("foreach", Expression, Body), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("foreach (bool l"))
      YP.write(Level)
      YP.write(Atom.a(" in "))
      convertExpressionCSharp(Expression)
      YP.write(Atom.a(") {"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        convertStatementListCSharp(Body, NextLevel)
        convertIndentationCSharp(Level)
        YP.write(Atom.a("}"))
        YP.nl()
        convertStatementListCSharp(RestStatements, Level)
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("throw", Expression), RestStatements)):
      convertIndentationCSharp(Level)
      YP.write(Atom.a("throw "))
      convertExpressionCSharp(Expression)
      YP.write(Atom.a(";"))
      YP.nl()
      convertStatementListCSharp(RestStatements, Level)
      return
    if doBreak:
      break

def convertIndentationCSharp(Level):
  N = Variable()
  for l1 in YP.unify(N, YP.multiply(Level, 2)):
    repeatWrite(Atom.a(" "), N)
    return

def convertArgListCSharp(arg1):
  doBreak = False
  for _ in [1]:
    for l2 in YP.unify(arg1, Atom.NIL):
      return
    if doBreak:
      break
  for _ in [1]:
    Head = Variable()
    Tail = Variable()
    for l2 in YP.unify(arg1, ListPair(Head, Tail)):
      convertExpressionCSharp(Head)
      cutIf1 = False
      for _ in [1]:
        if YP.termNotEqual(Tail, Atom.NIL):
          YP.write(Atom.a(", "))
          convertArgListCSharp(Tail)
          return
          cutIf1 = True
          doBreak = True
          break
        convertArgListCSharp(Tail)
        return
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def convertExpressionCSharp(arg1):
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("arg", X)):
    YP.write(Atom.a("object "))
    YP.write(X)
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("call", Name, ArgList)):
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListCSharp(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  _FunctorArgs = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("functorCall", Name, _FunctorArgs, ArgList)):
    convertExpressionCSharp(Functor2("call", Name, ArgList))
    return
  Obj = Variable()
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("callMember", Functor1("var", Obj), Name, ArgList)):
    YP.write(Obj)
    YP.write(Atom.a("."))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListCSharp(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("new", Name, ArgList)):
    YP.write(Atom.a("new "))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListCSharp(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  for l1 in YP.unify(arg1, Functor1("var", Name)):
    YP.write(Name)
    return
  for l1 in YP.unify(arg1, Atom.a("null")):
    YP.write(Atom.a("null"))
    return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("not", X)):
    YP.write(Atom.a("!("))
    convertExpressionCSharp(X)
    YP.write(Atom.a(")"))
    return
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Functor2("and", X, Y)):
    YP.write(Atom.a("("))
    convertExpressionCSharp(X)
    YP.write(Atom.a(") && ("))
    convertExpressionCSharp(Y)
    YP.write(Atom.a(")"))
    return
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor1("objectArray", ArgList)):
    YP.write(Atom.a("new object[] {"))
    convertArgListCSharp(ArgList)
    YP.write(Atom.a("}"))
    return
  X = Variable()
  Codes = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    if YP.atom(X):
      YP.write(Atom.a("\""))
      for l3 in YP.atom_codes(X, Codes):
        convertStringCodesCSharp(Codes)
        YP.write(Atom.a("\""))
        return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    YP.write(X)
    return

def convertStringCodesCSharp(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    return
  Code = Variable()
  RestCodes = Variable()
  for l1 in YP.unify(arg1, ListPair(Code, RestCodes)):
    for l2 in putCStringCode(Code):
      convertStringCodesCSharp(RestCodes)
      return

def convertFunctionJavascript(arg1):
  for l1 in YP.unify(arg1, Atom.a("getDeclaringClass")):
    YP.write(Atom.a("function getDeclaringClass() { return null; }"))
    YP.nl()
    YP.nl()
    return
  x1 = Variable()
  Name = Variable()
  ArgList = Variable()
  Body = Variable()
  for l1 in YP.unify(arg1, Functor("function", [x1, Name, ArgList, Body])):
    YP.write(Atom.a("function "))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(") {"))
    YP.nl()
    convertStatementListJavascript(Body, 1)
    YP.write(Atom.a("}"))
    YP.nl()
    YP.nl()
    return

def convertStatementListJavascript(arg1, arg2):
  x1 = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    return
  Level = arg2
  Name = Variable()
  Body = Variable()
  RestStatements = Variable()
  NextLevel = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("breakableBlock", Name, Body), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Name)
    YP.write(Atom.a(":"))
    YP.nl()
    convertIndentationJavascript(Level)
    YP.write(Atom.a("{"))
    YP.nl()
    for l2 in YP.unify(NextLevel, YP.add(Level, 1)):
      convertStatementListJavascript(Body, NextLevel)
      convertIndentationJavascript(Level)
      YP.write(Atom.a("}"))
      YP.nl()
      convertStatementListJavascript(RestStatements, Level)
      return
  Level = arg2
  _Type = Variable()
  Name = Variable()
  Expression = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor3("declare", _Type, Name, Expression), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("var "))
    YP.write(Name)
    YP.write(Atom.a(" = "))
    convertExpressionJavascript(Expression)
    YP.write(Atom.a(";"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  Name = Variable()
  Expression = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("assign", Name, Expression), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Name)
    YP.write(Atom.a(" = "))
    convertExpressionJavascript(Expression)
    YP.write(Atom.a(";"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("yieldtrue"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("yield true;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("yieldfalse"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("yield false;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("yieldbreak"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("return;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("return"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("return;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("returntrue"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("return true;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a("returnfalse"), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("return false;"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  Name = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor1("breakBlock", Name), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("break "))
    YP.write(Name)
    YP.write(Atom.a(";"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  Name = Variable()
  ArgList = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("call", Name, ArgList), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(");"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  Name = Variable()
  _FunctorArgs = Variable()
  ArgList = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements)):
    convertStatementListJavascript(ListPair(Functor2("call", Name, ArgList), RestStatements), Level)
    return
  Level = arg2
  Obj = Variable()
  Name = Variable()
  ArgList = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor3("callMember", Functor1("var", Obj), Name, ArgList), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Obj)
    YP.write(Atom.a("."))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(");"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return
  Level = arg2
  Body = Variable()
  RestStatements = Variable()
  NextLevel = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor1("blockScope", Body), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("{"))
    YP.nl()
    for l2 in YP.unify(NextLevel, YP.add(Level, 1)):
      convertStatementListJavascript(Body, NextLevel)
      convertIndentationJavascript(Level)
      YP.write(Atom.a("}"))
      YP.nl()
      convertStatementListJavascript(RestStatements, Level)
      return
  Level = arg2
  Expression = Variable()
  Body = Variable()
  RestStatements = Variable()
  NextLevel = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("if", Expression, Body), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("if ("))
    convertExpressionJavascript(Expression)
    YP.write(Atom.a(") {"))
    YP.nl()
    for l2 in YP.unify(NextLevel, YP.add(Level, 1)):
      convertStatementListJavascript(Body, NextLevel)
      convertIndentationJavascript(Level)
      YP.write(Atom.a("}"))
      YP.nl()
      convertStatementListJavascript(RestStatements, Level)
      return
  Level = arg2
  Expression = Variable()
  Body = Variable()
  RestStatements = Variable()
  NextLevel = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("foreach", Expression, Body), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("for each (var l"))
    YP.write(Level)
    YP.write(Atom.a(" in "))
    convertExpressionJavascript(Expression)
    YP.write(Atom.a(") {"))
    YP.nl()
    for l2 in YP.unify(NextLevel, YP.add(Level, 1)):
      convertStatementListJavascript(Body, NextLevel)
      convertIndentationJavascript(Level)
      YP.write(Atom.a("}"))
      YP.nl()
      convertStatementListJavascript(RestStatements, Level)
      return
  Level = arg2
  Expression = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor1("throw", Expression), RestStatements)):
    convertIndentationJavascript(Level)
    YP.write(Atom.a("throw "))
    convertExpressionJavascript(Expression)
    YP.write(Atom.a(";"))
    YP.nl()
    convertStatementListJavascript(RestStatements, Level)
    return

def convertIndentationJavascript(Level):
  N = Variable()
  for l1 in YP.unify(N, YP.multiply(Level, 2)):
    repeatWrite(Atom.a(" "), N)
    return

def convertArgListJavascript(arg1):
  doBreak = False
  for _ in [1]:
    for l2 in YP.unify(arg1, Atom.NIL):
      return
    if doBreak:
      break
  for _ in [1]:
    Head = Variable()
    Tail = Variable()
    for l2 in YP.unify(arg1, ListPair(Head, Tail)):
      convertExpressionJavascript(Head)
      cutIf1 = False
      for _ in [1]:
        if YP.termNotEqual(Tail, Atom.NIL):
          YP.write(Atom.a(", "))
          convertArgListJavascript(Tail)
          return
          cutIf1 = True
          doBreak = True
          break
        convertArgListJavascript(Tail)
        return
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def convertExpressionJavascript(arg1):
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("arg", X)):
    YP.write(X)
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("call", Name, ArgList)):
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  _FunctorArgs = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("functorCall", Name, _FunctorArgs, ArgList)):
    convertExpressionJavascript(Functor2("call", Name, ArgList))
    return
  Obj = Variable()
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("callMember", Functor1("var", Obj), Name, ArgList)):
    YP.write(Obj)
    YP.write(Atom.a("."))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("new", Name, ArgList)):
    YP.write(Atom.a("new "))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  for l1 in YP.unify(arg1, Functor1("var", Name)):
    YP.write(Name)
    return
  for l1 in YP.unify(arg1, Atom.a("null")):
    YP.write(Atom.a("null"))
    return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("not", X)):
    YP.write(Atom.a("!("))
    convertExpressionJavascript(X)
    YP.write(Atom.a(")"))
    return
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Functor2("and", X, Y)):
    YP.write(Atom.a("("))
    convertExpressionJavascript(X)
    YP.write(Atom.a(") && ("))
    convertExpressionJavascript(Y)
    YP.write(Atom.a(")"))
    return
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor1("objectArray", ArgList)):
    YP.write(Atom.a("["))
    convertArgListJavascript(ArgList)
    YP.write(Atom.a("]"))
    return
  X = Variable()
  Codes = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    if YP.atom(X):
      YP.write(Atom.a("\""))
      for l3 in YP.atom_codes(X, Codes):
        convertStringCodesJavascript(Codes)
        YP.write(Atom.a("\""))
        return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    YP.write(X)
    return

def convertStringCodesJavascript(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    return
  Code = Variable()
  RestCodes = Variable()
  for l1 in YP.unify(arg1, ListPair(Code, RestCodes)):
    for l2 in putCStringCode(Code):
      convertStringCodesJavascript(RestCodes)
      return

def convertFunctionPython(arg1):
  doBreak = False
  for _ in [1]:
    for l2 in YP.unify(arg1, Atom.a("getDeclaringClass")):
      YP.write(Atom.a("def getDeclaringClass():"))
      YP.nl()
      YP.write(Atom.a("  return globals()"))
      YP.nl()
      YP.nl()
      return
    if doBreak:
      break
  for _ in [1]:
    x1 = Variable()
    Name = Variable()
    ArgList = Variable()
    Body = Variable()
    Level = Variable()
    HasBreakableBlock = Variable()
    for l2 in YP.unify(arg1, Functor("function", [x1, Name, ArgList, Body])):
      YP.write(Atom.a("def "))
      YP.write(Name)
      YP.write(Atom.a("("))
      convertArgListPython(ArgList)
      YP.write(Atom.a("):"))
      YP.nl()
      for l3 in YP.unify(Level, 1):
        cutIf1 = False
        for _ in [1]:
          if hasBreakableBlockPython(Body):
            for l6 in YP.unify(HasBreakableBlock, 1):
              cutIf2 = False
              for _ in [1]:
                if YP.termEqual(HasBreakableBlock, 1):
                  convertIndentationPython(Level)
                  YP.write(Atom.a("doBreak = False"))
                  YP.nl()
                  for l9 in convertStatementListPython(Body, Level, HasBreakableBlock):
                    YP.nl()
                    return
                  if doBreak:
                    break
                  cutIf2 = True
                  doBreak = True
                  break
                for l8 in convertStatementListPython(Body, Level, HasBreakableBlock):
                  YP.nl()
                  return
                if doBreak:
                  break
              if cutIf2:
                doBreak = False
              if doBreak:
                break
            if doBreak:
              break
            cutIf1 = True
            doBreak = True
            break
          for l5 in YP.unify(HasBreakableBlock, 0):
            cutIf3 = False
            for _ in [1]:
              if YP.termEqual(HasBreakableBlock, 1):
                convertIndentationPython(Level)
                YP.write(Atom.a("doBreak = False"))
                YP.nl()
                for l8 in convertStatementListPython(Body, Level, HasBreakableBlock):
                  YP.nl()
                  return
                if doBreak:
                  break
                cutIf3 = True
                doBreak = True
                break
              for l7 in convertStatementListPython(Body, Level, HasBreakableBlock):
                YP.nl()
                return
              if doBreak:
                break
            if cutIf3:
              doBreak = False
            if doBreak:
              break
          if doBreak:
            break
        if cutIf1:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def hasBreakableBlockPython(arg1):
  _Name = Variable()
  _Body = Variable()
  _RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("breakableBlock", _Name, _Body), _RestStatements)):
    return True
  Body = Variable()
  _RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor1("blockScope", Body), _RestStatements)):
    if hasBreakableBlockPython(Body):
      return True
  _Expression = Variable()
  Body = Variable()
  _RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("if", _Expression, Body), _RestStatements)):
    if hasBreakableBlockPython(Body):
      return True
  _Expression = Variable()
  Body = Variable()
  _RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("foreach", _Expression, Body), _RestStatements)):
    if hasBreakableBlockPython(Body):
      return True
  x1 = Variable()
  RestStatements = Variable()
  for l1 in YP.unify(arg1, ListPair(x1, RestStatements)):
    if hasBreakableBlockPython(RestStatements):
      return True
  return False

def convertStatementListPython(arg1, arg2, arg3):
  doBreak = False
  for _ in [1]:
    x1 = arg2
    x2 = arg3
    for l2 in YP.unify(arg1, Atom.NIL):
      yield True
      return
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Name = Variable()
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("breakableBlock", Name, Body), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Name)
      YP.write(Atom.a(" = False"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        cutIf1 = False
        for _ in [1]:
          if YP.termEqual(Body, Atom.NIL):
            convertIndentationPython(Level)
            YP.write(Atom.a("if "))
            YP.write(Name)
            YP.write(Atom.a(":"))
            YP.nl()
            convertIndentationPython(NextLevel)
            YP.write(Atom.a("doBreak = False"))
            YP.nl()
            convertIndentationPython(Level)
            YP.write(Atom.a("if doBreak:"))
            YP.nl()
            convertIndentationPython(NextLevel)
            YP.write(Atom.a("break"))
            YP.nl()
            for l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
              yield True
              return
            if doBreak:
              break
            cutIf1 = True
            doBreak = True
            break
          convertIndentationPython(Level)
          YP.write(Atom.a("for _ in [1]:"))
          YP.nl()
          for l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock):
            convertIndentationPython(Level)
            YP.write(Atom.a("if "))
            YP.write(Name)
            YP.write(Atom.a(":"))
            YP.nl()
            convertIndentationPython(NextLevel)
            YP.write(Atom.a("doBreak = False"))
            YP.nl()
            convertIndentationPython(Level)
            YP.write(Atom.a("if doBreak:"))
            YP.nl()
            convertIndentationPython(NextLevel)
            YP.write(Atom.a("break"))
            YP.nl()
            for l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
              yield True
              return
            if doBreak:
              break
          if doBreak:
            break
        if cutIf1:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    _Type = Variable()
    Name = Variable()
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("declare", _Type, Name, Expression), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Name)
      YP.write(Atom.a(" = "))
      convertExpressionPython(Expression)
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Name = Variable()
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("assign", Name, Expression), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Name)
      YP.write(Atom.a(" = "))
      convertExpressionPython(Expression)
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldtrue"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("yield True"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldfalse"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("yield False"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("yieldbreak"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("return"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("return"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("return"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("returntrue"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("return True"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Atom.a("returnfalse"), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("return False"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Name = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("breakBlock", Name), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Name)
      YP.write(Atom.a(" = True"))
      YP.nl()
      convertIndentationPython(Level)
      YP.write(Atom.a("doBreak = True"))
      YP.nl()
      convertIndentationPython(Level)
      YP.write(Atom.a("break"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Name = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("call", Name, ArgList), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Name)
      YP.write(Atom.a("("))
      convertArgListPython(ArgList)
      YP.write(Atom.a(")"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Name = Variable()
    _FunctorArgs = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements)):
      for l3 in convertStatementListPython(ListPair(Functor2("call", Name, ArgList), RestStatements), Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Obj = Variable()
    Name = Variable()
    ArgList = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor3("callMember", Functor1("var", Obj), Name, ArgList), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Obj)
      YP.write(Atom.a("."))
      YP.write(Name)
      YP.write(Atom.a("("))
      convertArgListPython(ArgList)
      YP.write(Atom.a(")"))
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("blockScope", Body), RestStatements)):
      cutIf2 = False
      for _ in [1]:
        if YP.termEqual(HasBreakableBlock, 1):
          if YP.termNotEqual(Body, Atom.NIL):
            convertIndentationPython(Level)
            YP.write(Atom.a("for _ in [1]:"))
            YP.nl()
            for l6 in YP.unify(NextLevel, YP.add(Level, 1)):
              for l7 in convertStatementListPython(Body, NextLevel, HasBreakableBlock):
                cutIf3 = False
                for _ in [1]:
                  if YP.termEqual(HasBreakableBlock, 1):
                    cutIf4 = False
                    for _ in [1]:
                      if YP.greaterThan(Level, 1):
                        convertIndentationPython(Level)
                        YP.write(Atom.a("if doBreak:"))
                        YP.nl()
                        convertIndentationPython(NextLevel)
                        YP.write(Atom.a("break"))
                        YP.nl()
                        for l12 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                          yield True
                          return
                        if doBreak:
                          break
                        cutIf4 = True
                        doBreak = True
                        break
                      for l11 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                        yield True
                        return
                      if doBreak:
                        break
                    if cutIf4:
                      doBreak = False
                    if doBreak:
                      break
                    cutIf3 = True
                    doBreak = True
                    break
                  for l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                    yield True
                    return
                  if doBreak:
                    break
                if cutIf3:
                  doBreak = False
                if doBreak:
                  break
              if doBreak:
                break
            if doBreak:
              break
            cutIf2 = True
            doBreak = True
            break
        for l4 in YP.unify(NextLevel, Level):
          for l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock):
            cutIf5 = False
            for _ in [1]:
              if YP.termEqual(HasBreakableBlock, 1):
                cutIf6 = False
                for _ in [1]:
                  if YP.greaterThan(Level, 1):
                    convertIndentationPython(Level)
                    YP.write(Atom.a("if doBreak:"))
                    YP.nl()
                    convertIndentationPython(NextLevel)
                    YP.write(Atom.a("break"))
                    YP.nl()
                    for l10 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                      yield True
                      return
                    if doBreak:
                      break
                    cutIf6 = True
                    doBreak = True
                    break
                  for l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                    yield True
                    return
                  if doBreak:
                    break
                if cutIf6:
                  doBreak = False
                if doBreak:
                  break
                cutIf5 = True
                doBreak = True
                break
              for l7 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                yield True
                return
              if doBreak:
                break
            if cutIf5:
              doBreak = False
            if doBreak:
              break
          if doBreak:
            break
        if doBreak:
          break
      if cutIf2:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Expression = Variable()
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("if", Expression, Body), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("if "))
      convertExpressionPython(Expression)
      YP.write(Atom.a(":"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        cutIf7 = False
        for _ in [1]:
          if YP.termEqual(Body, Atom.NIL):
            convertIndentationPython(NextLevel)
            YP.write(Atom.a("pass"))
            YP.nl()
            for l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
              yield True
              return
            if doBreak:
              break
            cutIf7 = True
            doBreak = True
            break
          for l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock):
            for l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
              yield True
              return
            if doBreak:
              break
          if doBreak:
            break
        if cutIf7:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Expression = Variable()
    Body = Variable()
    RestStatements = Variable()
    NextLevel = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("foreach", Expression, Body), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("for l"))
      YP.write(Level)
      YP.write(Atom.a(" in "))
      convertExpressionPython(Expression)
      YP.write(Atom.a(":"))
      YP.nl()
      for l3 in YP.unify(NextLevel, YP.add(Level, 1)):
        for l4 in YP.unify(NextLevel, YP.add(Level, 1)):
          cutIf8 = False
          for _ in [1]:
            if YP.termEqual(Body, Atom.NIL):
              convertIndentationPython(NextLevel)
              YP.write(Atom.a("pass"))
              YP.nl()
              cutIf9 = False
              for _ in [1]:
                if YP.termEqual(HasBreakableBlock, 1):
                  convertIndentationPython(Level)
                  YP.write(Atom.a("if doBreak:"))
                  YP.nl()
                  convertIndentationPython(NextLevel)
                  YP.write(Atom.a("break"))
                  YP.nl()
                  for l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                    yield True
                    return
                  if doBreak:
                    break
                  cutIf9 = True
                  doBreak = True
                  break
                for l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                  yield True
                  return
                if doBreak:
                  break
              if cutIf9:
                doBreak = False
              if doBreak:
                break
              cutIf8 = True
              doBreak = True
              break
            for l6 in convertStatementListPython(Body, NextLevel, HasBreakableBlock):
              cutIf10 = False
              for _ in [1]:
                if YP.termEqual(HasBreakableBlock, 1):
                  convertIndentationPython(Level)
                  YP.write(Atom.a("if doBreak:"))
                  YP.nl()
                  convertIndentationPython(NextLevel)
                  YP.write(Atom.a("break"))
                  YP.nl()
                  for l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                    yield True
                    return
                  if doBreak:
                    break
                  cutIf10 = True
                  doBreak = True
                  break
                for l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
                  yield True
                  return
                if doBreak:
                  break
              if cutIf10:
                doBreak = False
              if doBreak:
                break
            if doBreak:
              break
          if cutIf8:
            doBreak = False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Level = arg2
    HasBreakableBlock = arg3
    Expression = Variable()
    RestStatements = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor1("throw", Expression), RestStatements)):
      convertIndentationPython(Level)
      YP.write(Atom.a("raise "))
      convertExpressionPython(Expression)
      YP.nl()
      for l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock):
        yield True
        return
      if doBreak:
        break
    if doBreak:
      break

def convertIndentationPython(Level):
  N = Variable()
  for l1 in YP.unify(N, YP.multiply(Level, 2)):
    repeatWrite(Atom.a(" "), N)
    return

def convertArgListPython(arg1):
  doBreak = False
  for _ in [1]:
    for l2 in YP.unify(arg1, Atom.NIL):
      return
    if doBreak:
      break
  for _ in [1]:
    Head = Variable()
    Tail = Variable()
    for l2 in YP.unify(arg1, ListPair(Head, Tail)):
      convertExpressionPython(Head)
      cutIf1 = False
      for _ in [1]:
        if YP.termNotEqual(Tail, Atom.NIL):
          YP.write(Atom.a(", "))
          convertArgListPython(Tail)
          return
          cutIf1 = True
          doBreak = True
          break
        convertArgListPython(Tail)
        return
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def convertExpressionPython(arg1):
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("arg", X)):
    YP.write(X)
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("call", Name, ArgList)):
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListPython(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  _FunctorArgs = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("functorCall", Name, _FunctorArgs, ArgList)):
    convertExpressionPython(Functor2("call", Name, ArgList))
    return
  Obj = Variable()
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor3("callMember", Functor1("var", Obj), Name, ArgList)):
    YP.write(Obj)
    YP.write(Atom.a("."))
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListPython(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor2("new", Name, ArgList)):
    YP.write(Name)
    YP.write(Atom.a("("))
    convertArgListPython(ArgList)
    YP.write(Atom.a(")"))
    return
  Name = Variable()
  for l1 in YP.unify(arg1, Functor1("var", Name)):
    YP.write(Name)
    return
  for l1 in YP.unify(arg1, Atom.a("null")):
    YP.write(Atom.a("None"))
    return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("not", X)):
    YP.write(Atom.a("not ("))
    convertExpressionPython(X)
    YP.write(Atom.a(")"))
    return
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Functor2("and", X, Y)):
    YP.write(Atom.a("("))
    convertExpressionPython(X)
    YP.write(Atom.a(") and ("))
    convertExpressionPython(Y)
    YP.write(Atom.a(")"))
    return
  ArgList = Variable()
  for l1 in YP.unify(arg1, Functor1("objectArray", ArgList)):
    YP.write(Atom.a("["))
    convertArgListPython(ArgList)
    YP.write(Atom.a("]"))
    return
  X = Variable()
  Codes = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    if YP.atom(X):
      YP.write(Atom.a("\""))
      for l3 in YP.atom_codes(X, Codes):
        convertStringCodesPython(Codes)
        YP.write(Atom.a("\""))
        return
  X = Variable()
  for l1 in YP.unify(arg1, Functor1("object", X)):
    YP.write(X)
    return

def convertStringCodesPython(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    return
  Code = Variable()
  RestCodes = Variable()
  for l1 in YP.unify(arg1, ListPair(Code, RestCodes)):
    for l2 in putCStringCode(Code):
      convertStringCodesPython(RestCodes)
      return

def putCStringCode(Code):
  doBreak = False
  for _ in [1]:
    HexDigit = Variable()
    HexChar = Variable()
    if YP.lessThanOrEqual(Code, 31):
      cutIf1 = False
      for _ in [1]:
        if YP.lessThanOrEqual(Code, 15):
          YP.write(Atom.a("\\x0"))
          for l5 in YP.unify(HexDigit, Code):
            cutIf2 = False
            for _ in [1]:
              if YP.lessThanOrEqual(HexDigit, 9):
                for l8 in YP.unify(HexChar, YP.add(HexDigit, 48)):
                  YP.put_code(HexChar)
                  yield True
                  return
                if doBreak:
                  break
                cutIf2 = True
                doBreak = True
                break
              for l7 in YP.unify(HexChar, YP.add(HexDigit, 55)):
                YP.put_code(HexChar)
                yield True
                return
              if doBreak:
                break
            if cutIf2:
              doBreak = False
            if doBreak:
              break
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        YP.write(Atom.a("\\x1"))
        for l4 in YP.unify(HexDigit, YP.subtract(Code, 16)):
          cutIf3 = False
          for _ in [1]:
            if YP.lessThanOrEqual(HexDigit, 9):
              for l7 in YP.unify(HexChar, YP.add(HexDigit, 48)):
                YP.put_code(HexChar)
                yield True
                return
              if doBreak:
                break
              cutIf3 = True
              doBreak = True
              break
            for l6 in YP.unify(HexChar, YP.add(HexDigit, 55)):
              YP.put_code(HexChar)
              yield True
              return
            if doBreak:
              break
          if cutIf3:
            doBreak = False
          if doBreak:
            break
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
  for _ in [1]:
    if YP.termEqual(Code, 34):
      YP.put_code(92)
      YP.put_code(34)
      yield True
      return
  for _ in [1]:
    if YP.termEqual(Code, 92):
      YP.put_code(92)
      YP.put_code(92)
      yield True
      return
  for _ in [1]:
    YP.put_code(Code)
    yield True
    return

def member(X, arg2):
  x2 = Variable()
  for l1 in YP.unify(arg2, ListPair(X, x2)):
    yield False
  x2 = Variable()
  Rest = Variable()
  for l1 in YP.unify(arg2, ListPair(x2, Rest)):
    for l2 in member(X, Rest):
      yield False

def append(arg1, arg2, arg3):
  List = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, List):
      for l3 in YP.unify(arg3, List):
        yield False
  List2 = arg2
  X = Variable()
  List1 = Variable()
  List12 = Variable()
  for l1 in YP.unify(arg1, ListPair(X, List1)):
    for l2 in YP.unify(arg3, ListPair(X, List12)):
      for l3 in append(List1, List2, List12):
        yield False

