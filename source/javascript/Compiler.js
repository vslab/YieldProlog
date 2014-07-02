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

function CompilerState() {
	this._pred = new IndexedAnswers(4);
	this._moduleForNameArity = new Object();
}

// Make these static functions that explicitly take the State so Prolog can call it.

// Make a new CompilerState and bind it to State.
CompilerState.make = function(State) {
    return YP.unify(State, new CompilerState());
}

CompilerState.assertPred = function(State, Pred, Determinism) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Pred);
    var functorArgs = YP.getFunctorArgs(Pred);
    // Debug: Should check if it's already asserted and is the same.
    State._pred.addAnswer([functorName, functorArgs.length, Pred, YP.getValue(Determinism)]);
}

CompilerState.assertModuleForNameArity = function(State, Name, Arity, Module) {
    State = YP.getValue(State);
    Name = YP.getValue(Name);
    Arity = YP.getValue(Arity);
    Module = YP.getValue(Module);
    // If the Module Atom comes from the parser, it always has null _declaringClass.
    if (Module instanceof Atom && Module._module == null && Name instanceof Atom && typeof(Arity) == "number")
        // Replace a previous entry if it exists.
        State._moduleForNameArity[Name + "/" + Arity] = Module;
}

CompilerState.startFunction = function(State, Head) {
    State = YP.getValue(State);
    State._gensymCounter = 0;
    State._useFinalCutCode = false;
    State._finalCutCode = new Variable();
    State._codeUsesYield = false;
    if (CompilerState.isDetNoneOut(State, Head))
        State._determinism = Atom.a("detNoneOut");
    else if (CompilerState.isSemidetNoneOut(State, Head))
        State._determinism = Atom.a("semidetNoneOut");
    else
        State._determinism = Atom.a("nondet");
}

CompilerState.setCodeUsesYield = function(State) {
    State = YP.getValue(State);
    State._codeUsesYield = true;
}

CompilerState.codeUsesYield = function(State) {
    State = YP.getValue(State);
    return State._codeUsesYield;
}

CompilerState.determinismEquals = function(State, Term) {
    State = YP.getValue(State);
    return YP.termEqual(State._determinism, Term);
}

// Set _variableNames to a new list of (Name = Variable) for each unique variable in rule.
// If the variable is in variableNameSuggestions, use it, otherwise use x1, x2, etc.
CompilerState.newVariableNames = function(State, Rule, VariableNameSuggestions) {
    State = YP.getValue(State);
    var variablesSet = [];
    YP.addUniqueVariables(Rule, variablesSet);

    State._variableNames = [];
    var xCounter = 0;
    for each (var variable in variablesSet)
        State._variableNames.push
            (new Functor2(Atom.a("="), CompilerState.makeVariableName(variable, VariableNameSuggestions, ++xCounter), 
			 variable));
}

CompilerState.makeVariableName = function(variable, variableNameSuggestions, xCounter) {
    // Debug: should require named variables to start with _ or capital. Should
    //   check for duplicates and clashes with keywords.
    for (var element = YP.getValue(variableNameSuggestions);
         element instanceof Functor2 && element._name == Atom.DOT;
         element = YP.getValue(element._arg2)) {
        var suggestionPair = YP.getValue(element._arg1);
        if (sameVariable(variable, suggestionPair._arg2)) {
            var suggestion = YP.getValue(suggestionPair._arg1);
            if (suggestion == Atom.a("Atom"))
                suggestion = Atom.a("Atom_1");
            if (suggestion == Atom.a("Variable"))
                suggestion = Atom.a("Variable_1");
            if (suggestion == Atom.a("Functor"))
                suggestion = Atom.a("Functor_1");
            return suggestion;
        }
    }

    return Atom.a("x" + xCounter);
}

// Unify Result with the name assigned by CompilerState.newVariableNames in State._variableNames
//   for variable.
CompilerState.getVariableName = function(State, variable, Result) {
    State = YP.getValue(State);
    for each (var variableInfo in State._variableNames) {
        if (variableInfo instanceof Functor2 && variableInfo._name == Atom.a("=")) {
            if (sameVariable(variable, variableInfo._arg2))
                return YP.unify(Result, variableInfo._arg1);
        }
    }

    // We set up names for all unique variables, so this should never happen.
    throw new PrologException(Atom.a("Can't find entry in _variableNames"));
}

CompilerState.variableNamesList = function(State, VariableNamesList) {
    State = YP.getValue(State);
    return YP.unify(VariableNamesList, ListPair.make(State._variableNames));
}

CompilerState.gensym = function(State, Base, Symbol) {
    State = YP.getValue(State);
    return YP.unify(Symbol, Atom.a(Base.toString() + ++State._gensymCounter));
}

CompilerState.isDetNoneOut = function(State, Term) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Term);
    var functorArgs = YP.getFunctorArgs(Term);

    var pred = new Variable();
    for each (var l1 in State._pred.match([functorName, functorArgs.length, pred, Atom.a("det")])) {
        if (CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue()))) {
            return true;
        }
    }

    return false;
}

CompilerState.isSemidetNoneOut = function(State, Term) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Term);
    var functorArgs = YP.getFunctorArgs(Term);

    var pred = new Variable();
    for each (var l1 in State._pred.match([functorName, functorArgs.length, pred, Atom.a("semidet")])) {
        if (CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue()))) {
            return true;
        }
    }

    return false;
}

// Return false if any of args is out, otherwise true.
// args is an array of ::(Type,Mode) where Mode is in or out.
CompilerState.isNoneOut = function(args) {
    for each (var arg in args) {
        if (arg instanceof Functor2 && arg._name == Atom.a("::") && arg._arg2 == Atom.a("out"))
            return false;
    }
    return true;
}

CompilerState.nameArityHasModule = function(State, Name, Arity, Module) {
    State = YP.getValue(State);
    Name = YP.getValue(Name);
    Arity = YP.getValue(Arity);
    Module = YP.getValue(Module);
    if (Name instanceof Atom && typeof(Arity) == "number") {
        var FoundModule = State._moduleForNameArity[Name + "/" + Arity];
        if (!FoundModule === undefined)
            return false;
        return FoundModule == Module;
    }
    return false;
}

function Compiler() {
}

// Use makeFunctionPseudoCode, convertFunctionJavascript and compileAnonymousFunction
// to return an anonymous YP.IClause for the Head and Body of a rule clause.
// Head is a prolog term such as new Functor2("test1", X, Y).
// Note that the name of the head is ignored.
// Body is a prolog term such as 
// new Functor2(",", new Functor1(Atom.a("test2", Atom.a("")), X), 
//              new Functor2("=", Y, X)).
// This may not be null.  (For a head-only clause, set the Body to Atom.a("true"). 
// (This has no declaringClass because it is assumed that predicates with default module Atom.a("")
//  are in the global scope.)
// Returns a new object on which you can call match(args) where
// args length is the arity of the Head.
Compiler.compileAnonymousClause = function(Head, Body) {
    var args = YP.getFunctorArgs(Head);
    // compileAnonymousFunction wants "function".
    var Rule = new Functor2(Atom.RULE, Functor.make("function", args), Body);
    var RuleList = ListPair.make(new Functor2(Atom.F, Rule, Atom.NIL));

    var functionCode = new YP.StringWriter();
    var SaveOutputStream = new Variable();
    for each (var l1 in YP.current_output(SaveOutputStream)) {
        try {
            YP.tell(functionCode);
            var PseudoCode = new Variable();
            for each (var l2 in makeFunctionPseudoCode(RuleList, PseudoCode)) {
                if (YP.termEqual(PseudoCode, Atom.a("getDeclaringClass")))
                    // Ignore getDeclaringClass since we have access to the one passed in.
                    continue;

                convertFunctionJavascript(PseudoCode);
            }
            YP.told();
        }
        finally {
            // Restore after calling tell.
            YP.tell(SaveOutputStream.getValue());
        }
    }

    return Compiler.compileAnonymousFunction(functionCode.toString(), args.length);
}
           
// Use eval to compile the functionCode and return a YP.ClauseHeadAndBody object which implements
//   match(args) which is called with an array of the arguments to match the clause.
// functionCode is the code for the iterator, such as
// "function() { yield false; }"
// nArgs is the number of args in the function.
Compiler.compileAnonymousFunction = function(functionCode, nArgs) {
    var matchCode = new YP.StringWriter();
    matchCode.write("(function(args) { return this._function("); 
    if (nArgs >= 1)
        matchCode.write("args[0]");
    for (var i = 1; i < nArgs; ++i)
        matchCode.write(", args[" + i + "]");
    matchCode.write("); })");
  
    var clause = new YP.ClauseHeadAndBody();
    // Put inside parentheses to make a syntactically valid expression.
    clause._function = eval("(" + functionCode + ")");
    clause.match = eval(matchCode.toString());
    return clause; 
}

// If the functor with name and args can be called directly as determined by
//   functorCallFunctionName, then call it and return its iterator.  If the predicate is
//   dynamic and undefined, or if static and the method cannot be found, return
//   the result of YP.unknownPredicate.
// declaringClass is used to resolve references to the default 
// module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
//   null, this looks in the global scope.  If not found, this throws a 
//   PrologException for existence_error.
// This returns null if the functor has a special form than needs to be compiled 
//   (including ,/2 and ;/2).
Compiler.getSimpleIterator = function(name, args, declaringClass) {
    var state = new CompilerState();
    var FunctionName = new Variable();
    for each (var l1 in functorCallFunctionName(state, name, args.length, FunctionName)) {
        var functionNameAtom = FunctionName.getValue();
        if (functionNameAtom == Atom.NIL)
            // name is for a dynamic predicate.
            return YP.matchDynamic(name, args);

        var methodName = functionNameAtom._name;
        // Set the default for the method to call.
        var methodClass = declaringClass;

        var checkMode = false;
        if (methodName.substr(0,2) == "YP.") {
            // Assume we only check mode in calls to standard Prolog predicates in YP.
            checkMode = true;

            // Use the method in class YP.
            methodName = methodName.substr(3);
            methodClass = YP;
        }
        if (methodName.indexOf(".") >= 0)
            // We don't support calling inner classes, etc.
            return null;

        var func = null;
	    try {
            if (methodClass == null)
                // Look in the global scope.
	            func = eval(methodName);
	        else
	            func = methodClass[methodName];
	             
	        if (func === undefined || typeof(func) != "function")
	            func = null;
	    } catch (e) {
	        func = null;
        }

        if (func == null)
            throw new PrologException
                (new Functor2
                 (Atom.a("existence_error"), Atom.a("procedure"),
                  new Functor2(Atom.a("/"), name, args.length)),
                 "Cannot find predicate function " + methodName + " for " + name + "/" + args.length + 
                 " in " + (methodClass == null ? "the global scope" : methodClass.toString()));
        
        if (checkMode) {
            assertYPPred(state);
            var functor = Functor.make(name, args);
            if (CompilerState.isDetNoneOut(state, functor)) {
                func.apply(null, args);
                return YP.succeed();
            }
            if (CompilerState.isSemidetNoneOut(state, functor)) {
                if (func.apply(null, args))
                    return YP.succeed();
                else
                    return YP.fail();
            }
        }
        return Iterator(func.apply(null, args));
    }

    return null;
}

// Return true if there is a dynamic or static predicate with name and arity.
// This returns false for built-in predicates.
// declaringClass: used to resolve references to the default 
// module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
//   null, return false.
Compiler.isCurrentPredicate = function(name, arity, declaringClass) {
    var state = new CompilerState();
    var FunctionName = new Variable();
    for each (var l1 in functorCallFunctionName(state, name, arity, FunctionName)) {
        var functionNameAtom = FunctionName.getValue();
        if (functionNameAtom == Atom.NIL)
            // name is for a dynamic predicate.
            return YP.isDynamicCurrentPredicate(name, arity);

        var methodName = functionNameAtom._name;

        if (methodName.substr(0,2) == "YP.")
            // current_predicate/1 should fail for built-ins.
            return false;
        if (methodName.indexOf(".") >= 0)
            // We don't support calling inner classes, etc.
            return false;
            
        try {
            // Look in the global scope.
	  	    var func = eval(methodName);
	  	    // Note that Javascript doesn't let us check the arity, but make sure it's a function.
	  	    return typeof(func) == "function";
	  	} catch (e) {
	  	    // eval didn't find it.
	  	    return false;
	  	}
    }

    return false;
}

// Compiler output follows.

function getDeclaringClass() { return null; }

function repeatWrite(arg1, N) {
  {
    var _Value = arg1;
    if (YP.termEqual(N, 0)) {
      return;
    }
  }
  {
    var Value = arg1;
    var NextN = new Variable();
    YP.write(Value);
    for each (var l2 in YP.unify(NextN, YP.subtract(N, 1))) {
      repeatWrite(Value, NextN);
      return;
    }
  }
}

function sameVariable(Variable1, Variable2) {
  {
    if (YP.var(Variable1)) {
      if (YP.var(Variable2)) {
        if (YP.termEqual(Variable1, Variable2)) {
          return true;
        }
      }
    }
  }
  return false;
}

function makeFunctionPseudoCode(RuleList, FunctionCode) {
  {
    var State = new Variable();
    for each (var l2 in CompilerState.make(State)) {
      assertYPPred(State);
      processCompilerDirectives(RuleList, State);
      for each (var l3 in YP.unify(FunctionCode, Atom.a("getDeclaringClass"))) {
        yield false;
      }
      for each (var l3 in makeFunctionPseudoCode3(RuleList, State, FunctionCode)) {
        yield false;
      }
    }
  }
}

function assertYPPred(State) {
  {
    CompilerState.assertPred(State, Atom.a("nl"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("write", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("put_code", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("see", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, Atom.a("seen"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("tell", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, Atom.a("told"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("throw", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("abolish", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("retractall", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor2("set_prolog_flag", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("var", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("nonvar", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("atom", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("integer", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("float", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("number", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("atomic", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("compound", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("ground", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("==", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("\\==", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@<", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@=<", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@>", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@>=", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    return;
  }
}

function processCompilerDirectives(arg1, arg2) {
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var State = arg2;
    var Pred = new Variable();
    var Determinism = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", new Functor1("pred", new Functor2("is", Pred, Determinism))), x3), RestRules))) {
      CompilerState.assertPred(State, Pred, Determinism);
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
  {
    var State = arg2;
    var Module = new Variable();
    var PredicateList = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", new Functor2("import", Module, PredicateList)), x3), RestRules))) {
      for each (var l3 in importPredicateList(State, Module, PredicateList)) {
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var x1 = new Variable();
    var x2 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", x1), x2), RestRules))) {
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
  {
    var State = arg2;
    var Head = new Variable();
    var _Body = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor2(":-", Head, _Body), x3), RestRules))) {
      for each (var l3 in YP.functor(Head, Name, Arity)) {
        CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""));
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var Fact = new Variable();
    var x2 = new Variable();
    var RestRules = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", Fact, x2), RestRules))) {
      for each (var l3 in YP.functor(Fact, Name, Arity)) {
        CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""));
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var x1 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, RestRules))) {
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
}

function importPredicateList(arg1, arg2, arg3) {
  {
    var _State = arg1;
    var _Module = arg2;
    for each (var l2 in YP.unify(arg3, Atom.NIL)) {
      yield true;
      return;
    }
  }
  {
    var State = arg1;
    var Module = arg2;
    var Name = new Variable();
    var Arity = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor2("/", Name, Arity), Rest))) {
      CompilerState.assertModuleForNameArity(State, Name, Arity, Module);
      for each (var l3 in importPredicateList(State, Module, Rest)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg1;
    var Module = arg2;
    var x3 = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(x3, Rest))) {
      for each (var l3 in importPredicateList(State, Module, Rest)) {
        yield true;
        return;
      }
    }
  }
}

function makeFunctionPseudoCode3(RuleList, State, FunctionCode) {
  {
    var SamePredicateRuleList = new Variable();
    var RestRules = new Variable();
    for each (var l2 in samePredicateRuleList(RuleList, SamePredicateRuleList, RestRules)) {
      if (YP.termNotEqual(SamePredicateRuleList, Atom.NIL)) {
        for each (var l4 in compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode)) {
          yield false;
        }
        for each (var l4 in makeFunctionPseudoCode3(RestRules, State, FunctionCode)) {
          yield false;
        }
      }
    }
  }
}

function compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode) {
  {
    var FirstRule = new Variable();
    var x5 = new Variable();
    var x6 = new Variable();
    var x7 = new Variable();
    var Head = new Variable();
    var x9 = new Variable();
    var ArgAssignments = new Variable();
    var Calls = new Variable();
    var Rule = new Variable();
    var VariableNameSuggestions = new Variable();
    var ClauseBag = new Variable();
    var Name = new Variable();
    var ArgsList = new Variable();
    var FunctionArgNames = new Variable();
    var MergedArgName = new Variable();
    var ArgName = new Variable();
    var MergedArgNames = new Variable();
    var FunctionArgs = new Variable();
    var BodyCode = new Variable();
    var ReturnType = new Variable();
    var BodyWithReturn = new Variable();
    for each (var l2 in YP.unify(new ListPair(new Functor2("f", FirstRule, x5), x6), SamePredicateRuleList)) {
      cutIf1:
      {
        for each (var l4 in YP.unify(FirstRule, new Functor1(":-", x7))) {
          break cutIf1;
        }
        cutIf2:
        {
          for each (var l5 in YP.unify(new Functor2(":-", Head, x9), FirstRule)) {
            CompilerState.startFunction(State, Head);
            var findallAnswers3 = new FindallAnswers(new Functor2("f", ArgAssignments, Calls));
            for each (var l6 in member(new Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList)) {
              for each (var l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls)) {
                findallAnswers3.add();
              }
            }
            for each (var l6 in findallAnswers3.result(ClauseBag)) {
              for each (var l7 in YP.univ(Head, new ListPair(Name, ArgsList))) {
                for each (var l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames)) {
                  var findallAnswers4 = new FindallAnswers(MergedArgName);
                  for each (var l9 in member(ArgName, FunctionArgNames)) {
                    cutIf5:
                    {
                      for each (var l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName)) {
                        findallAnswers4.add();
                        break cutIf5;
                      }
                      for each (var l11 in YP.unify(MergedArgName, ArgName)) {
                        findallAnswers4.add();
                      }
                    }
                  }
                  for each (var l9 in findallAnswers4.result(MergedArgNames)) {
                    for each (var l10 in maplist_arg(MergedArgNames, FunctionArgs)) {
                      for each (var l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode)) {
                        cutIf6:
                        {
                          if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                            for each (var l14 in YP.unify(ReturnType, Atom.a("void"))) {
                              cutIf7:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf7;
                                }
                                cutIf8:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf8;
                                  }
                                  cutIf9:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf9;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            break cutIf6;
                          }
                          cutIf10:
                          {
                            if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                              for each (var l15 in YP.unify(ReturnType, Atom.a("bool"))) {
                                cutIf11:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                    for each (var l18 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf11;
                                  }
                                  cutIf12:
                                  {
                                    if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf12;
                                    }
                                    cutIf13:
                                    {
                                      if (CompilerState.codeUsesYield(State)) {
                                        for each (var l20 in YP.unify(BodyWithReturn, BodyCode)) {
                                          for each (var l21 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                            yield false;
                                          }
                                        }
                                        break cutIf13;
                                      }
                                      for each (var l19 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              break cutIf10;
                            }
                            for each (var l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>"))) {
                              cutIf14:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf14;
                                }
                                cutIf15:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf15;
                                  }
                                  cutIf16:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf16;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            break cutIf2;
          }
          for each (var l5 in YP.unify(Head, FirstRule)) {
            CompilerState.startFunction(State, Head);
            var findallAnswers17 = new FindallAnswers(new Functor2("f", ArgAssignments, Calls));
            for each (var l6 in member(new Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList)) {
              for each (var l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls)) {
                findallAnswers17.add();
              }
            }
            for each (var l6 in findallAnswers17.result(ClauseBag)) {
              for each (var l7 in YP.univ(Head, new ListPair(Name, ArgsList))) {
                for each (var l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames)) {
                  var findallAnswers18 = new FindallAnswers(MergedArgName);
                  for each (var l9 in member(ArgName, FunctionArgNames)) {
                    cutIf19:
                    {
                      for each (var l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName)) {
                        findallAnswers18.add();
                        break cutIf19;
                      }
                      for each (var l11 in YP.unify(MergedArgName, ArgName)) {
                        findallAnswers18.add();
                      }
                    }
                  }
                  for each (var l9 in findallAnswers18.result(MergedArgNames)) {
                    for each (var l10 in maplist_arg(MergedArgNames, FunctionArgs)) {
                      for each (var l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode)) {
                        cutIf20:
                        {
                          if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                            for each (var l14 in YP.unify(ReturnType, Atom.a("void"))) {
                              cutIf21:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf21;
                                }
                                cutIf22:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf22;
                                  }
                                  cutIf23:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf23;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            break cutIf20;
                          }
                          cutIf24:
                          {
                            if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                              for each (var l15 in YP.unify(ReturnType, Atom.a("bool"))) {
                                cutIf25:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                    for each (var l18 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf25;
                                  }
                                  cutIf26:
                                  {
                                    if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf26;
                                    }
                                    cutIf27:
                                    {
                                      if (CompilerState.codeUsesYield(State)) {
                                        for each (var l20 in YP.unify(BodyWithReturn, BodyCode)) {
                                          for each (var l21 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                            yield false;
                                          }
                                        }
                                        break cutIf27;
                                      }
                                      for each (var l19 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              break cutIf24;
                            }
                            for each (var l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>"))) {
                              cutIf28:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf28;
                                }
                                cutIf29:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf29;
                                  }
                                  cutIf30:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf30;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function samePredicateRuleList(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var First = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, new ListPair(First, Atom.NIL))) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var SamePredicateRuleList = arg2;
    var RestRules = arg3;
    var First = new Variable();
    var Rest = new Variable();
    var FirstRule = new Variable();
    var x6 = new Variable();
    var SecondRule = new Variable();
    var x8 = new Variable();
    var x9 = new Variable();
    var FirstHead = new Variable();
    var x11 = new Variable();
    var SecondHead = new Variable();
    var x13 = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    var RestSamePredicates = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(new Functor2("f", FirstRule, x6), First)) {
        for each (var l4 in YP.unify(new ListPair(new Functor2("f", SecondRule, x8), x9), Rest)) {
          cutIf1:
          {
            for each (var l6 in YP.unify(new Functor2(":-", FirstHead, x11), FirstRule)) {
              cutIf2:
              {
                for each (var l8 in YP.unify(new Functor2(":-", SecondHead, x13), SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf3:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf3;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                  break cutIf2;
                }
                for each (var l8 in YP.unify(SecondHead, SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf4:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf4;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                }
              }
              break cutIf1;
            }
            for each (var l6 in YP.unify(FirstHead, FirstRule)) {
              cutIf5:
              {
                for each (var l8 in YP.unify(new Functor2(":-", SecondHead, x13), SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf6:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf6;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                  break cutIf5;
                }
                for each (var l8 in YP.unify(SecondHead, SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf7:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf7;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function maplist_compileClause(arg1, arg2, arg3) {
  {
    var _MergedArgNames = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var MergedArgNames = arg2;
    var ArgAssignments = new Variable();
    var Calls = new Variable();
    var Rest = new Variable();
    var ClauseCode = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", ArgAssignments, Calls), Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("blockScope", ClauseCode), RestResults))) {
        for each (var l4 in prependArgAssignments(ArgAssignments, Calls, MergedArgNames, ClauseCode)) {
          for each (var l5 in maplist_compileClause(Rest, MergedArgNames, RestResults)) {
            yield true;
            return;
          }
        }
      }
    }
  }
}

function prependArgAssignments(arg1, arg2, arg3, arg4) {
  {
    var _MergedArgNames = arg3;
    var In = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, In)) {
        for each (var l4 in YP.unify(arg4, In)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var In = arg2;
    var MergedArgNames = arg3;
    var ClauseCode = arg4;
    var VariableName = new Variable();
    var ArgName = new Variable();
    var RestArgAssignments = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", VariableName, ArgName), RestArgAssignments))) {
      cutIf1:
      {
        for each (var l4 in member(VariableName, MergedArgNames)) {
          for each (var l5 in prependArgAssignments(RestArgAssignments, In, MergedArgNames, ClauseCode)) {
            yield true;
            return;
          }
          break cutIf1;
        }
        for each (var l4 in prependArgAssignments(RestArgAssignments, new ListPair(new Functor3("declare", Atom.a("object"), VariableName, new Functor1("var", ArgName)), In), MergedArgNames, ClauseCode)) {
          yield true;
          return;
        }
      }
    }
  }
}

function argAssignedAll(arg1, arg2, VariableName) {
  {
    var _ArgName = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      if (YP.nonvar(VariableName)) {
        yield true;
        return;
      }
    }
  }
  {
    var ArgName = arg1;
    var ArgAssignments = new Variable();
    var _Calls = new Variable();
    var RestClauseBag = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(new Functor2("f", ArgAssignments, _Calls), RestClauseBag))) {
      for each (var l3 in member(new Functor2("f", VariableName, ArgName), ArgAssignments)) {
        for each (var l4 in argAssignedAll(ArgName, RestClauseBag, VariableName)) {
          yield false;
        }
      }
    }
  }
}

function maplist_arg(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var First = new Variable();
    var Rest = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("arg", First), RestResults))) {
        for each (var l4 in maplist_arg(Rest, RestResults)) {
          yield true;
          return;
        }
      }
    }
  }
}

function getFunctionArgNames(arg1, arg2, arg3) {
  {
    var _StartArgNumber = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var StartArgNumber = arg2;
    var x1 = new Variable();
    var Rest = new Variable();
    var ArgName = new Variable();
    var RestFunctionArgs = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var NextArgNumber = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(ArgName, RestFunctionArgs))) {
        for each (var l4 in YP.number_codes(StartArgNumber, NumberCodes)) {
          for each (var l5 in YP.atom_codes(NumberAtom, NumberCodes)) {
            for each (var l6 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
              for each (var l7 in YP.unify(NextArgNumber, YP.add(StartArgNumber, 1))) {
                for each (var l8 in getFunctionArgNames(Rest, NextArgNumber, RestFunctionArgs)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls) {
  {
    var Head = new Variable();
    var Body = new Variable();
    var x8 = new Variable();
    var HeadArgs = new Variable();
    var CompiledHeadArgs = new Variable();
    var BodyCode = new Variable();
    var VariableNamesList = new Variable();
    var ArgUnifications = new Variable();
    for each (var l2 in YP.unify(new Functor2(":-", Head, Body), Rule)) {
      CompilerState.newVariableNames(State, Rule, VariableNameSuggestions);
      for each (var l3 in YP.univ(Head, new ListPair(x8, HeadArgs))) {
        for each (var l4 in maplist_compileTerm(HeadArgs, State, CompiledHeadArgs)) {
          for each (var l5 in compileRuleBody(Body, State, BodyCode)) {
            for each (var l6 in CompilerState.variableNamesList(State, VariableNamesList)) {
              for each (var l7 in compileArgUnifications(HeadArgs, CompiledHeadArgs, 1, HeadArgs, BodyCode, ArgUnifications)) {
                for each (var l8 in compileDeclarations(VariableNamesList, HeadArgs, Atom.NIL, ArgAssignments, ArgUnifications, Calls)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    for each (var l2 in compileBodyWithHeadBindings(new Functor2(":-", Rule, Atom.a("true")), VariableNameSuggestions, State, ArgAssignments, Calls)) {
      yield true;
      return;
    }
  }
}

function compileArgUnifications(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var BodyCode = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg5, BodyCode)) {
        for each (var l4 in YP.unify(arg6, BodyCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var Index = arg3;
    var AllHeadArgs = arg4;
    var BodyCode = arg5;
    var ArgUnifications = arg6;
    var HeadArg = new Variable();
    var RestHeadArgs = new Variable();
    var x3 = new Variable();
    var RestCompiledHeadArgs = new Variable();
    var _ArgIndex1 = new Variable();
    var NextIndex = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(HeadArg, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(x3, RestCompiledHeadArgs))) {
        for each (var l4 in getVariableArgIndex1(HeadArg, AllHeadArgs, _ArgIndex1)) {
          for each (var l5 in YP.unify(NextIndex, YP.add(Index, 1))) {
            for each (var l6 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, ArgUnifications)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Index = arg3;
    var AllHeadArgs = arg4;
    var BodyCode = arg5;
    var _HeadArg = new Variable();
    var RestHeadArgs = new Variable();
    var CompiledHeadArg = new Variable();
    var RestCompiledHeadArgs = new Variable();
    var ArgName = new Variable();
    var RestArgUnifications = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var NextIndex = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(_HeadArg, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(CompiledHeadArg, RestCompiledHeadArgs))) {
        for each (var l4 in YP.unify(arg6, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.unify"), new ListPair(new Functor1("var", ArgName), new ListPair(CompiledHeadArg, Atom.NIL))), RestArgUnifications), Atom.NIL))) {
          for each (var l5 in YP.number_codes(Index, NumberCodes)) {
            for each (var l6 in YP.atom_codes(NumberAtom, NumberCodes)) {
              for each (var l7 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
                for each (var l8 in YP.unify(NextIndex, YP.add(Index, 1))) {
                  for each (var l9 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, RestArgUnifications)) {
                    yield true;
                    return;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileDeclarations(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var _HeadArgs = arg2;
    var ArgAssignmentsIn = new Variable();
    var DeclarationsIn = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, ArgAssignmentsIn)) {
        for each (var l4 in YP.unify(arg4, ArgAssignmentsIn)) {
          for each (var l5 in YP.unify(arg5, DeclarationsIn)) {
            for each (var l6 in YP.unify(arg6, DeclarationsIn)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var HeadArgs = arg2;
    var ArgAssignmentsIn = arg3;
    var ArgAssignmentsOut = arg4;
    var DeclarationsIn = arg5;
    var DeclarationsOut = arg6;
    var VariableName = new Variable();
    var Var = new Variable();
    var RestVariableNames = new Variable();
    var ArgIndex1 = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var ArgName = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", VariableName, Var), RestVariableNames))) {
      for each (var l3 in getVariableArgIndex1(Var, HeadArgs, ArgIndex1)) {
        for each (var l4 in YP.number_codes(ArgIndex1, NumberCodes)) {
          for each (var l5 in YP.atom_codes(NumberAtom, NumberCodes)) {
            for each (var l6 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
              for each (var l7 in compileDeclarations(RestVariableNames, HeadArgs, new ListPair(new Functor2("f", VariableName, ArgName), ArgAssignmentsIn), ArgAssignmentsOut, DeclarationsIn, DeclarationsOut)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var HeadArgs = arg2;
    var ArgAssignmentsIn = arg3;
    var ArgAssignmentsOut = arg4;
    var DeclarationsIn = arg5;
    var VariableName = new Variable();
    var _Var = new Variable();
    var RestVariableNames = new Variable();
    var DeclarationsOut = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", VariableName, _Var), RestVariableNames))) {
      for each (var l3 in YP.unify(arg6, new ListPair(new Functor3("declare", Atom.a("Variable"), VariableName, new Functor2("new", Atom.a("Variable"), Atom.NIL)), DeclarationsOut))) {
        for each (var l4 in compileDeclarations(RestVariableNames, HeadArgs, ArgAssignmentsIn, ArgAssignmentsOut, DeclarationsIn, DeclarationsOut)) {
          yield true;
          return;
        }
      }
    }
  }
}

function getVariableArgIndex1(Var, arg2, arg3) {
  {
    var FirstHeadArgs = new Variable();
    var RestHeadArgs = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(FirstHeadArgs, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg3, 1)) {
        if (sameVariable(Var, FirstHeadArgs)) {
          cutIf1:
          {
            for each (var l6 in getVariableArgIndex1(Var, RestHeadArgs, x4)) {
              break cutIf1;
            }
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Index = arg3;
    var x2 = new Variable();
    var RestHeadArgs = new Variable();
    var RestIndex = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(x2, RestHeadArgs))) {
      for each (var l3 in getVariableArgIndex1(Var, RestHeadArgs, RestIndex)) {
        for each (var l4 in YP.unify(Index, YP.add(1, RestIndex))) {
          yield true;
          return;
        }
      }
    }
  }
}

function compileRuleBody(arg1, arg2, arg3) {
  {
    var A = arg1;
    var State = arg2;
    var PseudoCode = arg3;
    if (YP.var(A)) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("call", A), Atom.a("true")), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      if (YP.var(A)) {
        for each (var l4 in compileRuleBody(new Functor2(",", new Functor1("call", A), B), State, PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in compileFunctorCall(A, State, ACode)) {
        if (CompilerState.isDetNoneOut(State, A)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            for each (var l6 in YP.unify(PseudoCode, new ListPair(ACode, BCode))) {
              yield true;
              return;
            }
          }
        }
        if (CompilerState.isSemidetNoneOut(State, A)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            for each (var l6 in YP.unify(PseudoCode, new ListPair(new Functor2("if", ACode, BCode), Atom.NIL))) {
              yield true;
              return;
            }
          }
        }
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          for each (var l5 in YP.unify(PseudoCode, new ListPair(new Functor2("foreach", ACode, BCode), Atom.NIL))) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(";", new Functor2("->", A, T), B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(";", new Functor2("->", A, new Functor2(",", T, C)), new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(";", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(";", new Functor2(",", A, C), new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("\\+", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("if", new Functor1("not", ACode), BCode), Atom.NIL))) {
        if (CompilerState.isSemidetNoneOut(State, A)) {
          for each (var l5 in compileFunctorCall(A, State, ACode)) {
            for each (var l6 in compileRuleBody(B, State, BCode)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("\\+", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, Atom.a("fail")), Atom.a("true")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("once", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, Atom.a("true")), Atom.a("fail")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("->", A, T), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, T), Atom.a("fail")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("\\=", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("\\+", new Functor2("=", A, B)), C), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var ACode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("!"), A))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in append(ACode, new ListPair(Atom.a("yieldbreak"), Atom.NIL), PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Name = new Variable();
    var A = new Variable();
    var ACode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("$CUTIF", Name), A))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in append(ACode, new ListPair(new Functor1("breakBlock", Name), Atom.NIL), PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg2;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("fail"), x1))) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("true"), A))) {
      for each (var l3 in compileRuleBody(A, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var Term = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var TermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("is", A, Term), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.unify"), new ListPair(ACode, new ListPair(TermCode, Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ACode)) {
          for each (var l5 in compileExpression(Term, State, TermCode)) {
            for each (var l6 in compileRuleBody(B, State, BCode)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var ACode = new Variable();
    var B = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("$DET_NONE_OUT", ACode), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(ACode, BCode))) {
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var FunctionName = new Variable();
    var X1Code = new Variable();
    var X2Code = new Variable();
    var BCode = new Variable();
    var Name = new Variable();
    var X1 = new Variable();
    var X2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("if", new Functor2("call", FunctionName, new ListPair(X1Code, new ListPair(X2Code, Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in YP.univ(A, ListPair.make([Name, X1, X2]))) {
          for each (var l5 in binaryExpressionConditional(Name, FunctionName)) {
            for each (var l6 in compileExpression(X1, State, X1Code)) {
              for each (var l7 in compileExpression(X2, State, X2Code)) {
                for each (var l8 in compileRuleBody(B, State, BCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    var TemplateCode = new Variable();
    var FindallAnswers = new Variable();
    var GoalAndAddCode = new Variable();
    var BagCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("findall", Template, Goal, Bag), B))) {
      for each (var l3 in compileTerm(Template, State, TemplateCode)) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("findallAnswers"), FindallAnswers)) {
          for each (var l5 in compileRuleBody(new Functor2(",", Goal, new Functor2(",", new Functor1("$DET_NONE_OUT", new Functor3("callMember", new Functor1("var", FindallAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode)) {
            for each (var l6 in compileTerm(Bag, State, BagCode)) {
              for each (var l7 in compileRuleBody(B, State, BCode)) {
                for each (var l8 in append(new ListPair(new Functor3("declare", Atom.a("FindallAnswers"), FindallAnswers, new Functor2("new", Atom.a("FindallAnswers"), new ListPair(TemplateCode, Atom.NIL))), GoalAndAddCode), new ListPair(new Functor2("foreach", new Functor3("callMember", new Functor1("var", FindallAnswers), Atom.a("result"), new ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("bagof", Template, Goal, Bag), B))) {
      for each (var l3 in compileBagof(Atom.a("result"), Template, Goal, Bag, B, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("setof", Template, Goal, Bag), B))) {
      for each (var l3 in compileBagof(Atom.a("resultSet"), Template, Goal, Bag, B, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("call", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.getIterator"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("current_predicate", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.current_predicate"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("asserta", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("call", Atom.a("YP.asserta"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("assertz", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("call", Atom.a("YP.assertz"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("assert", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("assertz", A), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var Goal = new Variable();
    var Catcher = new Variable();
    var Handler = new Variable();
    var B = new Variable();
    var CatchGoal = new Variable();
    var GoalTermCode = new Variable();
    var BCode = new Variable();
    var CatcherTermCode = new Variable();
    var HandlerAndBCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("catch", Goal, Catcher, Handler), B))) {
      for each (var l3 in YP.unify(arg3, ListPair.make([new Functor3("declare", Atom.a("YP.Catch"), CatchGoal, new Functor2("new", Atom.a("YP.Catch"), new ListPair(GoalTermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL)))), new Functor2("foreach", new Functor1("var", CatchGoal), BCode), new Functor2("foreach", new Functor3("callMember", new Functor1("var", CatchGoal), Atom.a("unifyExceptionOrThrow"), new ListPair(CatcherTermCode, Atom.NIL)), HandlerAndBCode)]))) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("catchGoal"), CatchGoal)) {
          for each (var l5 in compileTerm(Goal, State, GoalTermCode)) {
            for each (var l6 in compileTerm(Catcher, State, CatcherTermCode)) {
              for each (var l7 in compileRuleBody(B, State, BCode)) {
                for each (var l8 in compileRuleBody(new Functor2(",", Handler, B), State, HandlerAndBCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(",", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(",", A, new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", A, B))) {
      if (YP.var(A)) {
        for each (var l4 in compileRuleBody(new Functor2(";", new Functor1("call", A), B), State, PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    var CutIfLabel = new Variable();
    var Code = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", new Functor2("->", A, T), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("breakableBlock", CutIfLabel, Code), Atom.NIL))) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("cutIf"), CutIfLabel)) {
          for each (var l5 in compileRuleBody(new Functor2(";", new Functor2(",", A, new Functor2(",", new Functor1("$CUTIF", CutIfLabel), T)), B), State, Code)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var _B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", Atom.a("!"), _B))) {
      for each (var l3 in compileRuleBody(Atom.a("!"), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", A, B))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          for each (var l5 in append(ACode, BCode, PseudoCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("return"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("returntrue"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("yieldtrue"), new ListPair(Atom.a("yieldbreak"), Atom.NIL)))) {
        CompilerState.setCodeUsesYield(State);
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg2;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("$CUTIF", Name))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("breakBlock", Name), Atom.NIL))) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("return"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("returntrue"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("yieldfalse"), Atom.NIL))) {
        CompilerState.setCodeUsesYield(State);
        yield true;
        return;
      }
    }
  }
  {
    var A = arg1;
    var State = arg2;
    var PseudoCode = arg3;
    for each (var l2 in compileRuleBody(new Functor2(",", A, Atom.a("true")), State, PseudoCode)) {
      yield true;
      return;
    }
  }
}

function compileBagof(ResultMethod, Template, Goal, Bag, B, State, PseudoCode) {
  {
    var TemplateCode = new Variable();
    var GoalTermCode = new Variable();
    var UnqualifiedGoal = new Variable();
    var BagofAnswers = new Variable();
    var GoalAndAddCode = new Variable();
    var BagCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in compileTerm(Template, State, TemplateCode)) {
      for each (var l3 in compileTerm(Goal, State, GoalTermCode)) {
        for each (var l4 in unqualifiedGoal(Goal, UnqualifiedGoal)) {
          for each (var l5 in CompilerState.gensym(State, Atom.a("bagofAnswers"), BagofAnswers)) {
            for each (var l6 in compileRuleBody(new Functor2(",", UnqualifiedGoal, new Functor2(",", new Functor1("$DET_NONE_OUT", new Functor3("callMember", new Functor1("var", BagofAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode)) {
              for each (var l7 in compileTerm(Bag, State, BagCode)) {
                for each (var l8 in compileRuleBody(B, State, BCode)) {
                  for each (var l9 in append(new ListPair(new Functor3("declare", Atom.a("BagofAnswers"), BagofAnswers, new Functor2("new", Atom.a("BagofAnswers"), new ListPair(TemplateCode, new ListPair(GoalTermCode, Atom.NIL)))), GoalAndAddCode), new ListPair(new Functor2("foreach", new Functor3("callMember", new Functor1("var", BagofAnswers), ResultMethod, new ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode)) {
                    yield true;
                    return;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function unqualifiedGoal(arg1, arg2) {
  {
    var Goal = arg1;
    for each (var l2 in YP.unify(arg2, new Functor1("call", Goal))) {
      if (YP.var(Goal)) {
        yield true;
        return;
      }
    }
  }
  {
    var UnqualifiedGoal = arg2;
    var x1 = new Variable();
    var Goal = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("^", x1, Goal))) {
      for each (var l3 in unqualifiedGoal(Goal, UnqualifiedGoal)) {
        yield true;
        return;
      }
    }
  }
  {
    var UnqualifiedGoal = new Variable();
    for each (var l2 in YP.unify(arg1, UnqualifiedGoal)) {
      for each (var l3 in YP.unify(arg2, UnqualifiedGoal)) {
        yield true;
        return;
      }
    }
  }
}

function binaryExpressionConditional(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=:="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.equal"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=\\="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.notEqual"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.greaterThan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.lessThan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.greaterThanOrEqual"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.lessThanOrEqual"))) {
        yield true;
        return;
      }
    }
  }
}

function compileFunctorCall(Functor_1, State, PseudoCode) {
  {
    var FunctorName = new Variable();
    var FunctorArgs = new Variable();
    var x6 = new Variable();
    var Arity = new Variable();
    var FunctionName = new Variable();
    var CompiledArgs = new Variable();
    for each (var l2 in YP.univ(Functor_1, new ListPair(FunctorName, FunctorArgs))) {
      for each (var l3 in YP.functor(Functor_1, x6, Arity)) {
        for each (var l4 in functorCallFunctionName(State, FunctorName, Arity, FunctionName)) {
          for each (var l5 in maplist_compileTerm(FunctorArgs, State, CompiledArgs)) {
            cutIf1:
            {
              if (YP.termEqual(FunctionName, Atom.NIL)) {
                for each (var l8 in YP.unify(PseudoCode, new Functor2("call", Atom.a("YP.matchDynamic"), new ListPair(new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", FunctorName), Atom.NIL)), new ListPair(new Functor1("objectArray", CompiledArgs), Atom.NIL))))) {
                  yield true;
                  return;
                }
                break cutIf1;
              }
              for each (var l7 in YP.unify(PseudoCode, new Functor3("functorCall", FunctionName, FunctorArgs, CompiledArgs))) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
}

function functorCallFunctionName(arg1, arg2, arg3, arg4) {
  {
    var _State = arg1;
    var Name = arg2;
    var Arity = arg3;
    var x4 = arg4;
    if (functorCallIsSpecialForm(Name, Arity)) {
      return;
    }
  }
  {
    var x1 = arg1;
    var Name = arg2;
    var Arity = arg3;
    var FunctionName = arg4;
    for each (var l2 in functorCallYPFunctionName(Name, Arity, FunctionName)) {
      yield true;
      return;
    }
  }
  {
    var State = arg1;
    var Arity = arg3;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg2, Name)) {
      for each (var l3 in YP.unify(arg4, Name)) {
        if (CompilerState.nameArityHasModule(State, Name, Arity, Atom.a(""))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg1;
    var _Arity = arg3;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg2, Name)) {
      for each (var l3 in YP.unify(arg4, Name)) {
        for each (var l4 in Atom.module(Name, Atom.a(""))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var _Arity = arg3;
    for each (var l2 in YP.unify(arg4, Atom.NIL)) {
      for each (var l3 in Atom.module(Name, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var Arity = arg3;
    var x4 = arg4;
    var Module = new Variable();
    var Message = new Variable();
    for each (var l2 in Atom.module(Name, Module)) {
      for each (var l3 in YP.atom_concat(Atom.a("Not supporting calls to external module: "), Module, Message)) {
        YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("callable"), new Functor2("/", Name, Arity)), Message));
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var _Arity = arg3;
    var x4 = arg4;
    YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("callable"), Name), Atom.a("Term is not callable")));
    yield true;
    return;
  }
}

function functorCallIsSpecialForm(Name, Arity) {
  {
    var x3 = new Variable();
    if (YP.termEqual(Arity, 0)) {
      if (YP.termEqual(Name, Atom.a("!"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("fail"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("true"))) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 1)) {
      if (YP.termEqual(Name, Atom.a("\\+"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("once"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("$CUTIF"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("$DET_NONE_OUT"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("call"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("current_predicate"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("asserta"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("assertz"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("assert"))) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 2)) {
      if (YP.termEqual(Name, Atom.a(";"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a(","))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("->"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("\\="))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("is"))) {
        return true;
      }
      for each (var l3 in binaryExpressionConditional(Name, x3)) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 3)) {
      if (YP.termEqual(Name, Atom.a("findall"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("bagof"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("setof"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("catch"))) {
        return true;
      }
    }
  }
  return false;
}

function functorCallYPFunctionName(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.unify"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=.."))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.univ"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("var"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.var"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.nonvar"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.arg"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.functor"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("repeat"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.repeat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("get_code"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.get_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_op"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_op"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_length"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_concat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, 5)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.sub_atom"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_chars"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_codes"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.char_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number_chars"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number_codes"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.copy_term"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sort"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.sort"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nl"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.nl"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("write"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.write"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("put_code"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.put_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("see"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.see"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("seen"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.seen"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("tell"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.tell"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("told"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.told"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.clause"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("retract"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.retract"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.abolish"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("retractall"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.retractall"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.integer"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.isFloat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atomic"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.compound"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("ground"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.ground"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\=="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termNotEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@<"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termLessThan"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@=<"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termLessThanOrEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@>"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termGreaterThan"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@>="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termGreaterThanOrEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("throw"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.throwException"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_prolog_flag"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.set_prolog_flag"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_input"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_input"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_output"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_output"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read_term"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read_term2"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read_term"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read_term3"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read1"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read2"))) {
          yield true;
          return;
        }
      }
    }
  }
}

function compileTerm(arg1, arg2, arg3) {
  {
    var Term = arg1;
    var State = arg2;
    var VariableName = new Variable();
    for each (var l2 in YP.unify(arg3, new Functor1("var", VariableName))) {
      if (YP.var(Term)) {
        for each (var l4 in CompilerState.getVariableName(State, Term, VariableName)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, new Functor1("var", Atom.a("Atom.NIL")))) {
        yield true;
        return;
      }
    }
  }
  {
    var Term = arg1;
    var State = arg2;
    var Code = arg3;
    var ModuleCode = new Variable();
    if (YP.atom(Term)) {
      cutIf1:
      {
        for each (var l4 in compileAtomModule(Term, 0, State, ModuleCode)) {
          for each (var l5 in YP.unify(Code, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Term), new ListPair(ModuleCode, Atom.NIL))))) {
            yield true;
            return;
          }
          break cutIf1;
        }
        for each (var l4 in YP.unify(Code, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Term), Atom.NIL)))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var CompiledList = new Variable();
    var x5 = new Variable();
    var Rest2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new Functor2("call", Atom.a("ListPair.make"), new ListPair(new Functor1("objectArray", CompiledList), Atom.NIL)))) {
        if (YP.nonvar(Rest)) {
          for each (var l5 in YP.unify(Rest, new ListPair(x5, Rest2))) {
            if (YP.termNotEqual(Rest2, Atom.NIL)) {
              for each (var l7 in maplist_compileTerm(new ListPair(First, Rest), State, CompiledList)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var Arg1 = new Variable();
    var Arg2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new Functor2("new", Atom.a("ListPair"), new ListPair(Arg1, new ListPair(Arg2, Atom.NIL))))) {
        for each (var l4 in compileTerm(First, State, Arg1)) {
          for each (var l5 in compileTerm(Rest, State, Arg2)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Term = arg1;
    var State = arg2;
    var Result = arg3;
    var Name = new Variable();
    var TermArgs = new Variable();
    var x6 = new Variable();
    var Arity = new Variable();
    var ModuleCode = new Variable();
    var NameCode = new Variable();
    var X1 = new Variable();
    var Arg1 = new Variable();
    var X2 = new Variable();
    var Arg2 = new Variable();
    var X3 = new Variable();
    var Arg3 = new Variable();
    var Args = new Variable();
    for each (var l2 in YP.univ(Term, new ListPair(Name, TermArgs))) {
      cutIf2:
      {
        if (YP.termEqual(TermArgs, Atom.NIL)) {
          for each (var l5 in YP.unify(Result, new Functor1("object", Name))) {
            yield true;
            return;
          }
          break cutIf2;
        }
        for each (var l4 in YP.functor(Term, x6, Arity)) {
          cutIf3:
          {
            for each (var l6 in compileAtomModule(Name, Arity, State, ModuleCode)) {
              for each (var l7 in YP.unify(NameCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Name), new ListPair(ModuleCode, Atom.NIL))))) {
                cutIf4:
                {
                  for each (var l9 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in YP.unify(Result, new Functor2("new", Atom.a("Functor1"), new ListPair(NameCode, new ListPair(Arg1, Atom.NIL))))) {
                        yield true;
                        return;
                      }
                    }
                    break cutIf4;
                  }
                  cutIf5:
                  {
                    for each (var l10 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                      for each (var l11 in compileTerm(X1, State, Arg1)) {
                        for each (var l12 in compileTerm(X2, State, Arg2)) {
                          for each (var l13 in YP.unify(Result, new Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2])))) {
                            yield true;
                            return;
                          }
                        }
                      }
                      break cutIf5;
                    }
                    for each (var l10 in YP.unify(TermArgs, ListPair.make([X1, X2, X3]))) {
                      for each (var l11 in compileTerm(X1, State, Arg1)) {
                        for each (var l12 in compileTerm(X2, State, Arg2)) {
                          for each (var l13 in compileTerm(X3, State, Arg3)) {
                            for each (var l14 in YP.unify(Result, new Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3])))) {
                              yield true;
                              return;
                            }
                          }
                        }
                      }
                    }
                    for each (var l10 in maplist_compileTerm(TermArgs, State, Args)) {
                      for each (var l11 in YP.unify(Result, new Functor2("new", Atom.a("Functor"), new ListPair(NameCode, new ListPair(new Functor1("objectArray", Args), Atom.NIL))))) {
                        yield true;
                        return;
                      }
                    }
                  }
                }
              }
              break cutIf3;
            }
            for each (var l6 in YP.unify(NameCode, new Functor1("object", Name))) {
              cutIf6:
              {
                for each (var l8 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
                  for each (var l9 in compileTerm(X1, State, Arg1)) {
                    for each (var l10 in YP.unify(Result, new Functor2("new", Atom.a("Functor1"), new ListPair(NameCode, new ListPair(Arg1, Atom.NIL))))) {
                      yield true;
                      return;
                    }
                  }
                  break cutIf6;
                }
                cutIf7:
                {
                  for each (var l9 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in compileTerm(X2, State, Arg2)) {
                        for each (var l12 in YP.unify(Result, new Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2])))) {
                          yield true;
                          return;
                        }
                      }
                    }
                    break cutIf7;
                  }
                  for each (var l9 in YP.unify(TermArgs, ListPair.make([X1, X2, X3]))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in compileTerm(X2, State, Arg2)) {
                        for each (var l12 in compileTerm(X3, State, Arg3)) {
                          for each (var l13 in YP.unify(Result, new Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3])))) {
                            yield true;
                            return;
                          }
                        }
                      }
                    }
                  }
                  for each (var l9 in maplist_compileTerm(TermArgs, State, Args)) {
                    for each (var l10 in YP.unify(Result, new Functor2("new", Atom.a("Functor"), new ListPair(NameCode, new ListPair(new Functor1("objectArray", Args), Atom.NIL))))) {
                      yield true;
                      return;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileAtomModule(Name, arg2, arg3, ModuleCode) {
  {
    var Arity = arg2;
    var State = arg3;
    if (CompilerState.nameArityHasModule(State, Name, Arity, Atom.a(""))) {
      for each (var l3 in YP.unify(ModuleCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Atom.a("")), Atom.NIL)))) {
        yield true;
        return;
      }
    }
  }
  {
    var _Arity = arg2;
    var _State = arg3;
    var Module = new Variable();
    for each (var l2 in Atom.module(Name, Module)) {
      if (YP.termNotEqual(Module, Atom.NIL)) {
        for each (var l4 in YP.unify(ModuleCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Module), Atom.NIL)))) {
          yield true;
          return;
        }
      }
    }
  }
}

function maplist_compileTerm(arg1, arg2, arg3) {
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var FirstResult = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(FirstResult, RestResults))) {
        if (YP.nonvar(Rest)) {
          for each (var l5 in compileTerm(First, State, FirstResult)) {
            for each (var l6 in maplist_compileTerm(Rest, State, RestResults)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
}

function compileExpression(Term, State, Result) {
  {
    var Name = new Variable();
    var TermArgs = new Variable();
    var X1 = new Variable();
    var FunctionName = new Variable();
    var Arg1 = new Variable();
    var x9 = new Variable();
    var X2 = new Variable();
    var Arg2 = new Variable();
    var x12 = new Variable();
    var Arity = new Variable();
    if (YP.nonvar(Term)) {
      for each (var l3 in YP.univ(Term, new ListPair(Name, TermArgs))) {
        if (YP.atom(Name)) {
          cutIf1:
          {
            for each (var l6 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
              for each (var l7 in unaryFunction(Name, FunctionName)) {
                for each (var l8 in compileExpression(X1, State, Arg1)) {
                  for each (var l9 in YP.unify(Result, new Functor2("call", FunctionName, new ListPair(Arg1, Atom.NIL)))) {
                    yield true;
                    return;
                  }
                }
                break cutIf1;
              }
            }
            cutIf2:
            {
              for each (var l7 in YP.unify(Term, new ListPair(x9, Atom.NIL))) {
                for each (var l8 in compileTerm(Term, State, Result)) {
                  yield true;
                  return;
                }
                break cutIf2;
              }
              cutIf3:
              {
                for each (var l8 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                  for each (var l9 in binaryFunction(Name, FunctionName)) {
                    for each (var l10 in compileExpression(X1, State, Arg1)) {
                      for each (var l11 in compileExpression(X2, State, Arg2)) {
                        for each (var l12 in YP.unify(Result, new Functor2("call", FunctionName, new ListPair(Arg1, new ListPair(Arg2, Atom.NIL))))) {
                          yield true;
                          return;
                        }
                      }
                    }
                    break cutIf3;
                  }
                }
                for each (var l8 in YP.functor(Term, x12, Arity)) {
                  YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Name, Arity)), Atom.a("Not an expression function")));
                  yield false;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    for each (var l2 in compileTerm(Term, State, Result)) {
      yield true;
      return;
    }
  }
}

function unaryFunction(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("-"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.negate"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abs"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.abs"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sign"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sign"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.toFloat"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("floor"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.floor"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("truncate"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.truncate"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("round"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.round"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("ceiling"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.ceiling"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sin"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sin"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cos"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.cos"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atan"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.atan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("exp"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.exp"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("log"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.log"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sqrt"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sqrt"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseComplement"))) {
        yield true;
        return;
      }
    }
  }
}

function binaryFunction(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("+"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.add"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("-"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.subtract"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("*"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.multiply"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("/"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.divide"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("//"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.intDivide"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("mod"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.mod"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("**"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.pow"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">>"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseShiftRight"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("<<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseShiftLeft"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("/\\"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseAnd"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\/"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseOr"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("min"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.min"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("max"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.max"))) {
        yield true;
        return;
      }
    }
  }
}

function convertFunctionCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("public class YPInnerClass {}"));
      YP.nl();
      YP.write(Atom.a("public static Type getDeclaringClass() { return typeof(YPInnerClass).DeclaringType; }"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var ReturnType = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    var Level = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [ReturnType, Name, ArgList, Body]))) {
      YP.write(Atom.a("public static "));
      YP.write(ReturnType);
      YP.write(Atom.a(" "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(Level, 1)) {
        convertStatementListCSharp(Body, Level);
        YP.write(Atom.a("}"));
        YP.nl();
        YP.nl();
        return;
      }
    }
  }
}

function convertStatementListCSharp(arg1, x1, x2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield true;
      return;
    }
  }
}

function convertStatementListCSharp(arg1, Level) {
  {
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NewStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      for each (var l3 in append(Body, new ListPair(new Functor1("label", Name), RestStatements), NewStatements)) {
        convertStatementListCSharp(NewStatements, Level);
        return;
      }
    }
  }
  {
    var Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", Type, Name, Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Type);
      YP.write(Atom.a(" "));
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield return true;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield return false;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield break;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return true;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return false;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("label", Name), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a(":"));
      YP.nl();
      cutIf1:
      {
        if (YP.termEqual(RestStatements, Atom.NIL)) {
          convertIndentationCSharp(Level);
          YP.write(Atom.a("{}"));
          YP.nl();
          convertStatementListCSharp(RestStatements, Level);
          return;
          break cutIf1;
        }
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("goto "));
      YP.write(Name);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      convertStatementListCSharp(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level);
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("if ("));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("foreach (bool l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("throw "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
}

function convertIndentationCSharp(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionCSharp(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListCSharp(Tail);
          return;
          break cutIf1;
        }
        convertArgListCSharp(Tail);
        return;
      }
    }
  }
}

function convertExpressionCSharp(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(Atom.a("object "));
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionCSharp(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Atom.a("new "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("null"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("!("));
      convertExpressionCSharp(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionCSharp(X);
      YP.write(Atom.a(") && ("));
      convertExpressionCSharp(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("new object[] {"));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a("}"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesCSharp(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesCSharp(RestCodes);
        return;
      }
    }
  }
}

function convertFunctionJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("function getDeclaringClass() { return null; }"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var x1 = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [x1, Name, ArgList, Body]))) {
      YP.write(Atom.a("function "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(") {"));
      YP.nl();
      convertStatementListJavascript(Body, 1);
      YP.write(Atom.a("}"));
      YP.nl();
      YP.nl();
      return;
    }
  }
}

function convertStatementListJavascript(arg1, arg2) {
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a(":"));
      YP.nl();
      convertIndentationJavascript(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var _Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", _Type, Name, Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("var "));
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("yield true;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("yield false;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return true;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return false;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("break "));
      YP.write(Name);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      convertStatementListJavascript(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("if ("));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("for each (var l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("throw "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
}

function convertIndentationJavascript(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionJavascript(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListJavascript(Tail);
          return;
          break cutIf1;
        }
        convertArgListJavascript(Tail);
        return;
      }
    }
  }
}

function convertExpressionJavascript(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionJavascript(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Atom.a("new "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("null"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("!("));
      convertExpressionJavascript(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionJavascript(X);
      YP.write(Atom.a(") && ("));
      convertExpressionJavascript(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("["));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a("]"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesJavascript(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesJavascript(RestCodes);
        return;
      }
    }
  }
}

function convertFunctionPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("def getDeclaringClass():"));
      YP.nl();
      YP.write(Atom.a("  return globals()"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var x1 = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    var Level = new Variable();
    var HasBreakableBlock = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [x1, Name, ArgList, Body]))) {
      YP.write(Atom.a("def "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a("):"));
      YP.nl();
      for each (var l3 in YP.unify(Level, 1)) {
        cutIf1:
        {
          if (hasBreakableBlockPython(Body)) {
            for each (var l6 in YP.unify(HasBreakableBlock, 1)) {
              cutIf2:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("doBreak = False"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                    YP.nl();
                    return;
                  }
                  break cutIf2;
                }
                for each (var l8 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                  YP.nl();
                  return;
                }
              }
            }
            break cutIf1;
          }
          for each (var l5 in YP.unify(HasBreakableBlock, 0)) {
            cutIf3:
            {
              if (YP.termEqual(HasBreakableBlock, 1)) {
                convertIndentationPython(Level);
                YP.write(Atom.a("doBreak = False"));
                YP.nl();
                for each (var l8 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                  YP.nl();
                  return;
                }
                break cutIf3;
              }
              for each (var l7 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                YP.nl();
                return;
              }
            }
          }
        }
      }
    }
  }
}

function hasBreakableBlockPython(arg1) {
  {
    var _Name = new Variable();
    var _Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", _Name, _Body), _RestStatements))) {
      return true;
    }
  }
  {
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var _Expression = new Variable();
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", _Expression, Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var _Expression = new Variable();
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", _Expression, Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var x1 = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, RestStatements))) {
      if (hasBreakableBlockPython(RestStatements)) {
        return true;
      }
    }
  }
  return false;
}

function convertStatementListPython(arg1, arg2, arg3) {
  {
    var x1 = arg2;
    var x2 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield true;
      return;
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = False"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        cutIf1:
        {
          if (YP.termEqual(Body, Atom.NIL)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("if "));
            YP.write(Name);
            YP.write(Atom.a(":"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("doBreak = False"));
            YP.nl();
            convertIndentationPython(Level);
            YP.write(Atom.a("if doBreak:"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("break"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
            break cutIf1;
          }
          convertIndentationPython(Level);
          YP.write(Atom.a("for _ in [1]:"));
          YP.nl();
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("if "));
            YP.write(Name);
            YP.write(Atom.a(":"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("doBreak = False"));
            YP.nl();
            convertIndentationPython(Level);
            YP.write(Atom.a("if doBreak:"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("break"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var _Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", _Type, Name, Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("yield True"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("yield False"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return True"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return False"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = True"));
      YP.nl();
      convertIndentationPython(Level);
      YP.write(Atom.a("doBreak = True"));
      YP.nl();
      convertIndentationPython(Level);
      YP.write(Atom.a("break"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      for each (var l3 in convertStatementListPython(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      cutIf2:
      {
        if (YP.termEqual(HasBreakableBlock, 1)) {
          if (YP.termNotEqual(Body, Atom.NIL)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("for _ in [1]:"));
            YP.nl();
            for each (var l6 in YP.unify(NextLevel, YP.add(Level, 1))) {
              for each (var l7 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
                cutIf3:
                {
                  if (YP.termEqual(HasBreakableBlock, 1)) {
                    cutIf4:
                    {
                      if (YP.greaterThan(Level, 1)) {
                        convertIndentationPython(Level);
                        YP.write(Atom.a("if doBreak:"));
                        YP.nl();
                        convertIndentationPython(NextLevel);
                        YP.write(Atom.a("break"));
                        YP.nl();
                        for each (var l12 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                          yield true;
                          return;
                        }
                        break cutIf4;
                      }
                      for each (var l11 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                        yield true;
                        return;
                      }
                    }
                    break cutIf3;
                  }
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                }
              }
            }
            break cutIf2;
          }
        }
        for each (var l4 in YP.unify(NextLevel, Level)) {
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            cutIf5:
            {
              if (YP.termEqual(HasBreakableBlock, 1)) {
                cutIf6:
                {
                  if (YP.greaterThan(Level, 1)) {
                    convertIndentationPython(Level);
                    YP.write(Atom.a("if doBreak:"));
                    YP.nl();
                    convertIndentationPython(NextLevel);
                    YP.write(Atom.a("break"));
                    YP.nl();
                    for each (var l10 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                      yield true;
                      return;
                    }
                    break cutIf6;
                  }
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                }
                break cutIf5;
              }
              for each (var l7 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("if "));
      convertExpressionPython(Expression);
      YP.write(Atom.a(":"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        cutIf7:
        {
          if (YP.termEqual(Body, Atom.NIL)) {
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("pass"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
            break cutIf7;
          }
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("for l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionPython(Expression);
      YP.write(Atom.a(":"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        for each (var l4 in YP.unify(NextLevel, YP.add(Level, 1))) {
          cutIf8:
          {
            if (YP.termEqual(Body, Atom.NIL)) {
              convertIndentationPython(NextLevel);
              YP.write(Atom.a("pass"));
              YP.nl();
              cutIf9:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("if doBreak:"));
                  YP.nl();
                  convertIndentationPython(NextLevel);
                  YP.write(Atom.a("break"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                  break cutIf9;
                }
                for each (var l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                  yield true;
                  return;
                }
              }
              break cutIf8;
            }
            for each (var l6 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
              cutIf10:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("if doBreak:"));
                  YP.nl();
                  convertIndentationPython(NextLevel);
                  YP.write(Atom.a("break"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                  break cutIf10;
                }
                for each (var l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("raise "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
}

function convertIndentationPython(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionPython(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListPython(Tail);
          return;
          break cutIf1;
        }
        convertArgListPython(Tail);
        return;
      }
    }
  }
}

function convertExpressionPython(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionPython(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("None"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("not ("));
      convertExpressionPython(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionPython(X);
      YP.write(Atom.a(") and ("));
      convertExpressionPython(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("["));
      convertArgListPython(ArgList);
      YP.write(Atom.a("]"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesPython(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesPython(RestCodes);
        return;
      }
    }
  }
}

function putCStringCode(Code) {
  {
    var HexDigit = new Variable();
    var HexChar = new Variable();
    if (YP.lessThanOrEqual(Code, 31)) {
      cutIf1:
      {
        if (YP.lessThanOrEqual(Code, 15)) {
          YP.write(Atom.a("\\x0"));
          for each (var l5 in YP.unify(HexDigit, Code)) {
            cutIf2:
            {
              if (YP.lessThanOrEqual(HexDigit, 9)) {
                for each (var l8 in YP.unify(HexChar, YP.add(HexDigit, 48))) {
                  YP.put_code(HexChar);
                  yield true;
                  return;
                }
                break cutIf2;
              }
              for each (var l7 in YP.unify(HexChar, YP.add(HexDigit, 55))) {
                YP.put_code(HexChar);
                yield true;
                return;
              }
            }
          }
          break cutIf1;
        }
        YP.write(Atom.a("\\x1"));
        for each (var l4 in YP.unify(HexDigit, YP.subtract(Code, 16))) {
          cutIf3:
          {
            if (YP.lessThanOrEqual(HexDigit, 9)) {
              for each (var l7 in YP.unify(HexChar, YP.add(HexDigit, 48))) {
                YP.put_code(HexChar);
                yield true;
                return;
              }
              break cutIf3;
            }
            for each (var l6 in YP.unify(HexChar, YP.add(HexDigit, 55))) {
              YP.put_code(HexChar);
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    if (YP.termEqual(Code, 34)) {
      YP.put_code(92);
      YP.put_code(34);
      yield true;
      return;
    }
  }
  {
    if (YP.termEqual(Code, 92)) {
      YP.put_code(92);
      YP.put_code(92);
      yield true;
      return;
    }
  }
  {
    YP.put_code(Code);
    yield true;
    return;
  }
}

function member(X, arg2) {
  {
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(X, x2))) {
      yield false;
    }
  }
  {
    var x2 = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(x2, Rest))) {
      for each (var l3 in member(X, Rest)) {
        yield false;
      }
    }
  }
}

function append(arg1, arg2, arg3) {
  {
    var List = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, List)) {
        for each (var l4 in YP.unify(arg3, List)) {
          yield false;
        }
      }
    }
  }
  {
    var List2 = arg2;
    var X = new Variable();
    var List1 = new Variable();
    var List12 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(X, List1))) {
      for each (var l3 in YP.unify(arg3, new ListPair(X, List12))) {
        for each (var l4 in append(List1, List2, List12)) {
          yield false;
        }
      }
    }
  }
}

