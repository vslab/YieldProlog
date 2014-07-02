function main() {
    YP.tell(new YP.HtmlDocumentWriter());
    var startTime = new Date().getTime();
    var nAnswers = 0;
    for each (var l1 in run_all_tests())
        nAnswers += 1;
    var finishTime = new Date().getTime();
    document.write("ISO Test Suite: " + 
		(finishTime - startTime) / 1000.0 + " seconds, " +
		nAnswers + " answers<br>");
}

// Following is the compiled code from YieldProlog/source/prolog/isoTestSuite.P

function getDeclaringClass() { return null; }

function run_all_tests() {
  {
    var F = new Variable();
    var Files = new Variable();
    var findallAnswers1 = new FindallAnswers(F);
    for each (var l2 in file(F)) {
      findallAnswers1.add();
    }
    for each (var l2 in findallAnswers1.result(Files)) {
      for each (var l3 in test_all(Files)) {
        for each (var l4 in write_results()) {
          yield true;
          return;
        }
      }
    }
  }
}

function test_all(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
  {
    var F = new Variable();
    var Fs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(F, Fs))) {
      for each (var l3 in run_tests(F)) {
        for each (var l4 in test_all(Fs)) {
          yield false;
        }
      }
    }
  }
}

function write_results() {
  {
    var F = new Variable();
    var ErrorBips = new Variable();
    var findallAnswers1 = new FindallAnswers(F);
    for each (var l2 in inerror(F)) {
      findallAnswers1.add();
    }
    for each (var l2 in findallAnswers1.result(ErrorBips)) {
      YP.write(Atom.a("--------------------"));
      YP.nl();
      cutIf2:
      {
        for each (var l4 in YP.unify(ErrorBips, Atom.NIL)) {
          YP.write(Atom.a("All bips passed -------------"));
          YP.nl();
          yield false;
          break cutIf2;
        }
        YP.nl();
        YP.write(Atom.a("The following BIPs gave unexpected answers:"));
        YP.nl();
        YP.write(Atom.a("The results should be examined carefully."));
        YP.nl();
        YP.nl();
        for each (var l4 in display_list(ErrorBips)) {
          yield false;
        }
      }
    }
  }
}

function result(G, Res) {
  {
    var Subs = new Variable();
    for each (var l2 in get_all_subs(G, Subs)) {
      for each (var l3 in special_ans_forms(Subs, Res)) {
        yield false;
      }
    }
  }
}

function special_ans_forms(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("success"), Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, Atom.a("success"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("failure"), Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, Atom.a("failure"))) {
        yield true;
        return;
      }
    }
  }
  {
    var Error = arg2;
    var E = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Error, Atom.NIL))) {
      for each (var l3 in YP.univ(Error, new ListPair(E, x3))) {
        for each (var l4 in error_type(E)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, X)) {
      for each (var l3 in YP.unify(arg2, X)) {
        yield false;
      }
    }
  }
}

function error_type(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("instantiation_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("type_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("domain_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("existence_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("permission_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("representation_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("evaluation_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("resource_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("syntax_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("system_error"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("unexpected_ball"))) {
      yield false;
    }
  }
}

function vars_in_term(T, V) {
  {
    for each (var l2 in vars_in_term3(T, Atom.NIL, V)) {
      yield false;
    }
  }
}

function vars_in_term3(arg1, VarsIn, arg3) {
  {
    var Term = arg1;
    var VarsOut = arg3;
    if (YP.atomic(Term)) {
      for each (var l3 in YP.unify(VarsOut, VarsIn)) {
        yield false;
      }
      return;
    }
  }
  {
    var Term = arg1;
    var VarsOut = arg3;
    if (YP.var(Term)) {
      cutIf1:
      {
        for each (var l4 in already_appears(Term, VarsIn)) {
          for each (var l5 in YP.unify(VarsOut, VarsIn)) {
            yield false;
          }
          break cutIf1;
        }
        for each (var l4 in append(VarsIn, new ListPair(Term, Atom.NIL), VarsOut)) {
          yield false;
        }
      }
      return;
    }
  }
  {
    var Vars = arg3;
    var A = new Variable();
    var B = new Variable();
    var V1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(A, B))) {
      for each (var l3 in vars_in_term3(A, VarsIn, V1)) {
        for each (var l4 in vars_in_term3(B, V1, Vars)) {
          yield false;
        }
      }
      return;
    }
  }
  {
    var T = arg1;
    var VarList = arg3;
    var _F = new Variable();
    var A = new Variable();
    var Args = new Variable();
    var Inter = new Variable();
    for each (var l2 in YP.univ(T, new ListPair(_F, new ListPair(A, Args)))) {
      for each (var l3 in vars_in_term3(A, VarsIn, Inter)) {
        for each (var l4 in vars_in_term3(Args, Inter, VarList)) {
          yield false;
        }
      }
    }
  }
}

function already_appears(Var, arg2) {
  {
    var V1 = new Variable();
    var _Vlist = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(V1, _Vlist))) {
      if (YP.termEqual(Var, V1)) {
        yield false;
      }
    }
  }
  {
    var _V1 = new Variable();
    var Vlist = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(_V1, Vlist))) {
      for each (var l3 in already_appears(Var, Vlist)) {
        yield false;
      }
    }
  }
}

function call_goal_get_subs(G, Sub) {
  {
    var GT = new Variable();
    var Vars = new Variable();
    var GVars = new Variable();
    for each (var l2 in YP.copy_term(G, GT)) {
      for each (var l3 in vars_in_term(G, Vars)) {
        for each (var l4 in vars_in_term(GT, GVars)) {
          for each (var l5 in YP.getIterator(GT, getDeclaringClass())) {
            for each (var l6 in make_subs_list1(Vars, GVars, Sub)) {
              yield false;
            }
          }
        }
      }
    }
  }
}

function make_subs_list1(arg1, arg2, arg3) {
  {
    var _V = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("success"))) {
      for each (var l3 in YP.unify(arg3, Atom.a("success"))) {
        yield false;
      }
    }
  }
  {
    var _V = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("failure"))) {
      for each (var l3 in YP.unify(arg3, Atom.a("failure"))) {
        yield false;
      }
    }
  }
  {
    var _V = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("impl_def"))) {
      for each (var l3 in YP.unify(arg3, Atom.a("impl_def"))) {
        yield false;
      }
    }
  }
  {
    var _V = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("undefined"))) {
      for each (var l3 in YP.unify(arg3, Atom.a("undefined"))) {
        yield false;
      }
    }
  }
  {
    var _V = arg1;
    var Error = new Variable();
    var E = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg2, Error)) {
      for each (var l3 in YP.unify(arg3, Error)) {
        for each (var l4 in YP.univ(Error, new ListPair(E, x4))) {
          for each (var l5 in error_type(E)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Vs = arg1;
    var GVs = arg2;
    var Sub = arg3;
    var S = new Variable();
    for each (var l2 in make_subs_list(Vs, GVs, S)) {
      for each (var l3 in compress_sub_list(Vs, S, Sub)) {
        yield false;
      }
    }
  }
}

function make_subs_list(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield false;
        }
      }
    }
  }
  {
    var Subs = arg3;
    var V = new Variable();
    var Rest = new Variable();
    var Ans = new Variable();
    var ARest = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(V, Rest))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Ans, ARest))) {
        if (YP.termEqual(V, Ans)) {
          for each (var l5 in make_subs_list(Rest, ARest, Subs)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var V = new Variable();
    var Rest = new Variable();
    var Ans = new Variable();
    var ARest = new Variable();
    var SubsRest = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(V, Rest))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Ans, ARest))) {
        for each (var l4 in YP.unify(arg3, new ListPair(new Functor2("<--", V, Ans), SubsRest))) {
          for each (var l5 in make_subs_list(Rest, ARest, SubsRest)) {
            yield false;
          }
        }
      }
    }
  }
}

function list_make_subs_list(arg1, arg2, arg3) {
  {
    var x1 = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("failure"), Atom.NIL))) {
        yield true;
        return;
      }
    }
  }
  {
    var V = arg1;
    var GTV = arg2;
    var S = arg3;
    for each (var l2 in list_make_subs_list_aux(V, GTV, S)) {
      yield false;
    }
  }
}

function list_make_subs_list_aux(arg1, arg2, arg3) {
  {
    var _Vars = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Vars = arg1;
    var GV1 = new Variable();
    var GVRest = new Variable();
    var Sub1 = new Variable();
    var SubRest = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(GV1, GVRest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Sub1, SubRest))) {
        for each (var l4 in make_subs_list1(Vars, GV1, Sub1)) {
          for each (var l5 in list_make_subs_list_aux(Vars, GVRest, SubRest)) {
            yield false;
          }
        }
      }
    }
  }
}

function call_with_result(arg1, arg2) {
  {
    var G = arg1;
    var R = arg2;
    var Sub = new Variable();
    for each (var l2 in call_goal_get_subs(G, Sub)) {
      cutIf1:
      {
        for each (var l4 in YP.unify(Sub, Atom.NIL)) {
          for each (var l5 in YP.unify(R, Atom.a("success"))) {
            yield false;
          }
          break cutIf1;
        }
        for each (var l4 in YP.unify(R, Sub)) {
          yield false;
        }
      }
    }
  }
  {
    var _G = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("failure"))) {
      yield false;
    }
  }
}

function protected_call_results(G, R) {
  {
    var B = new Variable();
    var catchGoal1 = new YP.Catch(new Functor2(Atom.a("call_with_result", Atom.a("")), G, R), getDeclaringClass());
    for each (var l2 in catchGoal1) {
      yield false;
    }
    for each (var l2 in catchGoal1.unifyExceptionOrThrow(B)) {
      for each (var l3 in YP.unify(R, B)) {
        yield false;
      }
    }
  }
}

function get_all_subs(G, AllSubs) {
  {
    var GT = new Variable();
    var GVars = new Variable();
    var GTAns = new Variable();
    var GTAnsList = new Variable();
    for each (var l2 in YP.copy_term(G, GT)) {
      for each (var l3 in vars_in_term(G, GVars)) {
        var findallAnswers1 = new FindallAnswers(GTAns);
        for each (var l4 in protect_call_result(GT, GTAns)) {
          findallAnswers1.add();
        }
        for each (var l4 in findallAnswers1.result(GTAnsList)) {
          for each (var l5 in list_make_subs_list(GVars, GTAnsList, AllSubs)) {
            yield false;
          }
        }
      }
    }
  }
}

function call_result(G, R) {
  {
    var GVars = new Variable();
    for each (var l2 in vars_in_term(G, GVars)) {
      for each (var l3 in YP.getIterator(G, getDeclaringClass())) {
        for each (var l4 in YP.unify(R, GVars)) {
          yield false;
        }
      }
    }
  }
}

function protect_call_result(G, R) {
  {
    var B = new Variable();
    var catchGoal1 = new YP.Catch(new Functor2(Atom.a("call_result", Atom.a("")), G, R), getDeclaringClass());
    for each (var l2 in catchGoal1) {
      yield false;
    }
    for each (var l2 in catchGoal1.unifyExceptionOrThrow(B)) {
      for each (var l3 in extract_error(B, R)) {
        yield false;
      }
    }
  }
}

function extract_error(arg1, arg2) {
  {
    var R = arg2;
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("error", R, x2))) {
      yield true;
      return;
    }
  }
  {
    var B = arg1;
    for each (var l2 in YP.unify(arg2, new Functor1("unexpected_ball", B))) {
      yield false;
    }
  }
}

function compress_sub_list(arg1, arg2, arg3) {
  {
    var x1 = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.a("success"))) {
        yield true;
        return;
      }
    }
  }
  {
    var Vars = arg1;
    var X = new Variable();
    var A = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(new Functor2("<--", X, A), Atom.NIL))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("<--", X, A), Atom.NIL))) {
        if (YP.termNotEqual(X, A)) {
          for each (var l5 in in_vars(A, Vars)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Vars = arg1;
    var LIn = arg2;
    var LOut = arg3;
    var X = new Variable();
    var A = new Variable();
    var Before = new Variable();
    var After = new Variable();
    var BN = new Variable();
    var AN = new Variable();
    var L1 = new Variable();
    for each (var l2 in split_list(new Functor2("<--", X, A), Before, After, LIn)) {
      if (YP.var(A)) {
        for each (var l4 in sub(new Functor2("<--", X, A), Before, BN)) {
          for each (var l5 in sub(new Functor2("<--", X, A), After, AN)) {
            for each (var l6 in append(BN, AN, L1)) {
              for each (var l7 in compress_sub_list(Vars, L1, LOut)) {
                yield false;
              }
            }
          }
        }
        return;
      }
    }
  }
  {
    var x1 = arg1;
    var L = new Variable();
    for each (var l2 in YP.unify(arg2, L)) {
      for each (var l3 in YP.unify(arg3, L)) {
        yield false;
      }
    }
  }
}

function in_vars(V, arg2) {
  {
    var V1 = new Variable();
    var _Vs = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(V1, _Vs))) {
      if (YP.termEqual(V, V1)) {
        yield true;
        return;
      }
    }
  }
  {
    var _V1 = new Variable();
    var Vs = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(_V1, Vs))) {
      for each (var l3 in in_vars(V, Vs)) {
        yield false;
      }
    }
  }
}

function sub(arg1, arg2, arg3) {
  {
    var _X = new Variable();
    var _A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", _X, _A))) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield false;
        }
      }
    }
  }
  {
    var X = new Variable();
    var A = new Variable();
    var H = new Variable();
    var T = new Variable();
    var H1 = new Variable();
    var T1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", X, A))) {
      for each (var l3 in YP.unify(arg2, new ListPair(H, T))) {
        for each (var l4 in YP.unify(arg3, new ListPair(H1, T1))) {
          for each (var l5 in sub1(new Functor2("<--", X, A), H, H1)) {
            for each (var l6 in sub(new Functor2("<--", X, A), T, T1)) {
              yield false;
            }
          }
        }
      }
    }
  }
}

function sub1(arg1, arg2, arg3) {
  {
    var X = new Variable();
    var A = new Variable();
    var Y = new Variable();
    var Old = new Variable();
    var New = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", X, A))) {
      for each (var l3 in YP.unify(arg2, new Functor2("<--", Y, Old))) {
        for each (var l4 in YP.unify(arg3, new Functor2("<--", Y, New))) {
          for each (var l5 in exp_sub(new Functor2("<--", X, A), Old, New)) {
            yield false;
          }
        }
      }
    }
  }
}

function exp_sub(arg1, B, New) {
  {
    var X = new Variable();
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", X, A))) {
      if (YP.var(B)) {
        if (YP.termEqual(B, A)) {
          for each (var l5 in YP.unify(New, X)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var _X = new Variable();
    var _A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", _X, _A))) {
      if (YP.var(B)) {
        for each (var l4 in YP.unify(New, B)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var _X = new Variable();
    var _A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", _X, _A))) {
      if (YP.atomic(B)) {
        for each (var l4 in YP.unify(New, B)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var X = new Variable();
    var A = new Variable();
    var x5 = new Variable();
    var x6 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", X, A))) {
      for each (var l3 in YP.unify(B, new ListPair(x5, x6))) {
        for each (var l4 in list_exp_sub(new Functor2("<--", X, A), B, New)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var X = new Variable();
    var A = new Variable();
    var F = new Variable();
    var L = new Variable();
    var L1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", X, A))) {
      for each (var l3 in YP.univ(B, new ListPair(F, L))) {
        for each (var l4 in list_exp_sub(new Functor2("<--", X, A), L, L1)) {
          for each (var l5 in YP.univ(New, new ListPair(F, L1))) {
            yield false;
          }
        }
      }
    }
  }
}

function list_exp_sub(arg1, arg2, arg3) {
  {
    var _S = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var S = arg1;
    var E = new Variable();
    var ER = new Variable();
    var EN = new Variable();
    var ERN = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(E, ER))) {
      for each (var l3 in YP.unify(arg3, new ListPair(EN, ERN))) {
        for each (var l4 in exp_sub(S, E, EN)) {
          for each (var l5 in list_exp_sub(S, ER, ERN)) {
            yield false;
          }
        }
      }
    }
  }
}

function split_list(Element, Before, After, List) {
  {
    for each (var l2 in append(Before, new ListPair(Element, After), List)) {
      yield false;
    }
  }
}

function compare_subst_lists(arg1, arg2, arg3, arg4) {
  {
    var F = arg1;
    var S = arg2;
    var x3 = new Variable();
    var x4 = new Variable();
    var x5 = new Variable();
    var x6 = new Variable();
    for each (var l2 in YP.unify(arg3, Atom.NIL)) {
      for each (var l3 in YP.unify(arg4, Atom.NIL)) {
        cutIf1:
        {
          for each (var l5 in YP.unify(F, new ListPair(x3, x4))) {
            break cutIf1;
          }
          cutIf2:
          {
            for each (var l6 in YP.unify(S, new ListPair(x5, x6))) {
              break cutIf2;
            }
            for each (var l6 in YP.unify(F, S)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var F = new Variable();
    var S = new Variable();
    var x3 = new Variable();
    var x4 = new Variable();
    var x5 = new Variable();
    var x6 = new Variable();
    for each (var l2 in YP.unify(arg1, F)) {
      for each (var l3 in YP.unify(arg2, S)) {
        for each (var l4 in YP.unify(arg3, F)) {
          for each (var l5 in YP.unify(arg4, S)) {
            cutIf3:
            {
              for each (var l7 in YP.unify(F, new ListPair(x3, x4))) {
                break cutIf3;
              }
              cutIf4:
              {
                for each (var l8 in YP.unify(S, new ListPair(x5, x6))) {
                  break cutIf4;
                }
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
    var F = arg1;
    var S = arg2;
    var FNS = arg3;
    var SNF = arg4;
    var x5 = new Variable();
    var x6 = new Variable();
    cutIf5:
    {
      for each (var l3 in YP.unify(F, new ListPair(x5, x6))) {
        break cutIf5;
      }
      for each (var l3 in del_item(F, S, SNF)) {
        cutIf6:
        {
          for each (var l5 in member(F, S)) {
            for each (var l6 in YP.unify(FNS, Atom.NIL)) {
              yield false;
            }
            break cutIf6;
          }
          for each (var l5 in YP.unify(FNS, F)) {
            yield false;
          }
        }
      }
      return;
    }
  }
  {
    var F = arg1;
    var S = arg2;
    var FNS = arg3;
    var SNF = arg4;
    var x5 = new Variable();
    var x6 = new Variable();
    cutIf7:
    {
      for each (var l3 in YP.unify(S, new ListPair(x5, x6))) {
        break cutIf7;
      }
      for each (var l3 in del_item(S, F, FNS)) {
        cutIf8:
        {
          for each (var l5 in member(S, F)) {
            for each (var l6 in YP.unify(SNF, Atom.NIL)) {
              yield false;
            }
            break cutIf8;
          }
          for each (var l5 in YP.unify(SNF, S)) {
            yield false;
          }
        }
      }
      return;
    }
  }
  {
    var F = arg1;
    var S = arg2;
    var F1 = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg3, Atom.NIL)) {
      for each (var l3 in YP.unify(arg4, Atom.NIL)) {
        for each (var l4 in YP.unify(F, new ListPair(F1, Atom.NIL))) {
          for each (var l5 in YP.unify(S, new ListPair(S1, Atom.NIL))) {
            for each (var l6 in same_subst(F1, S1)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var F = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, F)) {
      for each (var l3 in YP.unify(arg2, S)) {
        for each (var l4 in YP.unify(arg3, F)) {
          for each (var l5 in YP.unify(arg4, S)) {
            for each (var l6 in length(F, 1)) {
              for each (var l7 in length(S, 1)) {
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
    var F = arg1;
    var S = arg2;
    var FNS = arg3;
    var SNF = arg4;
    for each (var l2 in length(F, 1)) {
      for each (var l3 in del_item(F, S, SNF)) {
        cutIf9:
        {
          for each (var l5 in member(F, S)) {
            for each (var l6 in YP.unify(FNS, Atom.NIL)) {
              yield false;
            }
            break cutIf9;
          }
          for each (var l5 in YP.unify(FNS, F)) {
            yield false;
          }
        }
      }
      return;
    }
  }
  {
    var F = arg1;
    var S = arg2;
    var FNS = arg3;
    var SNF = arg4;
    for each (var l2 in length(S, 1)) {
      for each (var l3 in del_item(S, F, FNS)) {
        cutIf10:
        {
          for each (var l5 in member(S, F)) {
            for each (var l6 in YP.unify(SNF, Atom.NIL)) {
              yield false;
            }
            break cutIf10;
          }
          for each (var l5 in YP.unify(SNF, S)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var F = arg1;
    var S = arg2;
    var FNS = arg3;
    var SNF = arg4;
    for each (var l2 in list_del_item(F, S, SNF)) {
      for each (var l3 in list_del_item(S, F, FNS)) {
        yield false;
      }
    }
  }
}

function list_del_item(arg1, arg2, arg3) {
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, L)) {
        for each (var l4 in YP.unify(arg3, L)) {
          yield false;
        }
      }
    }
  }
  {
    var L1 = arg2;
    var Left = arg3;
    var It = new Variable();
    var R = new Variable();
    var LInter = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(It, R))) {
      for each (var l3 in del_item(It, L1, LInter)) {
        for each (var l4 in list_del_item(R, LInter, Left)) {
          yield false;
        }
      }
    }
  }
}

function del_item(arg1, arg2, arg3) {
  {
    var _Item = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Item = arg1;
    var R = arg3;
    var It = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(It, R))) {
      for each (var l3 in same_subst(Item, It)) {
        yield true;
        return;
      }
    }
  }
  {
    var Item = arg1;
    var It = new Variable();
    var Rest = new Variable();
    var R = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(It, Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(It, R))) {
        for each (var l4 in del_item(Item, Rest, R)) {
          yield false;
        }
      }
    }
  }
}

function same_subst(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Subs = arg2;
    var S1 = new Variable();
    var SRest = new Variable();
    var Subs1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(S1, SRest))) {
      for each (var l3 in delmemb(S1, Subs, Subs1)) {
        for each (var l4 in same_subst(SRest, Subs1)) {
          yield false;
        }
      }
    }
  }
}

function delmemb(arg1, arg2, arg3) {
  {
    var _E = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var R = arg3;
    var E = new Variable();
    var E1 = new Variable();
    var F = new Variable();
    var F1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("<--", E, E1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<--", F, F1), R))) {
        if (YP.termEqual(E, F)) {
          for each (var l5 in YP.copy_term(new Functor2("<--", E, E1), new Functor2("<--", F, F1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var E = arg1;
    var F = new Variable();
    var R = new Variable();
    var R1 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(F, R))) {
      for each (var l3 in YP.unify(arg3, new ListPair(F, R1))) {
        for each (var l4 in delmemb(E, R, R1)) {
          yield false;
        }
      }
    }
  }
}

function run_tests(File) {
  {
    var S = new Variable();
    YP.asserta(new Functor3("score", File, new Functor1("total", 0), new Functor1("wrong", 0)), getDeclaringClass());
    for each (var l2 in open(File, Atom.a("read"), S)) {
      for each (var l3 in loop_through(File, S)) {
        for each (var l4 in close(S)) {
          yield false;
        }
      }
    }
  }
}

function loop_through(arg1, _S) {
  {
    var F = arg1;
    var X = new Variable();
    for each (var l2 in fileContents(F, X)) {
      for each (var l3 in reset_flags()) {
        for each (var l4 in test(F, X)) {
        }
      }
    }
  }
  {
    var _F = arg1;
    yield false;
  }
}

function test(arg1, arg2) {
  {
    var x1 = arg1;
    for each (var l2 in YP.unify(arg2, Atom.a("end_of_file"))) {
      yield false;
    }
  }
  {
    var F = arg1;
    var R = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg2, new Functor2("error", R, x3))) {
      YP.write(Atom.a("Error in Input: "));
      YP.write(R);
      YP.nl();
      YP.nl();
      for each (var l3 in update_score(F, Atom.a("non_null"), Atom.a("non_null"))) {
        yield false;
      }
      return;
    }
  }
  {
    var F = arg1;
    var G = new Variable();
    var Expected = new Variable();
    var R = new Variable();
    var Extra = new Variable();
    var Missing = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(G, new ListPair(Expected, Atom.NIL)))) {
      for each (var l3 in result(G, R)) {
        for each (var l4 in compare_subst_lists(R, Expected, Extra, Missing)) {
          for each (var l5 in write_if_wrong(F, G, Expected, Extra, Missing)) {
            for each (var l6 in update_score(F, Missing, Extra)) {
              yield false;
            }
          }
        }
      }
    }
  }
}

function write_if_wrong(arg1, arg2, arg3, arg4, arg5) {
  {
    var x1 = arg1;
    var x2 = arg2;
    var x3 = arg3;
    for each (var l2 in YP.unify(arg4, Atom.NIL)) {
      for each (var l3 in YP.unify(arg5, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var F = arg1;
    var G = arg2;
    var Expected = arg3;
    var Extra = arg4;
    var Missing = arg5;
    var x6 = new Variable();
    for each (var l2 in fake_numbervars(ListPair.make([G, Expected, Missing]), 0, x6)) {
      YP.write(Atom.a("In file: "));
      YP.write(F);
      YP.nl();
      YP.write(Atom.a("possible error in Goal: "));
      YP.write(G);
      YP.nl();
      YP.write(Atom.a("Expected: "));
      YP.write(Expected);
      YP.nl();
      YP.write(Atom.a("Extra Solutions found: "));
      YP.write(Extra);
      YP.nl();
      YP.write(Atom.a("Solutions Missing: "));
      YP.write(Missing);
      YP.nl();
      YP.nl();
      yield false;
    }
  }
}

function update_score(F, arg2, arg3) {
  {
    var T = new Variable();
    var W = new Variable();
    var T1 = new Variable();
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        for each (var l4 in YP.retract(new Functor3("score", F, new Functor1("total", T), new Functor1("wrong", W)))) {
          for each (var l5 in YP.unify(T1, YP.add(T, 1))) {
            YP.asserta(new Functor3("score", F, new Functor1("total", T1), new Functor1("wrong", W)), getDeclaringClass());
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var x2 = arg2;
    var x3 = arg3;
    var T = new Variable();
    var W = new Variable();
    var T1 = new Variable();
    var W1 = new Variable();
    for each (var l2 in YP.retract(new Functor3("score", F, new Functor1("total", T), new Functor1("wrong", W)))) {
      for each (var l3 in YP.unify(T1, YP.add(T, 1))) {
        for each (var l4 in YP.unify(W1, YP.add(W, 1))) {
          YP.asserta(new Functor3("score", F, new Functor1("total", T1), new Functor1("wrong", W1)), getDeclaringClass());
          yield false;
        }
      }
    }
  }
}

function inerror(F) {
  {
    var _X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.matchDynamic(Atom.a("score"), [F, new Functor1("total", _X), new Functor1("wrong", Y)])) {
      if (YP.notEqual(Y, 0)) {
        yield false;
      }
    }
  }
}

function file(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("fail"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("catch-and-throw"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_input"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_output"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cut"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("repeat"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("retract"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      yield false;
    }
  }
}

function display_list(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      YP.nl();
      yield false;
    }
  }
  {
    var H = new Variable();
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      YP.write(H);
      YP.nl();
      for each (var l3 in display_list(T)) {
        yield false;
      }
    }
  }
}

function reset_flags() {
  {
    YP.set_prolog_flag(Atom.a("unknown"), Atom.a("error"));
    yield false;
  }
}

function exists(arg1) {
  {
    var P = new Variable();
    var I = new Variable();
    var List = new Variable();
    var G = new Variable();
    var x5 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", P, I))) {
      for each (var l3 in make_list(I, List)) {
        for each (var l4 in YP.univ(G, new ListPair(P, List))) {
          YP.set_prolog_flag(Atom.a("unknown"), Atom.a("fail"));
          var catchGoal1 = new YP.Catch(new Functor1("call", G), getDeclaringClass());
          for each (var l5 in catchGoal1) {
            for each (var l6 in reset_flags()) {
              yield true;
              return;
            }
          }
          for each (var l5 in catchGoal1.unifyExceptionOrThrow(x5)) {
            for each (var l6 in reset_flags()) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var P = new Variable();
    var I = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", P, I))) {
      YP.write(Atom.a("Predicate: "));
      YP.write(new Functor2("/", P, I));
      YP.write(Atom.a(" not implemented"));
      YP.nl();
      for each (var l3 in reset_flags()) {
        yield false;
      }
    }
  }
}

function make_list(N, L) {
  {
    if (YP.greaterThanOrEqual(N, 0)) {
      for each (var l3 in make_list1(N, L)) {
        yield false;
      }
    }
  }
}

function make_list1(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, 0)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var N = arg1;
    var x2 = new Variable();
    var L1 = new Variable();
    var N1 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(x2, L1))) {
      for each (var l3 in YP.unify(N1, YP.subtract(N, 1))) {
        for each (var l4 in make_list(N1, L1)) {
          yield false;
        }
      }
    }
  }
}

function fake_numbervars(arg1, arg2, arg3) {
  {
    var X = arg1;
    var N = arg2;
    var M = arg3;
    if (YP.var(X)) {
      for each (var l3 in YP.univ(X, new ListPair(Atom.a("$VAR"), new ListPair(N, Atom.NIL)))) {
        for each (var l4 in YP.unify(M, YP.add(N, 1))) {
          yield false;
        }
      }
      return;
    }
  }
  {
    var X = arg1;
    var N = new Variable();
    for each (var l2 in YP.unify(arg2, N)) {
      for each (var l3 in YP.unify(arg3, N)) {
        if (YP.atomic(X)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var N = arg2;
    var M = arg3;
    var H = new Variable();
    var T = new Variable();
    var N1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      for each (var l3 in fake_numbervars(H, N, N1)) {
        for each (var l4 in fake_numbervars(T, N1, M)) {
          yield false;
        }
      }
      return;
    }
  }
  {
    var T = arg1;
    var N = arg2;
    var M = arg3;
    var _F = new Variable();
    var Args = new Variable();
    for each (var l2 in YP.univ(T, new ListPair(_F, Args))) {
      for each (var l3 in fake_numbervars(Args, N, M)) {
        yield false;
      }
    }
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

function length(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, 0)) {
        yield true;
        return;
      }
    }
  }
  {
    var Length = arg2;
    var x1 = new Variable();
    var Rest = new Variable();
    var RestLength = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, Rest))) {
      for each (var l3 in length(Rest, RestLength)) {
        for each (var l4 in YP.unify(Length, YP.add(RestLength, 1))) {
          yield false;
        }
      }
    }
  }
}

function open(x1, x2, x3) {
  {
    yield false;
  }
}

function close(x1) {
  {
    yield false;
  }
}

function intAndFloatAreDifferent() {
  {
    if (YP.termNotEqual(1.0, 1)) {
      yield false;
    }
  }
}

function fileContents(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("abolish"), 1)), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("abolish"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("foo"), Atom.a("a"))), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("foo"), -1)), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("current_prolog_flag", Atom.a("max_arity"), A), new Functor2(",", new Functor2("is", X, new Functor2("+", A, 1)), new Functor1("abolish", new Functor2("/", Atom.a("foo"), X)))), new ListPair(new Functor1("representation_error", Atom.a("max_arity")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", 5, 2)), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, 1), new Functor1("var", X)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("var", X), new Functor2("=", X, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", Atom.a("fail"), new Functor1("call", 3)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("nofoo", X), new Functor1("call", X)), new ListPair(new Functor2("existence_error", Atom.a("procedure"), new Functor2("/", Atom.a("nofoo"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("and"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, Atom.a("true")), new Functor1("call", X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("true")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", X, Atom.a("b")), Atom.a("a")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 2, new Functor3("foo", Atom.a("a"), new Functor2("f", X, Atom.a("b")), Atom.a("c")), new Functor2("f", Atom.a("a"), Y)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), new ListPair(new Functor2("<--", Y, Atom.a("b")), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor3("arg", 1, new Functor2("foo", X, Atom.a("b")), Y), new Functor2("=", X, Atom.a("a"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", Y, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("b")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("foo")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 3, new Functor2("foo", 3, 4), N), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", X, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, X, Atom.a("a")), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, Atom.a("atom"), A), new ListPair(new Functor2("type_error", Atom.a("compound"), Atom.a("atom")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, 3, A), new ListPair(new Functor2("type_error", Atom.a("compound"), 3), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", -3, new Functor2("foo", Atom.a("a"), Atom.a("b")), A), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -3), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("arg", Atom.a("a"), new Functor2("foo", Atom.a("a"), Atom.a("b")), X), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", 0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", N, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", new Functor1("floot", 1), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", N, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", new Functor1("floot", 1), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 0.333, new Functor2("/", 1, 3)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">=", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">=", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">=", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(">=", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<", 0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("<", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=<", 0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=<", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=<", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arith_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=<", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("asserta", new Functor2(":-", new Functor1("bar", X), X)), new Functor2("clause", new Functor1("bar", X), B)), new ListPair(new ListPair(new ListPair(new Functor2("<--", B, new Functor1("call", X)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", x1), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", 4), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", new Functor2(":-", Atom.a("foo"), 4)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("asserta"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", new Functor2(":-", new Functor1("atom", x1), Atom.a("true"))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", new Functor1("foo", X), new Functor2("->", X, new Functor1("call", X)))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", x1), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", 4), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", Atom.a("foo"), 4)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("assertz"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", new Functor1("atom", x1), Atom.a("true"))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.a("string")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Var = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Var), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.NIL), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", 6), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atom", 3.3), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a(""), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.NIL, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("["), new ListPair(Atom.a("]"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("'"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("'"), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("iso"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make([Atom.a("i"), Atom.a("s"), Atom.a("o")])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, ListPair.make([Atom.a("p"), Atom.a("r"), Atom.a("o"), Atom.a("l"), Atom.a("o"), Atom.a("g")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("North"), new ListPair(Atom.a("N"), X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make([Atom.a("o"), Atom.a("r"), Atom.a("t"), Atom.a("h")])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("iso"), new ListPair(Atom.a("i"), new ListPair(Atom.a("s"), Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var E = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, ListPair.make([Atom.a("a"), E, Atom.a("c")])), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, new ListPair(Atom.a("a"), new ListPair(Atom.a("b"), L))), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", new Functor1("f", Atom.a("a")), L), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, Atom.a("iso")), new ListPair(new Functor2("type_error", Atom.a("list"), Atom.a("iso")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, new ListPair(Atom.a("a"), new ListPair(new Functor1("f", Atom.a("b")), Atom.NIL))), new ListPair(new Functor2("type_error", Atom.a("character"), new Functor1("f", Atom.a("b"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("atom_chars", X, new ListPair(Atom.a("1"), new ListPair(Atom.a("2"), Atom.NIL))), new Functor2("is", Y, new Functor2("+", X, 1))), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("12"), 0)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a(""), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.NIL, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(91, new ListPair(93, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("'"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(39, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("iso"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make([105, 115, 111])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, ListPair.make([112, 114, 111, 108, 111, 103])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("North"), new ListPair(78, L)), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make([111, 114, 116, 104])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("iso"), new ListPair(105, new ListPair(115, Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", new Functor1("f", Atom.a("a")), L), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, 120), new ListPair(new Functor2("type_error", Atom.a("list"), 120), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, ListPair.make([105, 115, Atom.a("o")])), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), A), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("hello world")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", T, Atom.a(" world"), Atom.a("small world")), new ListPair(new ListPair(new ListPair(new Functor2("<--", T, Atom.a("small")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), Atom.a("small world")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var T1 = new Variable();
    var T2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", T1, T2, Atom.a("hello")), new ListPair(ListPair.make([new ListPair(new Functor2("<--", T1, Atom.a("")), new ListPair(new Functor2("<--", T2, Atom.a("hello")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("h")), new ListPair(new Functor2("<--", T2, Atom.a("ello")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("he")), new ListPair(new Functor2("<--", T2, Atom.a("llo")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hel")), new ListPair(new Functor2("<--", T2, Atom.a("lo")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hell")), new ListPair(new Functor2("<--", T2, Atom.a("o")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hello")), new ListPair(new Functor2("<--", T2, Atom.a("")), Atom.NIL))]), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A1 = new Variable();
    var A3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", A1, Atom.a("iso"), A3), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A2 = new Variable();
    var A3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("iso"), A2, A3), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", new Functor1("f", Atom.a("a")), Atom.a("iso"), A3), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("iso"), new Functor1("f", Atom.a("a")), A3), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A1 = new Variable();
    var A2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", A1, A2, new Functor1("f", Atom.a("a"))), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("enchanted evening"), N), new ListPair(new ListPair(new ListPair(new Functor2("<--", N, 17), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a(""), N), new ListPair(new ListPair(new ListPair(new Functor2("<--", N, 0), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("scarlet"), 5), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Atom_1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom_1, 4), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", 1.23, 4), new ListPair(new Functor2("type_error", Atom.a("atom"), 1.23), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("atom"), Atom.a("4")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("4")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Var = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Var), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Atom.NIL), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", 6), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", 3.3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var Z = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2("=", X, Z)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, Atom.a("fail"), L), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Y = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", 1, new Functor2(";", new Functor2("=", Y, 1), new Functor2("=", Y, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var L = new Variable();
    var x4 = new Variable();
    var x5 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", new Functor2("f", X, Y), new Functor2(";", new Functor2("=", X, Atom.a("a")), new Functor2("=", Y, Atom.a("b"))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(new Functor2("f", Atom.a("a"), x4), new ListPair(new Functor2("f", x5, Atom.a("b")), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, new Functor2(";", new Functor2(",", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, new Functor2(";", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make([1, x4, 2])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("bagof", X, new Functor2(";", new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1))), new Functor2("=", X, 3)), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var Z = new Variable();
    var L = new Variable();
    var x5 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2(";", new Functor2("=", X, Z), new Functor2("=", Y, 1))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(x5, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var Z = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, Z), L), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, 1, L), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("bagof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor1("call", 4), S), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", Atom.a("!")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), X)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), new Functor1("call", 1))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), X)), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), new Functor1("call", 1))), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", 1), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), 1)), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(",", Atom.a("fail"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), 1)), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(",", new Functor1("write", 3), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("call"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(";", 1, Atom.a("true"))), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(";", 1, Atom.a("true"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("catch-and-throw"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor3("catch", Atom.a("true"), C, new Functor1("write", Atom.a("something"))), new Functor1("throw", Atom.a("blabla"))), new ListPair(Atom.a("system_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("catch-and-throw"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("catch", new Functor2("number_chars", A, L), new Functor2("error", Atom.a("instantiation_error"), x3), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Code = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("a"), Code), new ListPair(new ListPair(new ListPair(new Functor2("<--", Code, 97), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Char = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, 99), new ListPair(new ListPair(new ListPair(new Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Char = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, 99), new ListPair(new ListPair(new ListPair(new Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("b"), 98), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("b"), 4), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Code = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("ab"), Code), new ListPair(new Functor2("type_error", Atom.a("character"), Atom.a("ab")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("a"), Atom.a("x")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("x")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Char = new Variable();
    var Code = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, Code), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Char = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, -2), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Body = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("clause", Atom.a("x"), Body), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("clause", x1, B), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("clause", 4, B), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("clause", new Functor1("f", x1), 5), new ListPair(new Functor2("type_error", Atom.a("callable"), 5), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var Body = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("clause", new Functor1("atom", x1), Body), new ListPair(new Functor3("permission_error", Atom.a("access"), Atom.a("private_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", 33.3), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", -33.3), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new Functor1("-", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", x1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", Atom.a("a")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new ListPair(Atom.a("a"), Atom.NIL)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", X, Y), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", X, 3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", x1, Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", x1, x2), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("copy_term", new Functor2("+", new Functor2("+", X, X), Y), new Functor2("+", new Functor2("+", A, B), B)), new Functor2("=", A, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", B, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", Atom.a("a"), Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", Atom.a("a"), Atom.a("b")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", new Functor1("f", Atom.a("a")), new Functor1("f", X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b"))), new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b")))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_input"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1(Atom.a("exists", Atom.a("")), new Functor2("/", Atom.a("current_input"), 1)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_output"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1(Atom.a("exists", Atom.a("")), new Functor2("/", Atom.a("current_output"), 1)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", Atom.a("current_predicate"), 1)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", Atom.a("score"), 3)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Name = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, ListPair.make([new Functor3("functor", new Functor1(Atom.a("run_tests", Atom.a("")), 1), Name, x2), new Functor1("current_predicate", new Functor2("/", Name, 1)), Atom.a("success")]))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", 4), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", Atom.a("dog")), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), Atom.a("dog")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_predicate"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", 0, Atom.a("dog"))), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), new Functor2("/", 0, Atom.a("dog"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("debug"), Atom.a("off")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("warning"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("error"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var V = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("debug"), V), new ListPair(new ListPair(new ListPair(new Functor2("<--", V, Atom.a("off")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var V = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", 5, V), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var V = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("warning"), V), new ListPair(new Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("warning")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cut"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("!"), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cut"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cut"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", new Functor1("call", Atom.a("!")), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("fail"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("fail"), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("fail"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("undef_pred"), new ListPair(new Functor2("existence_error", Atom.a("procedure"), new Functor2("/", Atom.a("undef_pred"), 0)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("fail"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), Atom.a("undef_pred")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("fail"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Atom.a("undef_pred")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", new Functor2("+", X, Y), new Functor2("=", X, 1), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(new Functor2("+", 1, x4), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, Atom.a("fail"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 1)), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(1, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 1)), new ListPair(1, new ListPair(2, Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new ListPair(X, new ListPair(Y, Atom.NIL))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Goal = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, Goal, S), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, 4, S), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("findall"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor1("call", 1), S), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("float", 3.3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("float", -3.3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("float", 3), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("float", Atom.a("atom")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("float", X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), Atom.a("foo"), 3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), X, Y), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("foo")), new ListPair(new Functor2("<--", Y, 3), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), 3), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new Functor3("foo", A, B, C)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), 0), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor2("mats", A, B), A, B), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("mats")), new ListPair(new Functor2("<--", B, 2), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor1("foo", Atom.a("a")), Atom.a("foo"), 2), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor1("foo", Atom.a("a")), Atom.a("fo"), 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", 1, X, Y), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), new ListPair(new Functor2("<--", Y, 0), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, 1.1, 0), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1.1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new ListPair(x1, x2), Atom.a("."), 2), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", Atom.NIL, Atom.NIL, 0), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Y, 3), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), N), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), Atom.a("a")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var F = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", F, 1.5, 1), new ListPair(new Functor2("type_error", Atom.a("atom"), 1.5), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var F = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", F, new Functor1("foo", Atom.a("a")), 1), new ListPair(new Functor2("type_error", Atom.a("atomic"), new Functor1("foo", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var X = new Variable();
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("current_prolog_flag", Atom.a("max_arity"), A), new Functor2(",", new Functor2("is", X, new Functor2("+", A, 1)), new Functor3("functor", T, Atom.a("foo"), X))), new ListPair(new Functor1("representation_error", Atom.a("max_arity")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("functor", T, Atom.a("foo"), -1), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("fail"), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), new Functor2("=", X, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), Atom.a("true")), Atom.a("fail")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), Atom.a("fail")), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), new Functor2("=", X, 1)), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), new Functor2("=", X, 1)), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("if-then-else"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), Atom.a("true")), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("integer", 3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("integer", -3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("integer", 3.3), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("integer", X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("integer", Atom.a("atom")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Result = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("is", Result, new Functor2("+", 3, 11.0)), new ListPair(new ListPair(new ListPair(new Functor2("<--", Result, 14.0), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, new Functor2("+", 1, 2)), new Functor2("is", Y, new Functor2("*", X, 3))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new Functor2("+", 1, 2)), new ListPair(new Functor2("<--", Y, 9), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("is", Atom.a("foo"), 77), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("is", 77, N), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("is", 77, Atom.a("foo")), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("foo"), 0)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("is"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("is", X, new Functor1("float", 3)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 3.0), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", 33.3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", Atom.a("foo")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Foo = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", Foo), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Foo = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", Atom.a("foo"), Foo), new Functor1("nonvar", Foo)), new ListPair(new ListPair(new ListPair(new Functor2("<--", Foo, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", x1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", Atom.a("!")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", new Functor2(",", Atom.a("!"), Atom.a("fail"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new Functor1("\\+", new Functor2(",", Atom.a("!"), Atom.a("fail")))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", new Functor2("=", 4, 5)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", 3), new ListPair(new Functor2("type_error", Atom.a("callable"), 3), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_provable"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", X, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", X, Y), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("\\=", X, Y), new Functor2("\\=", X, Atom.a("abc"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor2("f", X, Atom.a("def")), new Functor2("f", Atom.a("def"), Y)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 2), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 1.0), new ListPair(Atom.a("success"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor1("g", X), new Functor1("f", new Functor1("f", X))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor2("f", X, 1), new Functor1("f", new Functor1("a", X))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("not_unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor3("f", X, Y, X), new Functor("f", [new Functor1("a", X), new Functor1("a", Y), Y, 2])), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", 3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", 3.3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", -3), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", Atom.a("a")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("3"), new ListPair(Atom.a("3"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33, new ListPair(Atom.a("3"), new ListPair(Atom.a("3"), Atom.NIL))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33.0, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make([Atom.a("3"), Atom.a("3"), Atom.a("."), Atom.a("0")])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", X, ListPair.make([Atom.a("3"), Atom.a("."), Atom.a("3"), Atom.a("E"), Atom.a("+"), Atom.a("0")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 3.3), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 3.3, ListPair.make([Atom.a("3"), Atom.a("."), Atom.a("3")])), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("-"), Atom.a("2"), Atom.a("5")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("\x0A"), Atom.a(" "), Atom.a("3")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, new ListPair(Atom.a("3"), new ListPair(Atom.a("x"), Atom.NIL))), new ListPair(new Functor1("syntax_error", x2), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("0"), Atom.a("x"), Atom.a("f")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("0"), Atom.a("'"), Atom.a("A")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 65), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("4"), Atom.a("."), Atom.a("2")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make([Atom.a("4"), Atom.a("2"), Atom.a("."), Atom.a("0"), Atom.a("e"), Atom.a("-"), Atom.a("1")])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", Atom.a("a"), L), new ListPair(new Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, 4), new ListPair(new Functor2("type_error", Atom.a("list"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, new ListPair(Atom.a("4"), new ListPair(2, Atom.NIL))), new ListPair(new Functor2("type_error", Atom.a("character"), 2), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(51, new ListPair(51, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33, new ListPair(51, new ListPair(51, Atom.NIL))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33.0, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make([51, 51, 46, 48])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33.0, ListPair.make([51, 51, 46, 48])), new ListPair(Atom.a("success"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([45, 50, 53])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, new ListPair(32, new ListPair(51, Atom.NIL))), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([48, 120, 102])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([48, 39, 97])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 97), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([52, 46, 50])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([52, 50, 46, 48, 101, 45, 49])), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", Atom.a("a"), L), new ListPair(new Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, 4), new ListPair(new Functor2("type_error", Atom.a("list"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make([49, 50, Atom.a("3")])), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("!")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("once", Atom.a("!")), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("repeat")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("once", 3), new ListPair(new Functor2("type_error", Atom.a("callable"), 3), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("once"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("once", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", Atom.a("true"), Atom.a("fail")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", Atom.a("!"), new Functor1("call", 3)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", new Functor2("=", X, 1), Atom.a("!")), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("or"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("repeat"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", Atom.a("repeat"), new Functor2(",", Atom.a("!"), Atom.a("fail"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("retract"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("retract", new Functor2(":-", 4, X)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("retract"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("retract", new Functor2(":-", new Functor1("atom", x1), new Functor2("==", X, Atom.NIL))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var V = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), new Functor2("current_prolog_flag", Atom.a("unknown"), V)), new ListPair(new ListPair(new ListPair(new Functor2("<--", V, Atom.a("fail")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", X, Atom.a("warning")), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", 5, Atom.a("decimals")), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("date"), Atom.a("July 1999")), new ListPair(new Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("date")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("debug"), Atom.a("no")), new ListPair(new Functor2("domain_error", Atom.a("flag_value"), new Functor2("+", Atom.a("debug"), Atom.a("no"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("max_arity"), 40), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("flag"), Atom.a("max_arity")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("fred")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("chars")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make([Atom.a("f"), Atom.a("r"), Atom.a("e"), Atom.a("d")])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("codes")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make([102, 114, 101, 100])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 1)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(2, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, Atom.a("fail"), L), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Y = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", 1, new Functor2(";", new Functor2("=", Y, 2), new Functor2("=", Y, 1)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var L = new Variable();
    var x4 = new Variable();
    var x5 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", new Functor2("f", X, Y), new Functor2(";", new Functor2("=", X, Atom.a("a")), new Functor2("=", Y, Atom.a("b"))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(new Functor2("f", x4, Atom.a("b")), new ListPair(new Functor2("f", Atom.a("a"), x5), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2(",", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make([x4, 1, 2])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("setof", X, new Functor2(";", new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1))), new Functor2("=", X, 3)), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var S = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2(";", new Functor2("=", Y, 1), new Functor2("=", X, 3)))), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make([x4, 1, 3])), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    var Z = new Variable();
    var L = new Variable();
    var x5 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2(";", new Functor2("=", X, Z), new Functor2("=", Y, 1))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(x5, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", X, new Functor2(";", Atom.a("true"), 4)), L), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("setof"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, 1, L), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("abracadabra"), 0, 5, x1, S2]), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("abrac")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("abracadabra"), x1, 5, 0, S2]), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("dabra")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Length = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("abracadabra"), 3, Length, 3, S2]), new ListPair(new ListPair(new ListPair(new Functor2("<--", Length, 5), new ListPair(new Functor2("<--", S2, Atom.a("acada")), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Before = new Variable();
    var After = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("abracadabra"), Before, 2, After, Atom.a("ab")]), new ListPair(new ListPair(new ListPair(new Functor2("<--", Before, 0), new ListPair(new Functor2("<--", After, 9), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", Before, 7), new ListPair(new Functor2("<--", After, 2), Atom.NIL)), Atom.NIL)), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("Banana"), 3, 2, x1, S2]), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("an")), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Before = new Variable();
    var After = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("charity"), Before, 3, After, S2]), new ListPair(ListPair.make([ListPair.make([new Functor2("<--", Before, 0), new Functor2("<--", After, 4), new Functor2("<--", S2, Atom.a("cha"))]), ListPair.make([new Functor2("<--", Before, 1), new Functor2("<--", After, 3), new Functor2("<--", S2, Atom.a("har"))]), ListPair.make([new Functor2("<--", Before, 2), new Functor2("<--", After, 2), new Functor2("<--", S2, Atom.a("ari"))]), ListPair.make([new Functor2("<--", Before, 3), new Functor2("<--", After, 1), new Functor2("<--", S2, Atom.a("rit"))]), ListPair.make([new Functor2("<--", Before, 4), new Functor2("<--", After, 0), new Functor2("<--", S2, Atom.a("ity"))])]), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Before = new Variable();
    var Length = new Variable();
    var After = new Variable();
    var Sub_atom = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("ab"), Before, Length, After, Sub_atom]), new ListPair(ListPair.make([ListPair.make([new Functor2("<--", Before, 0), new Functor2("<--", Length, 0), new Functor2("<--", After, 2), new Functor2("<--", Sub_atom, Atom.a(""))]), ListPair.make([new Functor2("<--", Before, 0), new Functor2("<--", Length, 1), new Functor2("<--", After, 1), new Functor2("<--", Sub_atom, Atom.a("a"))]), ListPair.make([new Functor2("<--", Before, 0), new Functor2("<--", Length, 2), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a("ab"))]), ListPair.make([new Functor2("<--", Before, 1), new Functor2("<--", Length, 0), new Functor2("<--", After, 1), new Functor2("<--", Sub_atom, Atom.a(""))]), ListPair.make([new Functor2("<--", Before, 1), new Functor2("<--", Length, 1), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a("b"))]), ListPair.make([new Functor2("<--", Before, 2), new Functor2("<--", Length, 0), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a(""))])]), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var Banana = new Variable();
    var x2 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Banana, 3, 2, x2, S2]), new ListPair(Atom.a("instantiation_error"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [new Functor1("f", Atom.a("a")), 2, 2, x1, S2]), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("Banana"), 4, 2, x1, 2]), new ListPair(new Functor2("type_error", Atom.a("atom"), 2), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("Banana"), Atom.a("a"), 2, x1, S2]), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("Banana"), 4, Atom.a("n"), x1, S2]), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("n")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", [Atom.a("Banana"), 4, x1, Atom.a("m"), S2]), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("m")), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", 1, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", 1, 2), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, Y), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", x1, x2), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, new Functor1("a", X)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_diff"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", new Functor1("f", Atom.a("a")), new Functor1("f", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", 1, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, X), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", 1, 2), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, Y), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", x1, x2), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, new Functor1("a", X)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_eq"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("==", new Functor1("f", Atom.a("a")), new Functor1("f", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", X, X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", X, X), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_gt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", X, X), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@<", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", X, X), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("term_lt="))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("true"), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 1), new ListPair(Atom.a("success"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", X, 1), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, Y), new Functor2("=", X, Atom.a("abc"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("abc")), new ListPair(new Functor2("<--", Y, Atom.a("abc")), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor2("f", X, Atom.a("def")), new Functor2("f", Atom.a("def"), Y)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("def")), new ListPair(new Functor2("<--", Y, Atom.a("def")), Atom.NIL)), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 2), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 1.0), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        for each (var l4 in intAndFloatAreDifferent()) {
          yield false;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor1("g", X), new Functor1("f", new Functor1("f", X))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor2("f", X, 1), new Functor1("f", new Functor1("a", X))), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor3("f", X, Y, X), new Functor("f", [new Functor1("a", X), new Functor1("a", Y), Y, 2])), new ListPair(Atom.a("failure"), Atom.NIL)))) {
        yield false;
      }
    }
  }
  {
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    var D = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("unify"))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor3("f", A, B, C), new Functor3("f", new Functor2("g", B, B), new Functor2("g", C, C), new Functor2("g", D, D))), new ListPair(new ListPair(ListPair.make([new Functor2("<--", A, new Functor2("g", new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D)), new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D)))), new Functor2("<--", B, new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D))), new Functor2("<--", C, new Functor2("g", D, D))]), Atom.NIL), Atom.NIL)))) {
        yield false;
      }
    }
  }
}

main();
