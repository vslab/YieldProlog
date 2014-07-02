import sys
# Hack sys.path for the examples.
sys.path.append("../..")
from YP import *
from Atom import *
from BagofAnswers import *
from FindallAnswers import *
from Functor1 import *
from Functor2 import *
from Functor3 import *
from Functor import *
from ListPair import *
from Variable import *
from time import *

def main():
    startTime = clock()
    nAnswers = 0
    for l1 in run_all_tests():
        nAnswers += 1
    finishTime = clock()
    print "ISO Test Suite:", (finishTime - startTime), "seconds,", \
          nAnswers, "answers"

# Following is the compiled code from YieldProlog/source/prolog/isoTestSuite.P

def getDeclaringClass():
  return globals()

def run_all_tests():
  F = Variable()
  Files = Variable()
  findallAnswers1 = FindallAnswers(F)
  for l1 in file(F):
    findallAnswers1.add()
  for l1 in findallAnswers1.result(Files):
    for l2 in test_all(Files):
      for l3 in write_results():
        yield True
        return

def test_all(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    yield False
  F = Variable()
  Fs = Variable()
  for l1 in YP.unify(arg1, ListPair(F, Fs)):
    for l2 in run_tests(F):
      for l3 in test_all(Fs):
        yield False

def write_results():
  doBreak = False
  for _ in [1]:
    F = Variable()
    ErrorBips = Variable()
    findallAnswers1 = FindallAnswers(F)
    for l2 in inerror(F):
      findallAnswers1.add()
    if doBreak:
      break
    for l2 in findallAnswers1.result(ErrorBips):
      YP.write(Atom.a("--------------------"))
      YP.nl()
      cutIf2 = False
      for _ in [1]:
        for l4 in YP.unify(ErrorBips, Atom.NIL):
          YP.write(Atom.a("All bips passed -------------"))
          YP.nl()
          yield False
          cutIf2 = True
          doBreak = True
          break
        if doBreak:
          break
        YP.nl()
        YP.write(Atom.a("The following BIPs gave unexpected answers:"))
        YP.nl()
        YP.write(Atom.a("The results should be examined carefully."))
        YP.nl()
        YP.nl()
        for l4 in display_list(ErrorBips):
          yield False
        if doBreak:
          break
      if cutIf2:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def result(G, Res):
  Subs = Variable()
  for l1 in get_all_subs(G, Subs):
    for l2 in special_ans_forms(Subs, Res):
      yield False

def special_ans_forms(arg1, arg2):
  for l1 in YP.unify(arg1, ListPair(Atom.a("success"), Atom.NIL)):
    for l2 in YP.unify(arg2, Atom.a("success")):
      yield True
      return
  for l1 in YP.unify(arg1, ListPair(Atom.a("failure"), Atom.NIL)):
    for l2 in YP.unify(arg2, Atom.a("failure")):
      yield True
      return
  Error = arg2
  E = Variable()
  x3 = Variable()
  for l1 in YP.unify(arg1, ListPair(Error, Atom.NIL)):
    for l2 in YP.univ(Error, ListPair(E, x3)):
      for l3 in error_type(E):
        yield True
        return
  X = Variable()
  for l1 in YP.unify(arg1, X):
    for l2 in YP.unify(arg2, X):
      yield False

def error_type(arg1):
  for l1 in YP.unify(arg1, Atom.a("instantiation_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("type_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("domain_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("existence_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("permission_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("representation_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("evaluation_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("resource_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("syntax_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("system_error")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("unexpected_ball")):
    yield False

def vars_in_term(T, V):
  for l1 in vars_in_term3(T, Atom.NIL, V):
    yield False

def vars_in_term3(arg1, VarsIn, arg3):
  doBreak = False
  for _ in [1]:
    Term = arg1
    VarsOut = arg3
    if YP.atomic(Term):
      for l3 in YP.unify(VarsOut, VarsIn):
        yield False
      if doBreak:
        break
      return
  for _ in [1]:
    Term = arg1
    VarsOut = arg3
    if YP.var(Term):
      cutIf1 = False
      for _ in [1]:
        for l4 in already_appears(Term, VarsIn):
          for l5 in YP.unify(VarsOut, VarsIn):
            yield False
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        for l4 in append(VarsIn, ListPair(Term, Atom.NIL), VarsOut):
          yield False
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
      return
  for _ in [1]:
    Vars = arg3
    A = Variable()
    B = Variable()
    V1 = Variable()
    for l2 in YP.unify(arg1, ListPair(A, B)):
      for l3 in vars_in_term3(A, VarsIn, V1):
        for l4 in vars_in_term3(B, V1, Vars):
          yield False
        if doBreak:
          break
      if doBreak:
        break
      return
    if doBreak:
      break
  for _ in [1]:
    T = arg1
    VarList = arg3
    _F = Variable()
    A = Variable()
    Args = Variable()
    Inter = Variable()
    for l2 in YP.univ(T, ListPair(_F, ListPair(A, Args))):
      for l3 in vars_in_term3(A, VarsIn, Inter):
        for l4 in vars_in_term3(Args, Inter, VarList):
          yield False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def already_appears(Var, arg2):
  V1 = Variable()
  _Vlist = Variable()
  for l1 in YP.unify(arg2, ListPair(V1, _Vlist)):
    if YP.termEqual(Var, V1):
      yield False
  _V1 = Variable()
  Vlist = Variable()
  for l1 in YP.unify(arg2, ListPair(_V1, Vlist)):
    for l2 in already_appears(Var, Vlist):
      yield False

def call_goal_get_subs(G, Sub):
  GT = Variable()
  Vars = Variable()
  GVars = Variable()
  for l1 in YP.copy_term(G, GT):
    for l2 in vars_in_term(G, Vars):
      for l3 in vars_in_term(GT, GVars):
        for l4 in YP.getIterator(GT, getDeclaringClass()):
          for l5 in make_subs_list1(Vars, GVars, Sub):
            yield False

def make_subs_list1(arg1, arg2, arg3):
  _V = arg1
  for l1 in YP.unify(arg2, Atom.a("success")):
    for l2 in YP.unify(arg3, Atom.a("success")):
      yield False
  _V = arg1
  for l1 in YP.unify(arg2, Atom.a("failure")):
    for l2 in YP.unify(arg3, Atom.a("failure")):
      yield False
  _V = arg1
  for l1 in YP.unify(arg2, Atom.a("impl_def")):
    for l2 in YP.unify(arg3, Atom.a("impl_def")):
      yield False
  _V = arg1
  for l1 in YP.unify(arg2, Atom.a("undefined")):
    for l2 in YP.unify(arg3, Atom.a("undefined")):
      yield False
  _V = arg1
  Error = Variable()
  E = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg2, Error):
    for l2 in YP.unify(arg3, Error):
      for l3 in YP.univ(Error, ListPair(E, x4)):
        for l4 in error_type(E):
          yield True
          return
  Vs = arg1
  GVs = arg2
  Sub = arg3
  S = Variable()
  for l1 in make_subs_list(Vs, GVs, S):
    for l2 in compress_sub_list(Vs, S, Sub):
      yield False

def make_subs_list(arg1, arg2, arg3):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Atom.NIL):
      for l3 in YP.unify(arg3, Atom.NIL):
        yield False
  Subs = arg3
  V = Variable()
  Rest = Variable()
  Ans = Variable()
  ARest = Variable()
  for l1 in YP.unify(arg1, ListPair(V, Rest)):
    for l2 in YP.unify(arg2, ListPair(Ans, ARest)):
      if YP.termEqual(V, Ans):
        for l4 in make_subs_list(Rest, ARest, Subs):
          yield False
        return
  V = Variable()
  Rest = Variable()
  Ans = Variable()
  ARest = Variable()
  SubsRest = Variable()
  for l1 in YP.unify(arg1, ListPair(V, Rest)):
    for l2 in YP.unify(arg2, ListPair(Ans, ARest)):
      for l3 in YP.unify(arg3, ListPair(Functor2("<--", V, Ans), SubsRest)):
        for l4 in make_subs_list(Rest, ARest, SubsRest):
          yield False

def list_make_subs_list(arg1, arg2, arg3):
  x1 = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, ListPair(Atom.a("failure"), Atom.NIL)):
      yield True
      return
  V = arg1
  GTV = arg2
  S = arg3
  for l1 in list_make_subs_list_aux(V, GTV, S):
    yield False

def list_make_subs_list_aux(arg1, arg2, arg3):
  _Vars = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield False
  Vars = arg1
  GV1 = Variable()
  GVRest = Variable()
  Sub1 = Variable()
  SubRest = Variable()
  for l1 in YP.unify(arg2, ListPair(GV1, GVRest)):
    for l2 in YP.unify(arg3, ListPair(Sub1, SubRest)):
      for l3 in make_subs_list1(Vars, GV1, Sub1):
        for l4 in list_make_subs_list_aux(Vars, GVRest, SubRest):
          yield False

def call_with_result(arg1, arg2):
  doBreak = False
  for _ in [1]:
    G = arg1
    R = arg2
    Sub = Variable()
    for l2 in call_goal_get_subs(G, Sub):
      cutIf1 = False
      for _ in [1]:
        for l4 in YP.unify(Sub, Atom.NIL):
          for l5 in YP.unify(R, Atom.a("success")):
            yield False
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        for l4 in YP.unify(R, Sub):
          yield False
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    _G = arg1
    for l2 in YP.unify(arg2, Atom.a("failure")):
      yield False
    if doBreak:
      break

def protected_call_results(G, R):
  B = Variable()
  catchGoal1 = YP.Catch(Functor2(Atom.a("call_with_result", Atom.a("")), G, R), getDeclaringClass())
  for l1 in catchGoal1:
    yield False
  for l1 in catchGoal1.unifyExceptionOrThrow(B):
    for l2 in YP.unify(R, B):
      yield False

def get_all_subs(G, AllSubs):
  GT = Variable()
  GVars = Variable()
  GTAns = Variable()
  GTAnsList = Variable()
  for l1 in YP.copy_term(G, GT):
    for l2 in vars_in_term(G, GVars):
      findallAnswers1 = FindallAnswers(GTAns)
      for l3 in protect_call_result(GT, GTAns):
        findallAnswers1.add()
      for l3 in findallAnswers1.result(GTAnsList):
        for l4 in list_make_subs_list(GVars, GTAnsList, AllSubs):
          yield False

def call_result(G, R):
  GVars = Variable()
  for l1 in vars_in_term(G, GVars):
    for l2 in YP.getIterator(G, getDeclaringClass()):
      for l3 in YP.unify(R, GVars):
        yield False

def protect_call_result(G, R):
  B = Variable()
  catchGoal1 = YP.Catch(Functor2(Atom.a("call_result", Atom.a("")), G, R), getDeclaringClass())
  for l1 in catchGoal1:
    yield False
  for l1 in catchGoal1.unifyExceptionOrThrow(B):
    for l2 in extract_error(B, R):
      yield False

def extract_error(arg1, arg2):
  R = arg2
  x2 = Variable()
  for l1 in YP.unify(arg1, Functor2("error", R, x2)):
    yield True
    return
  B = arg1
  for l1 in YP.unify(arg2, Functor1("unexpected_ball", B)):
    yield False

def compress_sub_list(arg1, arg2, arg3):
  x1 = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.a("success")):
      yield True
      return
  Vars = arg1
  X = Variable()
  A = Variable()
  for l1 in YP.unify(arg2, ListPair(Functor2("<--", X, A), Atom.NIL)):
    for l2 in YP.unify(arg3, ListPair(Functor2("<--", X, A), Atom.NIL)):
      if YP.termNotEqual(X, A):
        for l4 in in_vars(A, Vars):
          yield False
  Vars = arg1
  LIn = arg2
  LOut = arg3
  X = Variable()
  A = Variable()
  Before = Variable()
  After = Variable()
  BN = Variable()
  AN = Variable()
  L1 = Variable()
  for l1 in split_list(Functor2("<--", X, A), Before, After, LIn):
    if YP.var(A):
      for l3 in sub(Functor2("<--", X, A), Before, BN):
        for l4 in sub(Functor2("<--", X, A), After, AN):
          for l5 in append(BN, AN, L1):
            for l6 in compress_sub_list(Vars, L1, LOut):
              yield False
      return
  x1 = arg1
  L = Variable()
  for l1 in YP.unify(arg2, L):
    for l2 in YP.unify(arg3, L):
      yield False

def in_vars(V, arg2):
  V1 = Variable()
  _Vs = Variable()
  for l1 in YP.unify(arg2, ListPair(V1, _Vs)):
    if YP.termEqual(V, V1):
      yield True
      return
  _V1 = Variable()
  Vs = Variable()
  for l1 in YP.unify(arg2, ListPair(_V1, Vs)):
    for l2 in in_vars(V, Vs):
      yield False

def sub(arg1, arg2, arg3):
  _X = Variable()
  _A = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", _X, _A)):
    for l2 in YP.unify(arg2, Atom.NIL):
      for l3 in YP.unify(arg3, Atom.NIL):
        yield False
  X = Variable()
  A = Variable()
  H = Variable()
  T = Variable()
  H1 = Variable()
  T1 = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", X, A)):
    for l2 in YP.unify(arg2, ListPair(H, T)):
      for l3 in YP.unify(arg3, ListPair(H1, T1)):
        for l4 in sub1(Functor2("<--", X, A), H, H1):
          for l5 in sub(Functor2("<--", X, A), T, T1):
            yield False

def sub1(arg1, arg2, arg3):
  X = Variable()
  A = Variable()
  Y = Variable()
  Old = Variable()
  New = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", X, A)):
    for l2 in YP.unify(arg2, Functor2("<--", Y, Old)):
      for l3 in YP.unify(arg3, Functor2("<--", Y, New)):
        for l4 in exp_sub(Functor2("<--", X, A), Old, New):
          yield False

def exp_sub(arg1, B, New):
  X = Variable()
  A = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", X, A)):
    if YP.var(B):
      if YP.termEqual(B, A):
        for l4 in YP.unify(New, X):
          yield False
        return
  _X = Variable()
  _A = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", _X, _A)):
    if YP.var(B):
      for l3 in YP.unify(New, B):
        yield False
      return
  _X = Variable()
  _A = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", _X, _A)):
    if YP.atomic(B):
      for l3 in YP.unify(New, B):
        yield False
      return
  X = Variable()
  A = Variable()
  x5 = Variable()
  x6 = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", X, A)):
    for l2 in YP.unify(B, ListPair(x5, x6)):
      for l3 in list_exp_sub(Functor2("<--", X, A), B, New):
        yield False
      return
  X = Variable()
  A = Variable()
  F = Variable()
  L = Variable()
  L1 = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", X, A)):
    for l2 in YP.univ(B, ListPair(F, L)):
      for l3 in list_exp_sub(Functor2("<--", X, A), L, L1):
        for l4 in YP.univ(New, ListPair(F, L1)):
          yield False

def list_exp_sub(arg1, arg2, arg3):
  _S = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield False
  S = arg1
  E = Variable()
  ER = Variable()
  EN = Variable()
  ERN = Variable()
  for l1 in YP.unify(arg2, ListPair(E, ER)):
    for l2 in YP.unify(arg3, ListPair(EN, ERN)):
      for l3 in exp_sub(S, E, EN):
        for l4 in list_exp_sub(S, ER, ERN):
          yield False

def split_list(Element, Before, After, List):
  for l1 in append(Before, ListPair(Element, After), List):
    yield False

def compare_subst_lists(arg1, arg2, arg3, arg4):
  doBreak = False
  for _ in [1]:
    F = arg1
    S = arg2
    x3 = Variable()
    x4 = Variable()
    x5 = Variable()
    x6 = Variable()
    for l2 in YP.unify(arg3, Atom.NIL):
      for l3 in YP.unify(arg4, Atom.NIL):
        cutIf1 = False
        for _ in [1]:
          for l5 in YP.unify(F, ListPair(x3, x4)):
            cutIf1 = True
            doBreak = True
            break
          if doBreak:
            break
          cutIf2 = False
          for _ in [1]:
            for l6 in YP.unify(S, ListPair(x5, x6)):
              cutIf2 = True
              doBreak = True
              break
            if doBreak:
              break
            for l6 in YP.unify(F, S):
              yield True
              return
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
    if doBreak:
      break
  for _ in [1]:
    F = Variable()
    S = Variable()
    x3 = Variable()
    x4 = Variable()
    x5 = Variable()
    x6 = Variable()
    for l2 in YP.unify(arg1, F):
      for l3 in YP.unify(arg2, S):
        for l4 in YP.unify(arg3, F):
          for l5 in YP.unify(arg4, S):
            cutIf3 = False
            for _ in [1]:
              for l7 in YP.unify(F, ListPair(x3, x4)):
                cutIf3 = True
                doBreak = True
                break
              if doBreak:
                break
              cutIf4 = False
              for _ in [1]:
                for l8 in YP.unify(S, ListPair(x5, x6)):
                  cutIf4 = True
                  doBreak = True
                  break
                if doBreak:
                  break
                yield True
                return
              if cutIf4:
                doBreak = False
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
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    FNS = arg3
    SNF = arg4
    x5 = Variable()
    x6 = Variable()
    cutIf5 = False
    for _ in [1]:
      for l3 in YP.unify(F, ListPair(x5, x6)):
        cutIf5 = True
        doBreak = True
        break
      if doBreak:
        break
      for l3 in del_item(F, S, SNF):
        cutIf6 = False
        for _ in [1]:
          for l5 in member(F, S):
            for l6 in YP.unify(FNS, Atom.NIL):
              yield False
            if doBreak:
              break
            cutIf6 = True
            doBreak = True
            break
          if doBreak:
            break
          for l5 in YP.unify(FNS, F):
            yield False
          if doBreak:
            break
        if cutIf6:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
      return
    if cutIf5:
      doBreak = False
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    FNS = arg3
    SNF = arg4
    x5 = Variable()
    x6 = Variable()
    cutIf7 = False
    for _ in [1]:
      for l3 in YP.unify(S, ListPair(x5, x6)):
        cutIf7 = True
        doBreak = True
        break
      if doBreak:
        break
      for l3 in del_item(S, F, FNS):
        cutIf8 = False
        for _ in [1]:
          for l5 in member(S, F):
            for l6 in YP.unify(SNF, Atom.NIL):
              yield False
            if doBreak:
              break
            cutIf8 = True
            doBreak = True
            break
          if doBreak:
            break
          for l5 in YP.unify(SNF, S):
            yield False
          if doBreak:
            break
        if cutIf8:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
      return
    if cutIf7:
      doBreak = False
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    F1 = Variable()
    S1 = Variable()
    for l2 in YP.unify(arg3, Atom.NIL):
      for l3 in YP.unify(arg4, Atom.NIL):
        for l4 in YP.unify(F, ListPair(F1, Atom.NIL)):
          for l5 in YP.unify(S, ListPair(S1, Atom.NIL)):
            for l6 in same_subst(F1, S1):
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
  for _ in [1]:
    F = Variable()
    S = Variable()
    for l2 in YP.unify(arg1, F):
      for l3 in YP.unify(arg2, S):
        for l4 in YP.unify(arg3, F):
          for l5 in YP.unify(arg4, S):
            for l6 in length(F, 1):
              for l7 in length(S, 1):
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
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    FNS = arg3
    SNF = arg4
    for l2 in length(F, 1):
      for l3 in del_item(F, S, SNF):
        cutIf9 = False
        for _ in [1]:
          for l5 in member(F, S):
            for l6 in YP.unify(FNS, Atom.NIL):
              yield False
            if doBreak:
              break
            cutIf9 = True
            doBreak = True
            break
          if doBreak:
            break
          for l5 in YP.unify(FNS, F):
            yield False
          if doBreak:
            break
        if cutIf9:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
      return
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    FNS = arg3
    SNF = arg4
    for l2 in length(S, 1):
      for l3 in del_item(S, F, FNS):
        cutIf10 = False
        for _ in [1]:
          for l5 in member(S, F):
            for l6 in YP.unify(SNF, Atom.NIL):
              yield False
            if doBreak:
              break
            cutIf10 = True
            doBreak = True
            break
          if doBreak:
            break
          for l5 in YP.unify(SNF, S):
            yield False
          if doBreak:
            break
        if cutIf10:
          doBreak = False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    F = arg1
    S = arg2
    FNS = arg3
    SNF = arg4
    for l2 in list_del_item(F, S, SNF):
      for l3 in list_del_item(S, F, FNS):
        yield False
      if doBreak:
        break
    if doBreak:
      break

def list_del_item(arg1, arg2, arg3):
  L = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, L):
      for l3 in YP.unify(arg3, L):
        yield False
  L1 = arg2
  Left = arg3
  It = Variable()
  R = Variable()
  LInter = Variable()
  for l1 in YP.unify(arg1, ListPair(It, R)):
    for l2 in del_item(It, L1, LInter):
      for l3 in list_del_item(R, LInter, Left):
        yield False

def del_item(arg1, arg2, arg3):
  _Item = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield False
  Item = arg1
  R = arg3
  It = Variable()
  for l1 in YP.unify(arg2, ListPair(It, R)):
    for l2 in same_subst(Item, It):
      yield True
      return
  Item = arg1
  It = Variable()
  Rest = Variable()
  R = Variable()
  for l1 in YP.unify(arg2, ListPair(It, Rest)):
    for l2 in YP.unify(arg3, ListPair(It, R)):
      for l3 in del_item(Item, Rest, R):
        yield False

def same_subst(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield False
  Subs = arg2
  S1 = Variable()
  SRest = Variable()
  Subs1 = Variable()
  for l1 in YP.unify(arg1, ListPair(S1, SRest)):
    for l2 in delmemb(S1, Subs, Subs1):
      for l3 in same_subst(SRest, Subs1):
        yield False

def delmemb(arg1, arg2, arg3):
  _E = arg1
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      yield False
  R = arg3
  E = Variable()
  E1 = Variable()
  F = Variable()
  F1 = Variable()
  for l1 in YP.unify(arg1, Functor2("<--", E, E1)):
    for l2 in YP.unify(arg2, ListPair(Functor2("<--", F, F1), R)):
      if YP.termEqual(E, F):
        for l4 in YP.copy_term(Functor2("<--", E, E1), Functor2("<--", F, F1)):
          yield False
  E = arg1
  F = Variable()
  R = Variable()
  R1 = Variable()
  for l1 in YP.unify(arg2, ListPair(F, R)):
    for l2 in YP.unify(arg3, ListPair(F, R1)):
      for l3 in delmemb(E, R, R1):
        yield False

def run_tests(File):
  S = Variable()
  YP.asserta(Functor3("score", File, Functor1("total", 0), Functor1("wrong", 0)), getDeclaringClass())
  for l1 in open(File, Atom.a("read"), S):
    for l2 in loop_through(File, S):
      for l3 in close(S):
        yield False

def loop_through(arg1, _S):
  F = arg1
  X = Variable()
  for l1 in fileContents(F, X):
    for l2 in reset_flags():
      for l3 in test(F, X):
        pass
  _F = arg1
  yield False

def test(arg1, arg2):
  x1 = arg1
  for l1 in YP.unify(arg2, Atom.a("end_of_file")):
    yield False
  F = arg1
  R = Variable()
  x3 = Variable()
  for l1 in YP.unify(arg2, Functor2("error", R, x3)):
    YP.write(Atom.a("Error in Input: "))
    YP.write(R)
    YP.nl()
    YP.nl()
    for l2 in update_score(F, Atom.a("non_null"), Atom.a("non_null")):
      yield False
    return
  F = arg1
  G = Variable()
  Expected = Variable()
  R = Variable()
  Extra = Variable()
  Missing = Variable()
  for l1 in YP.unify(arg2, ListPair(G, ListPair(Expected, Atom.NIL))):
    for l2 in result(G, R):
      for l3 in compare_subst_lists(R, Expected, Extra, Missing):
        for l4 in write_if_wrong(F, G, Expected, Extra, Missing):
          for l5 in update_score(F, Missing, Extra):
            yield False

def write_if_wrong(arg1, arg2, arg3, arg4, arg5):
  x1 = arg1
  x2 = arg2
  x3 = arg3
  for l1 in YP.unify(arg4, Atom.NIL):
    for l2 in YP.unify(arg5, Atom.NIL):
      yield True
      return
  F = arg1
  G = arg2
  Expected = arg3
  Extra = arg4
  Missing = arg5
  x6 = Variable()
  for l1 in fake_numbervars(ListPair.make([G, Expected, Missing]), 0, x6):
    YP.write(Atom.a("In file: "))
    YP.write(F)
    YP.nl()
    YP.write(Atom.a("possible error in Goal: "))
    YP.write(G)
    YP.nl()
    YP.write(Atom.a("Expected: "))
    YP.write(Expected)
    YP.nl()
    YP.write(Atom.a("Extra Solutions found: "))
    YP.write(Extra)
    YP.nl()
    YP.write(Atom.a("Solutions Missing: "))
    YP.write(Missing)
    YP.nl()
    YP.nl()
    yield False

def update_score(F, arg2, arg3):
  T = Variable()
  W = Variable()
  T1 = Variable()
  for l1 in YP.unify(arg2, Atom.NIL):
    for l2 in YP.unify(arg3, Atom.NIL):
      for l3 in YP.retract(Functor3("score", F, Functor1("total", T), Functor1("wrong", W))):
        for l4 in YP.unify(T1, YP.add(T, 1)):
          YP.asserta(Functor3("score", F, Functor1("total", T1), Functor1("wrong", W)), getDeclaringClass())
          yield False
      return
  x2 = arg2
  x3 = arg3
  T = Variable()
  W = Variable()
  T1 = Variable()
  W1 = Variable()
  for l1 in YP.retract(Functor3("score", F, Functor1("total", T), Functor1("wrong", W))):
    for l2 in YP.unify(T1, YP.add(T, 1)):
      for l3 in YP.unify(W1, YP.add(W, 1)):
        YP.asserta(Functor3("score", F, Functor1("total", T1), Functor1("wrong", W1)), getDeclaringClass())
        yield False

def inerror(F):
  _X = Variable()
  Y = Variable()
  for l1 in YP.matchDynamic(Atom.a("score"), [F, Functor1("total", _X), Functor1("wrong", Y)]):
    if YP.notEqual(Y, 0):
      yield False

def file(arg1):
  for l1 in YP.unify(arg1, Atom.a("fail")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("and")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arg")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("catch-and-throw")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("clause")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("current_input")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("current_output")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("cut")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("findall")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("float")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("functor")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("integer")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("is")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("number")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("once")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("or")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("repeat")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("retract")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("setof")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("true")):
    yield False
  for l1 in YP.unify(arg1, Atom.a("unify")):
    yield False

def display_list(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    YP.nl()
    yield False
  H = Variable()
  T = Variable()
  for l1 in YP.unify(arg1, ListPair(H, T)):
    YP.write(H)
    YP.nl()
    for l2 in display_list(T):
      yield False

def reset_flags():
  YP.set_prolog_flag(Atom.a("unknown"), Atom.a("error"))
  yield False

def exists(arg1):
  P = Variable()
  I = Variable()
  List = Variable()
  G = Variable()
  x5 = Variable()
  for l1 in YP.unify(arg1, Functor2("/", P, I)):
    for l2 in make_list(I, List):
      for l3 in YP.univ(G, ListPair(P, List)):
        YP.set_prolog_flag(Atom.a("unknown"), Atom.a("fail"))
        catchGoal1 = YP.Catch(Functor1("call", G), getDeclaringClass())
        for l4 in catchGoal1:
          for l5 in reset_flags():
            yield True
            return
        for l4 in catchGoal1.unifyExceptionOrThrow(x5):
          for l5 in reset_flags():
            yield True
            return
  P = Variable()
  I = Variable()
  for l1 in YP.unify(arg1, Functor2("/", P, I)):
    YP.write(Atom.a("Predicate: "))
    YP.write(Functor2("/", P, I))
    YP.write(Atom.a(" not implemented"))
    YP.nl()
    for l2 in reset_flags():
      yield False

def make_list(N, L):
  if YP.greaterThanOrEqual(N, 0):
    for l2 in make_list1(N, L):
      yield False

def make_list1(arg1, arg2):
  for l1 in YP.unify(arg1, 0):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield False
  N = arg1
  x2 = Variable()
  L1 = Variable()
  N1 = Variable()
  for l1 in YP.unify(arg2, ListPair(x2, L1)):
    for l2 in YP.unify(N1, YP.subtract(N, 1)):
      for l3 in make_list(N1, L1):
        yield False

def fake_numbervars(arg1, arg2, arg3):
  X = arg1
  N = arg2
  M = arg3
  if YP.var(X):
    for l2 in YP.univ(X, ListPair(Atom.a("$VAR"), ListPair(N, Atom.NIL))):
      for l3 in YP.unify(M, YP.add(N, 1)):
        yield False
    return
  X = arg1
  N = Variable()
  for l1 in YP.unify(arg2, N):
    for l2 in YP.unify(arg3, N):
      if YP.atomic(X):
        yield True
        return
  N = arg2
  M = arg3
  H = Variable()
  T = Variable()
  N1 = Variable()
  for l1 in YP.unify(arg1, ListPair(H, T)):
    for l2 in fake_numbervars(H, N, N1):
      for l3 in fake_numbervars(T, N1, M):
        yield False
    return
  T = arg1
  N = arg2
  M = arg3
  _F = Variable()
  Args = Variable()
  for l1 in YP.univ(T, ListPair(_F, Args)):
    for l2 in fake_numbervars(Args, N, M):
      yield False

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

def length(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, 0):
      yield True
      return
  Length = arg2
  x1 = Variable()
  Rest = Variable()
  RestLength = Variable()
  for l1 in YP.unify(arg1, ListPair(x1, Rest)):
    for l2 in length(Rest, RestLength):
      for l3 in YP.unify(Length, YP.add(RestLength, 1)):
        yield False

def open(x1, x2, x3):
  yield False

def close(x1):
  yield False

def intAndFloatAreDifferent():
  if YP.termNotEqual(1.0, 1):
    yield False

def fileContents(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, ListPair(Functor1("abolish", Functor2("/", Atom.a("abolish"), 1)), ListPair(Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), Functor2("/", Atom.a("abolish"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, ListPair(Functor1("abolish", Functor2("/", Atom.a("foo"), Atom.a("a"))), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, ListPair(Functor1("abolish", Functor2("/", Atom.a("foo"), -1)), ListPair(Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL))):
      yield False
  A = Variable()
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("current_prolog_flag", Atom.a("max_arity"), A), Functor2(",", Functor2("is", X, Functor2("+", A, 1)), Functor1("abolish", Functor2("/", Atom.a("foo"), X)))), ListPair(Functor1("representation_error", Atom.a("max_arity")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("abolish")):
    for l2 in YP.unify(arg2, ListPair(Functor1("abolish", Functor2("/", 5, 2)), ListPair(Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("and")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("=", X, 1), Functor1("var", X)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("and")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor1("var", X), Functor2("=", X, 1)), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("and")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Atom.a("fail"), Functor1("call", 3)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("and")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor1("nofoo", X), Functor1("call", X)), ListPair(Functor2("existence_error", Atom.a("procedure"), Functor2("/", Atom.a("nofoo"), 1)), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("and")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("=", X, Atom.a("true")), Functor1("call", X)), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("true")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 1, Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 1, Functor2("foo", Atom.a("a"), Atom.a("b")), X), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 1, Functor2("foo", X, Atom.a("b")), Atom.a("a")), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 2, Functor3("foo", Atom.a("a"), Functor2("f", X, Atom.a("b")), Atom.a("c")), Functor2("f", Atom.a("a"), Y)), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("a")), ListPair(Functor2("<--", Y, Atom.a("b")), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor3("arg", 1, Functor2("foo", X, Atom.a("b")), Y), Functor2("=", X, Atom.a("a"))), ListPair(ListPair(ListPair(Functor2("<--", Y, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 1, Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("b")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 0, Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("foo")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 3, Functor2("foo", 3, 4), N), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", X, Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 1, X, Atom.a("a")), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 0, Atom.a("atom"), A), ListPair(Functor2("type_error", Atom.a("compound"), Atom.a("atom")), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", 0, 3, A), ListPair(Functor2("type_error", Atom.a("compound"), 3), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", -3, Functor2("foo", Atom.a("a"), Atom.a("b")), A), ListPair(Functor2("domain_error", Atom.a("not_less_than_zero"), -3), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arg")):
    for l2 in YP.unify(arg2, ListPair(Functor3("arg", Atom.a("a"), Functor2("foo", Atom.a("a"), Atom.a("b")), X), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=\\=", 0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=\\=", 1.0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=\\=", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=\\=", N, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=\\=", Functor1("floot", 1), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", 0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", 1.0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", N, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", Functor1("floot", 1), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=:=", 0.333, Functor2("/", 1, 3)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">", 0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">", 1.0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">", X, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">", Functor2("+", 2, Functor1("floot", 1)), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">=", 0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">=", 1.0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">=", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">=", X, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2(">=", Functor2("+", 2, Functor1("floot", 1)), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("<", 0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("<", 1.0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("<", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("<", X, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("<", Functor2("+", 2, Functor1("floot", 1)), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=<", 0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=<", 1.0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=<", Functor2("*", 3, 2), Functor2("-", 7, 1)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=<", X, 5), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("arith_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=<", Functor2("+", 2, Functor1("floot", 1)), 5), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("floot"), 1)), Atom.NIL))):
      yield False
  X = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor1("asserta", Functor2(":-", Functor1("bar", X), X)), Functor2("clause", Functor1("bar", X), B)), ListPair(ListPair(ListPair(Functor2("<--", B, Functor1("call", X)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    for l2 in YP.unify(arg2, ListPair(Functor1("asserta", x1), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    for l2 in YP.unify(arg2, ListPair(Functor1("asserta", 4), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    for l2 in YP.unify(arg2, ListPair(Functor1("asserta", Functor2(":-", Atom.a("foo"), 4)), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("asserta")):
    for l2 in YP.unify(arg2, ListPair(Functor1("asserta", Functor2(":-", Functor1("atom", x1), Atom.a("true"))), ListPair(Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), Functor2("/", Atom.a("atom"), 1)), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    for l2 in YP.unify(arg2, ListPair(Functor1("assertz", Functor2(":-", Functor1("foo", X), Functor2("->", X, Functor1("call", X)))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    for l2 in YP.unify(arg2, ListPair(Functor1("assertz", x1), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    for l2 in YP.unify(arg2, ListPair(Functor1("assertz", 4), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    for l2 in YP.unify(arg2, ListPair(Functor1("assertz", Functor2(":-", Atom.a("foo"), 4)), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("assertz")):
    for l2 in YP.unify(arg2, ListPair(Functor1("assertz", Functor2(":-", Functor1("atom", x1), Atom.a("true"))), ListPair(Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), Functor2("/", Atom.a("atom"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", Atom.a("atom")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", Atom.a("string")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", Functor1("a", Atom.a("b"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Var = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", Var), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", Atom.NIL), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", 6), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atom", 3.3), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.a(""), L), ListPair(ListPair(ListPair(Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.NIL, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Atom.a("["), ListPair(Atom.a("]"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.a("'"), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Atom.a("'"), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.a("iso"), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair.make([Atom.a("i"), Atom.a("s"), Atom.a("o")])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, ListPair.make([Atom.a("p"), Atom.a("r"), Atom.a("o"), Atom.a("l"), Atom.a("o"), Atom.a("g")])), ListPair(ListPair(ListPair(Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.a("North"), ListPair(Atom.a("N"), X)), ListPair(ListPair(ListPair(Functor2("<--", X, ListPair.make([Atom.a("o"), Atom.a("r"), Atom.a("t"), Atom.a("h")])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Atom.a("iso"), ListPair(Atom.a("i"), ListPair(Atom.a("s"), Atom.NIL))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, L), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  A = Variable()
  E = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, ListPair.make([Atom.a("a"), E, Atom.a("c")])), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, ListPair(Atom.a("a"), ListPair(Atom.a("b"), L))), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", Functor1("f", Atom.a("a")), L), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, Atom.a("iso")), ListPair(Functor2("type_error", Atom.a("list"), Atom.a("iso")), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_chars", A, ListPair(Atom.a("a"), ListPair(Functor1("f", Atom.a("b")), Atom.NIL))), ListPair(Functor2("type_error", Atom.a("character"), Functor1("f", Atom.a("b"))), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("atom_chars", X, ListPair(Atom.a("1"), ListPair(Atom.a("2"), Atom.NIL))), Functor2("is", Y, Functor2("+", X, 1))), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("12"), 0)), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.a(""), L), ListPair(ListPair(ListPair(Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.NIL, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(91, ListPair(93, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.a("'"), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(39, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.a("iso"), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair.make([105, 115, 111])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", A, ListPair.make([112, 114, 111, 108, 111, 103])), ListPair(ListPair(ListPair(Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.a("North"), ListPair(78, L)), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair.make([111, 114, 116, 104])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Atom.a("iso"), ListPair(105, ListPair(115, Atom.NIL))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", A, L), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", Functor1("f", Atom.a("a")), L), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", A, 120), ListPair(Functor2("type_error", Atom.a("list"), 120), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_codes", A, ListPair.make([105, 115, Atom.a("o")])), ListPair(Functor1("representation_error", Atom.a("character_code")), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), A), ListPair(ListPair(ListPair(Functor2("<--", A, Atom.a("hello world")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  T = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", T, Atom.a(" world"), Atom.a("small world")), ListPair(ListPair(ListPair(Functor2("<--", T, Atom.a("small")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), Atom.a("small world")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  T1 = Variable()
  T2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", T1, T2, Atom.a("hello")), ListPair(ListPair.make([ListPair(Functor2("<--", T1, Atom.a("")), ListPair(Functor2("<--", T2, Atom.a("hello")), Atom.NIL)), ListPair(Functor2("<--", T1, Atom.a("h")), ListPair(Functor2("<--", T2, Atom.a("ello")), Atom.NIL)), ListPair(Functor2("<--", T1, Atom.a("he")), ListPair(Functor2("<--", T2, Atom.a("llo")), Atom.NIL)), ListPair(Functor2("<--", T1, Atom.a("hel")), ListPair(Functor2("<--", T2, Atom.a("lo")), Atom.NIL)), ListPair(Functor2("<--", T1, Atom.a("hell")), ListPair(Functor2("<--", T2, Atom.a("o")), Atom.NIL)), ListPair(Functor2("<--", T1, Atom.a("hello")), ListPair(Functor2("<--", T2, Atom.a("")), Atom.NIL))]), Atom.NIL))):
      yield False
  A1 = Variable()
  A3 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", A1, Atom.a("iso"), A3), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  A2 = Variable()
  A3 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", Atom.a("iso"), A2, A3), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  A3 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", Functor1("f", Atom.a("a")), Atom.a("iso"), A3), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  A3 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", Atom.a("iso"), Functor1("f", Atom.a("a")), A3), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  A1 = Variable()
  A2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_concat")):
    for l2 in YP.unify(arg2, ListPair(Functor3("atom_concat", A1, A2, Functor1("f", Atom.a("a"))), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", Atom.a("enchanted evening"), N), ListPair(ListPair(ListPair(Functor2("<--", N, 17), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", Atom.a(""), N), ListPair(ListPair(ListPair(Functor2("<--", N, 0), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", Atom.a("scarlet"), 5), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Atom_1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", Atom_1, 4), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", 1.23, 4), ListPair(Functor2("type_error", Atom.a("atom"), 1.23), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atom_length")):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom_length", Atom.a("atom"), Atom.a("4")), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("4")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", Atom.a("atom")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", Functor1("a", Atom.a("b"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Var = Variable()
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", Var), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", Atom.NIL), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", 6), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("atomic")):
    for l2 in YP.unify(arg2, ListPair(Functor1("atomic", 3.3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), X), ListPair(ListPair(ListPair(Functor2("<--", X, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  Z = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2(";", Functor2("=", X, Y), Functor2("=", X, Z)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Y, ListPair(Z, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Atom.a("fail"), L), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Y = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", 1, Functor2(";", Functor2("=", Y, 1), Functor2("=", Y, 2)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(1, Atom.NIL)), ListPair(Functor2("<--", Y, 1), Atom.NIL)), ListPair(ListPair(Functor2("<--", L, ListPair(1, Atom.NIL)), ListPair(Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  L = Variable()
  x4 = Variable()
  x5 = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", Functor2("f", X, Y), Functor2(";", Functor2("=", X, Atom.a("a")), Functor2("=", Y, Atom.a("b"))), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Functor2("f", Atom.a("a"), x4), ListPair(Functor2("f", x5, Atom.a("b")), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2("^", Y, Functor2(";", Functor2(",", Functor2("=", X, 1), Functor2("=", Y, 1)), Functor2(",", Functor2("=", X, 2), Functor2("=", Y, 2)))), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2("^", Y, Functor2(";", Functor2(";", Functor2("=", X, 1), Functor2("=", Y, 1)), Functor2(",", Functor2("=", X, 2), Functor2("=", Y, 2)))), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair.make([1, x4, 2])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Functor3("bagof", X, Functor2(";", Functor2("^", Y, Functor2(";", Functor2("=", X, 1), Functor2("=", Y, 1))), Functor2("=", X, 3)), S)), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  Z = Variable()
  L = Variable()
  x5 = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2(";", Functor2("=", X, Y), Functor2(";", Functor2("=", X, Z), Functor2("=", Y, 1))), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Y, ListPair(Z, Atom.NIL))), Atom.NIL), ListPair(ListPair(Functor2("<--", L, ListPair(x5, Atom.NIL)), ListPair(Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  Z = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, Functor2("^", Y, Z), L), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("bagof", X, 1, L), ListPair(Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))):
      yield False
  X = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("bagof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor1("call", 4), S), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Atom.a("!")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Atom.a("fail"), X)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Atom.a("fail"), Functor1("call", 1))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Functor1("write", 3), X)), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Functor1("write", 3), Functor1("call", 1))), ListPair(Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", X), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", 1), ListPair(Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Atom.a("fail"), 1)), ListPair(Functor2("type_error", Atom.a("callable"), Functor2(",", Atom.a("fail"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(",", Functor1("write", 3), 1)), ListPair(Functor2("type_error", Atom.a("callable"), Functor2(",", Functor1("write", 3), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("call")):
    for l2 in YP.unify(arg2, ListPair(Functor1("call", Functor2(";", 1, Atom.a("true"))), ListPair(Functor2("type_error", Atom.a("callable"), Functor2(";", 1, Atom.a("true"))), Atom.NIL))):
      yield False
  C = Variable()
  for l1 in YP.unify(arg1, Atom.a("catch-and-throw")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor3("catch", Atom.a("true"), C, Functor1("write", Atom.a("something"))), Functor1("throw", Atom.a("blabla"))), ListPair(Atom.a("system_error"), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  x3 = Variable()
  for l1 in YP.unify(arg1, Atom.a("catch-and-throw")):
    for l2 in YP.unify(arg2, ListPair(Functor3("catch", Functor2("number_chars", A, L), Functor2("error", Atom.a("instantiation_error"), x3), Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Code = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Atom.a("a"), Code), ListPair(ListPair(ListPair(Functor2("<--", Code, 97), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  Char = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Char, 99), ListPair(ListPair(ListPair(Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  Char = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Char, 99), ListPair(ListPair(ListPair(Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Atom.a("b"), 98), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Atom.a("b"), 4), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Code = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Atom.a("ab"), Code), ListPair(Functor2("type_error", Atom.a("character"), Atom.a("ab")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Atom.a("a"), Atom.a("x")), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("x")), Atom.NIL))):
      yield False
  Char = Variable()
  Code = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Char, Code), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  Char = Variable()
  for l1 in YP.unify(arg1, Atom.a("char_code")):
    for l2 in YP.unify(arg2, ListPair(Functor2("char_code", Char, -2), ListPair(Functor1("representation_error", Atom.a("character_code")), Atom.NIL))):
      yield False
  Body = Variable()
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, ListPair(Functor2("clause", Atom.a("x"), Body), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  x1 = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, ListPair(Functor2("clause", x1, B), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  B = Variable()
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, ListPair(Functor2("clause", 4, B), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, ListPair(Functor2("clause", Functor1("f", x1), 5), ListPair(Functor2("type_error", Atom.a("callable"), 5), Atom.NIL))):
      yield False
  x1 = Variable()
  Body = Variable()
  for l1 in YP.unify(arg1, Atom.a("clause")):
    for l2 in YP.unify(arg2, ListPair(Functor2("clause", Functor1("atom", x1), Body), ListPair(Functor3("permission_error", Atom.a("access"), Atom.a("private_procedure"), Functor2("/", Atom.a("atom"), 1)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", 33.3), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", -33.3), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", Functor1("-", Atom.a("a"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", x1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", Atom.a("a")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", Functor1("a", Atom.a("b"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("compound")):
    for l2 in YP.unify(arg2, ListPair(Functor1("compound", ListPair(Atom.a("a"), Atom.NIL)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", X, Y), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", X, 3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", x1, Atom.a("a")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", Functor2("+", Atom.a("a"), X), Functor2("+", X, Atom.a("b"))), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  x1 = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", x1, x2), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("copy_term", Functor2("+", Functor2("+", X, X), Y), Functor2("+", Functor2("+", A, B), B)), Functor2("=", A, 1)), ListPair(ListPair(ListPair(Functor2("<--", B, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", Atom.a("a"), Atom.a("a")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", Atom.a("a"), Atom.a("b")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2("copy_term", Functor1("f", Atom.a("a")), Functor1("f", X)), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("copy_term")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("copy_term", Functor2("+", Atom.a("a"), X), Functor2("+", X, Atom.a("b"))), Functor2("copy_term", Functor2("+", Atom.a("a"), X), Functor2("+", X, Atom.a("b")))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_input")):
    for l2 in YP.unify(arg2, ListPair(Functor1(Atom.a("exists", Atom.a("")), Functor2("/", Atom.a("current_input"), 1)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_output")):
    for l2 in YP.unify(arg2, ListPair(Functor1(Atom.a("exists", Atom.a("")), Functor2("/", Atom.a("current_output"), 1)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair(Functor1("current_predicate", Functor2("/", Atom.a("current_predicate"), 1)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair(Functor1("current_predicate", Functor2("/", Atom.a("score"), 3)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  Name = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair.make([Functor3("functor", Functor1(Atom.a("run_tests", Atom.a("")), 1), Name, x2), Functor1("current_predicate", Functor2("/", Name, 1)), Atom.a("success")])):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair(Functor1("current_predicate", 4), ListPair(Functor2("type_error", Atom.a("predicate_indicator"), 4), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair(Functor1("current_predicate", Atom.a("dog")), ListPair(Functor2("type_error", Atom.a("predicate_indicator"), Atom.a("dog")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_predicate")):
    for l2 in YP.unify(arg2, ListPair(Functor1("current_predicate", Functor2("/", 0, Atom.a("dog"))), ListPair(Functor2("type_error", Atom.a("predicate_indicator"), Functor2("/", 0, Atom.a("dog"))), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("current_prolog_flag", Atom.a("debug"), Atom.a("off")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("warning"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("error"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  V = Variable()
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("current_prolog_flag", Atom.a("debug"), V), ListPair(ListPair(ListPair(Functor2("<--", V, Atom.a("off")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  V = Variable()
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("current_prolog_flag", 5, V), ListPair(Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))):
      yield False
  V = Variable()
  for l1 in YP.unify(arg1, Atom.a("current_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("current_prolog_flag", Atom.a("warning"), V), ListPair(Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("warning")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("cut")):
    for l2 in YP.unify(arg2, ListPair(Atom.a("!"), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("cut")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("cut")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2(",", Functor1("call", Atom.a("!")), Atom.a("fail")), Atom.a("true")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("fail")):
    for l2 in YP.unify(arg2, ListPair(Atom.a("fail"), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("fail")):
    for l2 in YP.unify(arg2, ListPair(Atom.a("undef_pred"), ListPair(Functor2("existence_error", Atom.a("procedure"), Functor2("/", Atom.a("undef_pred"), 0)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("fail")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), Atom.a("undef_pred")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("fail")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Atom.a("undef_pred")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", Functor2("+", X, Y), Functor2("=", X, 1), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(Functor2("+", 1, x4), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Atom.a("fail"), L), ListPair(ListPair(ListPair(Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 1)), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(1, ListPair(1, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor2(";", Functor2("=", X, 2), Functor2("=", X, 1)), ListPair(1, ListPair(2, Atom.NIL))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), ListPair(X, ListPair(Y, Atom.NIL))), ListPair(ListPair(ListPair(Functor2("<--", X, 1), ListPair(Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Goal = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Goal, S), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  X = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, 4, S), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  X = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("findall")):
    for l2 in YP.unify(arg2, ListPair(Functor3("findall", X, Functor1("call", 1), S), ListPair(Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, ListPair(Functor1("float", 3.3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, ListPair(Functor1("float", -3.3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, ListPair(Functor1("float", 3), ListPair(Atom.a("failure"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, ListPair(Functor1("float", Atom.a("atom")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("float")):
    for l2 in YP.unify(arg2, ListPair(Functor1("float", X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), Atom.a("foo"), 3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), X, Y), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("foo")), ListPair(Functor2("<--", Y, 3), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  A = Variable()
  B = Variable()
  C = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, Atom.a("foo"), 3), ListPair(ListPair(ListPair(Functor2("<--", X, Functor3("foo", A, B, C)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, Atom.a("foo"), 0), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  B = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Functor2("mats", A, B), A, B), ListPair(ListPair(ListPair(Functor2("<--", A, Atom.a("mats")), ListPair(Functor2("<--", B, 2), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Functor1("foo", Atom.a("a")), Atom.a("foo"), 2), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Functor1("foo", Atom.a("a")), Atom.a("fo"), 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", 1, X, Y), ListPair(ListPair(ListPair(Functor2("<--", X, 1), ListPair(Functor2("<--", Y, 0), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, 1.1, 0), ListPair(ListPair(ListPair(Functor2("<--", X, 1.1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  x1 = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", ListPair(x1, x2), Atom.a("."), 2), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", Atom.NIL, Atom.NIL, 0), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, Y, 3), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  X = Variable()
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, Atom.a("foo"), N), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", X, Atom.a("foo"), Atom.a("a")), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))):
      yield False
  F = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", F, 1.5, 1), ListPair(Functor2("type_error", Atom.a("atom"), 1.5), Atom.NIL))):
      yield False
  F = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", F, Functor1("foo", Atom.a("a")), 1), ListPair(Functor2("type_error", Atom.a("atomic"), Functor1("foo", Atom.a("a"))), Atom.NIL))):
      yield False
  A = Variable()
  X = Variable()
  T = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("current_prolog_flag", Atom.a("max_arity"), A), Functor2(",", Functor2("is", X, Functor2("+", A, 1)), Functor3("functor", T, Atom.a("foo"), X))), ListPair(Functor1("representation_error", Atom.a("max_arity")), Atom.NIL))):
      yield False
  T = Variable()
  for l1 in YP.unify(arg1, Atom.a("functor")):
    for l2 in YP.unify(arg2, ListPair(Functor3("functor", T, Atom.a("foo"), -1), ListPair(Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Atom.a("true"), Atom.a("true")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Atom.a("true"), Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Atom.a("fail"), Atom.a("true")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Atom.a("true"), Functor2("=", X, 1)), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), Atom.a("true")), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then")):
    for l2 in YP.unify(arg2, ListPair(Functor2("->", Atom.a("true"), Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2))), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("true"), Atom.a("true")), Atom.a("fail")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("true")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("true"), Atom.a("fail")), Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("true"), Functor2("=", X, 1)), Functor2("=", X, 2)), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("fail"), Functor2("=", X, 1)), Functor2("=", X, 2)), ListPair(ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Atom.a("true"), Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2))), Atom.a("true")), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("if-then-else")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("->", Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), Atom.a("true")), Atom.a("true")), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, ListPair(Functor1("integer", 3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, ListPair(Functor1("integer", -3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, ListPair(Functor1("integer", 3.3), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, ListPair(Functor1("integer", X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("integer")):
    for l2 in YP.unify(arg2, ListPair(Functor1("integer", Atom.a("atom")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Result = Variable()
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2("is", Result, Functor2("+", 3, 11.0)), ListPair(ListPair(ListPair(Functor2("<--", Result, 14.0), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("=", X, Functor2("+", 1, 2)), Functor2("is", Y, Functor2("*", X, 3))), ListPair(ListPair(ListPair(Functor2("<--", X, Functor2("+", 1, 2)), ListPair(Functor2("<--", Y, 9), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2("is", Atom.a("foo"), 77), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  N = Variable()
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2("is", 77, N), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2("is", 77, Atom.a("foo")), ListPair(Functor2("type_error", Atom.a("evaluable"), Functor2("/", Atom.a("foo"), 0)), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("is")):
    for l2 in YP.unify(arg2, ListPair(Functor2("is", X, Functor1("float", 3)), ListPair(ListPair(ListPair(Functor2("<--", X, 3.0), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor1("nonvar", 33.3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor1("nonvar", Atom.a("foo")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  Foo = Variable()
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor1("nonvar", Foo), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Foo = Variable()
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("=", Atom.a("foo"), Foo), Functor1("nonvar", Foo)), ListPair(ListPair(ListPair(Functor2("<--", Foo, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor1("nonvar", x1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("nonvar")):
    for l2 in YP.unify(arg2, ListPair(Functor1("nonvar", Functor1("a", Atom.a("b"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", Atom.a("true")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", Atom.a("!")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", Functor2(",", Atom.a("!"), Atom.a("fail"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), Functor1("\\+", Functor2(",", Atom.a("!"), Atom.a("fail")))), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", Functor2("=", 4, 5)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", 3), ListPair(Functor2("type_error", Atom.a("callable"), 3), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_provable")):
    for l2 in YP.unify(arg2, ListPair(Functor1("\\+", X), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", 1, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", X, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", X, Y), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("\\=", X, Y), Functor2("\\=", X, Atom.a("abc"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", Functor2("f", X, Atom.a("def")), Functor2("f", Atom.a("def"), Y)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", 1, 2), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", 1, 1.0), ListPair(Atom.a("success"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", Functor1("g", X), Functor1("f", Functor1("f", X))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", Functor2("f", X, 1), Functor1("f", Functor1("a", X))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("not_unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\=", Functor3("f", X, Y, X), Functor("f", [Functor1("a", X), Functor1("a", Y), Y, 2])), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, ListPair(Functor1("number", 3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, ListPair(Functor1("number", 3.3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, ListPair(Functor1("number", -3), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, ListPair(Functor1("number", Atom.a("a")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("number")):
    for l2 in YP.unify(arg2, ListPair(Functor1("number", X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", 33, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Atom.a("3"), ListPair(Atom.a("3"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", 33, ListPair(Atom.a("3"), ListPair(Atom.a("3"), Atom.NIL))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", 33.0, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair.make([Atom.a("3"), Atom.a("3"), Atom.a("."), Atom.a("0")])), Atom.NIL), Atom.NIL), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", X, ListPair.make([Atom.a("3"), Atom.a("."), Atom.a("3"), Atom.a("E"), Atom.a("+"), Atom.a("0")])), ListPair(ListPair(ListPair(Functor2("<--", X, 3.3), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", 3.3, ListPair.make([Atom.a("3"), Atom.a("."), Atom.a("3")])), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("-"), Atom.a("2"), Atom.a("5")])), ListPair(ListPair(ListPair(Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("\x0A"), Atom.a(" "), Atom.a("3")])), ListPair(ListPair(ListPair(Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair(Atom.a("3"), ListPair(Atom.a("x"), Atom.NIL))), ListPair(Functor1("syntax_error", x2), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("0"), Atom.a("x"), Atom.a("f")])), ListPair(ListPair(ListPair(Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("0"), Atom.a("'"), Atom.a("A")])), ListPair(ListPair(ListPair(Functor2("<--", A, 65), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("4"), Atom.a("."), Atom.a("2")])), ListPair(ListPair(ListPair(Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair.make([Atom.a("4"), Atom.a("2"), Atom.a("."), Atom.a("0"), Atom.a("e"), Atom.a("-"), Atom.a("1")])), ListPair(ListPair(ListPair(Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, L), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", Atom.a("a"), L), ListPair(Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, 4), ListPair(Functor2("type_error", Atom.a("list"), 4), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_chars")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_chars", A, ListPair(Atom.a("4"), ListPair(2, Atom.NIL))), ListPair(Functor2("type_error", Atom.a("character"), 2), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", 33, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(51, ListPair(51, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", 33, ListPair(51, ListPair(51, Atom.NIL))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", 33.0, L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair.make([51, 51, 46, 48])), Atom.NIL), Atom.NIL), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", 33.0, ListPair.make([51, 51, 46, 48])), ListPair(Atom.a("success"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([45, 50, 53])), ListPair(ListPair(ListPair(Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair(32, ListPair(51, Atom.NIL))), ListPair(ListPair(ListPair(Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([48, 120, 102])), ListPair(ListPair(ListPair(Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([48, 39, 97])), ListPair(ListPair(ListPair(Functor2("<--", A, 97), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([52, 46, 50])), ListPair(ListPair(ListPair(Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([52, 50, 46, 48, 101, 45, 49])), ListPair(ListPair(ListPair(Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  A = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, L), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", Atom.a("a"), L), ListPair(Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, 4), ListPair(Functor2("type_error", Atom.a("list"), 4), Atom.NIL))):
      yield False
  A = Variable()
  for l1 in YP.unify(arg1, Atom.a("number_codes")):
    for l2 in YP.unify(arg2, ListPair(Functor2("number_codes", A, ListPair.make([49, 50, Atom.a("3")])), ListPair(Functor1("representation_error", Atom.a("character_code")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor1("once", Atom.a("!")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor1("once", Atom.a("!")), Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2))), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor1("once", Atom.a("repeat")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor1("once", Atom.a("fail")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor1("once", 3), ListPair(Functor2("type_error", Atom.a("callable"), 3), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("once")):
    for l2 in YP.unify(arg2, ListPair(Functor1("once", X), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("or")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Atom.a("true"), Atom.a("fail")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("or")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("or")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Atom.a("!"), Functor1("call", 3)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("or")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2(",", Functor2("=", X, 1), Atom.a("!")), Functor2("=", X, 2)), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("or")):
    for l2 in YP.unify(arg2, ListPair(Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), ListPair(ListPair(Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("repeat")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Atom.a("repeat"), Functor2(",", Atom.a("!"), Atom.a("fail"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("retract")):
    for l2 in YP.unify(arg2, ListPair(Functor1("retract", Functor2(":-", 4, X)), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  x1 = Variable()
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("retract")):
    for l2 in YP.unify(arg2, ListPair(Functor1("retract", Functor2(":-", Functor1("atom", x1), Functor2("==", X, Atom.NIL))), ListPair(Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), Functor2("/", Atom.a("atom"), 1)), Atom.NIL))):
      yield False
  V = Variable()
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), Functor2("current_prolog_flag", Atom.a("unknown"), V)), ListPair(ListPair(ListPair(Functor2("<--", V, Atom.a("fail")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", X, Atom.a("warning")), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", 5, Atom.a("decimals")), ListPair(Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("date"), Atom.a("July 1999")), ListPair(Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("date")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("debug"), Atom.a("no")), ListPair(Functor2("domain_error", Atom.a("flag_value"), Functor2("+", Atom.a("debug"), Atom.a("no"))), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("max_arity"), 40), ListPair(Functor3("permission_error", Atom.a("modify"), Atom.a("flag"), Atom.a("max_arity")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("atom")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("fred")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("chars")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), ListPair(ListPair(ListPair(Functor2("<--", X, ListPair.make([Atom.a("f"), Atom.a("r"), Atom.a("e"), Atom.a("d")])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("codes")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("set_prolog_flag")):
    for l2 in YP.unify(arg2, ListPair(Functor2("read", ListPair.make([34, 102, 114, 101, 100, 34, 46, 32]), X), ListPair(ListPair(ListPair(Functor2("<--", X, ListPair.make([102, 114, 101, 100])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2(";", Functor2("=", X, 1), Functor2("=", X, 2)), X), ListPair(ListPair(ListPair(Functor2("<--", X, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2(";", Functor2("=", X, 2), Functor2("=", X, 1)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2(";", Functor2("=", X, 2), Functor2("=", X, 2)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(2, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Atom.a("fail"), L), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  Y = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", 1, Functor2(";", Functor2("=", Y, 2), Functor2("=", Y, 1)), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(1, Atom.NIL)), ListPair(Functor2("<--", Y, 1), Atom.NIL)), ListPair(ListPair(Functor2("<--", L, ListPair(1, Atom.NIL)), ListPair(Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  L = Variable()
  x4 = Variable()
  x5 = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", Functor2("f", X, Y), Functor2(";", Functor2("=", X, Atom.a("a")), Functor2("=", Y, Atom.a("b"))), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Functor2("f", x4, Atom.a("b")), ListPair(Functor2("f", Atom.a("a"), x5), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2("^", Y, Functor2(";", Functor2(",", Functor2("=", X, 1), Functor2("=", Y, 1)), Functor2(",", Functor2("=", X, 2), Functor2("=", Y, 2)))), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(1, ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2("^", Y, Functor2(";", Functor2(";", Functor2("=", X, 1), Functor2("=", Y, 1)), Functor2(",", Functor2("=", X, 2), Functor2("=", Y, 2)))), S), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair.make([x4, 1, 2])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Functor3("setof", X, Functor2(";", Functor2("^", Y, Functor2(";", Functor2("=", X, 1), Functor2("=", Y, 1))), Functor2("=", X, 3)), S)), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  S = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Functor3("setof", X, Functor2("^", Y, Functor2(";", Functor2("=", X, 1), Functor2(";", Functor2("=", Y, 1), Functor2("=", X, 3)))), S)), ListPair(ListPair(ListPair(Functor2("<--", S, ListPair.make([x4, 1, 3])), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  Z = Variable()
  L = Variable()
  x5 = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2(";", Functor2("=", X, Y), Functor2(";", Functor2("=", X, Z), Functor2("=", Y, 1))), L), ListPair(ListPair(ListPair(Functor2("<--", L, ListPair(Y, ListPair(Z, Atom.NIL))), Atom.NIL), ListPair(ListPair(Functor2("<--", L, ListPair(x5, Atom.NIL)), ListPair(Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, Functor2("^", X, Functor2(";", Atom.a("true"), 4)), L), ListPair(Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))):
      yield False
  X = Variable()
  L = Variable()
  for l1 in YP.unify(arg1, Atom.a("setof")):
    for l2 in YP.unify(arg2, ListPair(Functor3("setof", X, 1, L), ListPair(Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("abracadabra"), 0, 5, x1, S2]), ListPair(ListPair(ListPair(Functor2("<--", S2, Atom.a("abrac")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("abracadabra"), x1, 5, 0, S2]), ListPair(ListPair(ListPair(Functor2("<--", S2, Atom.a("dabra")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  Length = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("abracadabra"), 3, Length, 3, S2]), ListPair(ListPair(ListPair(Functor2("<--", Length, 5), ListPair(Functor2("<--", S2, Atom.a("acada")), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  Before = Variable()
  After = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("abracadabra"), Before, 2, After, Atom.a("ab")]), ListPair(ListPair(ListPair(Functor2("<--", Before, 0), ListPair(Functor2("<--", After, 9), Atom.NIL)), ListPair(ListPair(Functor2("<--", Before, 7), ListPair(Functor2("<--", After, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("Banana"), 3, 2, x1, S2]), ListPair(ListPair(ListPair(Functor2("<--", S2, Atom.a("an")), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  Before = Variable()
  After = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("charity"), Before, 3, After, S2]), ListPair(ListPair.make([ListPair.make([Functor2("<--", Before, 0), Functor2("<--", After, 4), Functor2("<--", S2, Atom.a("cha"))]), ListPair.make([Functor2("<--", Before, 1), Functor2("<--", After, 3), Functor2("<--", S2, Atom.a("har"))]), ListPair.make([Functor2("<--", Before, 2), Functor2("<--", After, 2), Functor2("<--", S2, Atom.a("ari"))]), ListPair.make([Functor2("<--", Before, 3), Functor2("<--", After, 1), Functor2("<--", S2, Atom.a("rit"))]), ListPair.make([Functor2("<--", Before, 4), Functor2("<--", After, 0), Functor2("<--", S2, Atom.a("ity"))])]), Atom.NIL))):
      yield False
  Before = Variable()
  Length = Variable()
  After = Variable()
  Sub_atom = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("ab"), Before, Length, After, Sub_atom]), ListPair(ListPair.make([ListPair.make([Functor2("<--", Before, 0), Functor2("<--", Length, 0), Functor2("<--", After, 2), Functor2("<--", Sub_atom, Atom.a(""))]), ListPair.make([Functor2("<--", Before, 0), Functor2("<--", Length, 1), Functor2("<--", After, 1), Functor2("<--", Sub_atom, Atom.a("a"))]), ListPair.make([Functor2("<--", Before, 0), Functor2("<--", Length, 2), Functor2("<--", After, 0), Functor2("<--", Sub_atom, Atom.a("ab"))]), ListPair.make([Functor2("<--", Before, 1), Functor2("<--", Length, 0), Functor2("<--", After, 1), Functor2("<--", Sub_atom, Atom.a(""))]), ListPair.make([Functor2("<--", Before, 1), Functor2("<--", Length, 1), Functor2("<--", After, 0), Functor2("<--", Sub_atom, Atom.a("b"))]), ListPair.make([Functor2("<--", Before, 2), Functor2("<--", Length, 0), Functor2("<--", After, 0), Functor2("<--", Sub_atom, Atom.a(""))])]), Atom.NIL))):
      yield False
  Banana = Variable()
  x2 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Banana, 3, 2, x2, S2]), ListPair(Atom.a("instantiation_error"), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Functor1("f", Atom.a("a")), 2, 2, x1, S2]), ListPair(Functor2("type_error", Atom.a("atom"), Functor1("f", Atom.a("a"))), Atom.NIL))):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("Banana"), 4, 2, x1, 2]), ListPair(Functor2("type_error", Atom.a("atom"), 2), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("Banana"), Atom.a("a"), 2, x1, S2]), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("Banana"), 4, Atom.a("n"), x1, S2]), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("n")), Atom.NIL))):
      yield False
  x1 = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("sub_atom")):
    for l2 in YP.unify(arg2, ListPair(Functor("sub_atom", [Atom.a("Banana"), 4, x1, Atom.a("m"), S2]), ListPair(Functor2("type_error", Atom.a("integer"), Atom.a("m")), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", 1, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", X, X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", 1, 2), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", X, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", X, Y), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  x1 = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", x1, x2), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", X, Functor1("a", X)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_diff")):
    for l2 in YP.unify(arg2, ListPair(Functor2("\\==", Functor1("f", Atom.a("a")), Functor1("f", Atom.a("a"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", 1, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", X, X), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", 1, 2), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", X, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", X, Y), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  x1 = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", x1, x2), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", X, Functor1("a", X)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_eq")):
    for l2 in YP.unify(arg2, ListPair(Functor2("==", Functor1("f", Atom.a("a")), Functor1("f", Atom.a("a"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", 1.0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", Atom.a("aardvark"), Atom.a("zebra")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", Atom.a("short"), Atom.a("short")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", Atom.a("short"), Atom.a("shorter")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", Functor1("foo", Atom.a("b")), Functor1("foo", Atom.a("a"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", X, X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_gt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>", Functor2("foo", Atom.a("a"), X), Functor2("foo", Atom.a("b"), Y)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", 1.0, 1), ListPair(Atom.a("failure"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", Atom.a("aardvark"), Atom.a("zebra")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", Atom.a("short"), Atom.a("short")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", Atom.a("short"), Atom.a("shorter")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", Functor1("foo", Atom.a("b")), Functor1("foo", Atom.a("a"))), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", X, X), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_gt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@>=", Functor2("foo", Atom.a("a"), X), Functor2("foo", Atom.a("b"), Y)), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", 1.0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", Atom.a("aardvark"), Atom.a("zebra")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", Atom.a("short"), Atom.a("short")), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", Atom.a("short"), Atom.a("shorter")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", Functor1("foo", Atom.a("b")), Functor1("foo", Atom.a("a"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", X, X), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_lt")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@<", Functor2("foo", Atom.a("a"), X), Functor2("foo", Atom.a("b"), Y)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", 1.0, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", Atom.a("aardvark"), Atom.a("zebra")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", Atom.a("short"), Atom.a("short")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", Atom.a("short"), Atom.a("shorter")), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", Functor1("foo", Atom.a("b")), Functor1("foo", Atom.a("a"))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", X, X), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("term_lt=")):
    for l2 in YP.unify(arg2, ListPair(Functor2("@=<", Functor2("foo", Atom.a("a"), X), Functor2("foo", Atom.a("b"), Y)), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("true")):
    for l2 in YP.unify(arg2, ListPair(Atom.a("true"), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", 1, 1), ListPair(Atom.a("success"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", X, 1), ListPair(ListPair(ListPair(Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2(",", Functor2("=", X, Y), Functor2("=", X, Atom.a("abc"))), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("abc")), ListPair(Functor2("<--", Y, Atom.a("abc")), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", Functor2("f", X, Atom.a("def")), Functor2("f", Atom.a("def"), Y)), ListPair(ListPair(ListPair(Functor2("<--", X, Atom.a("def")), ListPair(Functor2("<--", Y, Atom.a("def")), Atom.NIL)), Atom.NIL), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", 1, 2), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", 1, 1.0), ListPair(Atom.a("failure"), Atom.NIL))):
      for l3 in intAndFloatAreDifferent():
        yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", Functor1("g", X), Functor1("f", Functor1("f", X))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", Functor2("f", X, 1), Functor1("f", Functor1("a", X))), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  X = Variable()
  Y = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", Functor3("f", X, Y, X), Functor("f", [Functor1("a", X), Functor1("a", Y), Y, 2])), ListPair(Atom.a("failure"), Atom.NIL))):
      yield False
  A = Variable()
  B = Variable()
  C = Variable()
  D = Variable()
  for l1 in YP.unify(arg1, Atom.a("unify")):
    for l2 in YP.unify(arg2, ListPair(Functor2("=", Functor3("f", A, B, C), Functor3("f", Functor2("g", B, B), Functor2("g", C, C), Functor2("g", D, D))), ListPair(ListPair(ListPair.make([Functor2("<--", A, Functor2("g", Functor2("g", Functor2("g", D, D), Functor2("g", D, D)), Functor2("g", Functor2("g", D, D), Functor2("g", D, D)))), Functor2("<--", B, Functor2("g", Functor2("g", D, D), Functor2("g", D, D))), Functor2("<--", C, Functor2("g", D, D))]), Atom.NIL), Atom.NIL))):
      yield False

main()
