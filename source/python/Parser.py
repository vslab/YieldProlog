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
from FindallAnswers import *
from Functor1 import *
from Functor2 import *
from Functor3 import *
from Functor import *
from ListPair import *
from PrologException import *
from Variable import *

class Parser(object):
    @staticmethod
    def read_term2(Term, Options):
        Answer = Variable()
        Variables = Variable()
        for l1 in Parser.read_termOptions(Options, Variables):
            for l2 in portable_read3(Answer, Variables, Variable()):
                for l3 in remove_pos(Answer, Term):
                    yield False

    @staticmethod
    def read_term3(Input, Term, Options):
        SaveInput = Variable()
        Answer = Variable()
        Variables = Variable()
        for l1 in Parser.read_termOptions(Options, Variables):
            for l2 in YP.current_input(SaveInput):
                try:
                    YP.see(Input)
                    for l3 in portable_read3(Answer, Variables, Variable()):
                        for l4 in remove_pos(Answer, Term):
                            yield False
                finally:
                    YP.see(SaveInput)

    # For read_term, check if Options has variable_names(Variables).
    # Otherwise, ignore Options.
    @staticmethod
    def read_termOptions(Options, Variables):
        Options = YP.getValue(Options)
        if isinstance(Options, Variable):
            raise PrologException(Atom.a("instantiation_error"), "Options is an unbound variable")
        # First try to match Options = [variable_names(Variables)]
        for l1 in YP.unify(Options, ListPair.make(Functor1("variable_names", Variables))):
            yield False
            return

        # Default: Ignore Options.
        yield False

    @staticmethod
    def read1(Term):
        return Parser.read_term2(Term, Atom.NIL)

    @staticmethod
    def read2(Input, Term):
        return Parser.read_term3(Input, Term, Atom.NIL)

def formatError(Output, Format, Arguments):
    # Debug: Simple implementation for now.
    YP.write(Format)
    YP.write(Arguments)
    YP.nl()
    yield False

# Debug: Hand-modify this central predicate to do tail recursion.
def read_tokens(arg1, arg2, arg3):
    repeat = True
    while repeat:
        repeat = False

        for _ in [1]:
            C1 = arg1
            Dict = arg2
            Tokens = arg3
            C2 = Variable()
            if YP.lessThanOrEqual(C1, ListPair(32, Atom.NIL)):
                if YP.greaterThanOrEqual(C1, 0):
                    for l4 in YP.get_code(C2):
#                        for l5 in read_tokens(C2, Dict, Tokens):
#                            yield False
                        arg1 = YP.getValue(C2)
                        arg2 = YP.getValue(Dict)
                        arg3 = YP.getValue(Tokens)
                        repeat = True

                break
            if YP.greaterThanOrEqual(C1, ListPair(97, Atom.NIL)):
                if YP.lessThanOrEqual(C1, ListPair(122, Atom.NIL)):
                    for l4 in read_identifier(C1, Dict, Tokens):
                        yield False

                    break
            if YP.greaterThanOrEqual(C1, ListPair(65, Atom.NIL)):
                if YP.lessThanOrEqual(C1, ListPair(90, Atom.NIL)):
                    for l4 in read_variable(C1, Dict, Tokens):
                        yield False

                    break
            if YP.greaterThanOrEqual(C1, ListPair(48, Atom.NIL)):
                if YP.lessThanOrEqual(C1, ListPair(57, Atom.NIL)):
                    for l4 in read_number(C1, Dict, Tokens):
                        yield False

                    break
            if YP.lessThan(C1, 127):
                for l3 in read_special(C1, Dict, Tokens):
                    yield False

                break
            if YP.lessThanOrEqual(C1, 160):
                for l3 in YP.get_code(C2):
#                    for l4 in read_tokens(C2, Dict, Tokens):
#                        yield False
                    arg1 = YP.getValue(C2)
                    arg2 = YP.getValue(Dict)
                    arg3 = YP.getValue(Tokens)
                    repeat = True

                break
            if YP.greaterThanOrEqual(C1, 223):
                if YP.notEqual(C1, 247):
                    for l4 in read_identifier(C1, Dict, Tokens):
                        yield False

                    break
            if YP.greaterThanOrEqual(C1, 192):
                if YP.notEqual(C1, 215):
                    for l4 in read_variable(C1, Dict, Tokens):
                        yield False

                    break
            if YP.notEqual(C1, 170):
                if YP.notEqual(C1, 186):
                    for l4 in read_symbol(C1, Dict, Tokens):
                        yield False

                    break
            for l2 in read_identifier(C1, Dict, Tokens):
                yield False

# Compiler output follows.

def getDeclaringClass():
  return globals()

def parseInput(TermList):
  TermAndVariables = Variable()
  findallAnswers1 = FindallAnswers(TermAndVariables)
  for l1 in parseInputHelper(TermAndVariables):
    findallAnswers1.add()
  for l1 in findallAnswers1.result(TermList):
    yield False

def parseInputHelper(arg1):
  doBreak = False
  for _ in [1]:
    Term = Variable()
    Variables = Variable()
    Answer = Variable()
    x4 = Variable()
    for l2 in YP.unify(arg1, Functor2("f", Term, Variables)):
      for l3 in YP.repeat():
        for l4 in portable_read3(Answer, Variables, x4):
          for l5 in remove_pos(Answer, Term):
            cutIf1 = False
            for _ in [1]:
              if YP.termEqual(Term, Atom.a("end_of_file")):
                return
                cutIf1 = True
                doBreak = True
                break
              yield False
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

def clear_errors():
  yield False

def remove_pos(arg1, arg2):
  X = Variable()
  for l1 in YP.unify(arg1, X):
    for l2 in YP.unify(arg2, X):
      if YP.var(X):
        yield True
        return
  X = arg2
  _Pos = Variable()
  _Name = Variable()
  for l1 in YP.unify(arg1, Functor3("$VAR", _Pos, _Name, X)):
    if YP.var(X):
      yield True
      return
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield True
      return
  H = Variable()
  T = Variable()
  NH = Variable()
  NT = Variable()
  for l1 in YP.unify(arg1, ListPair(H, T)):
    for l2 in YP.unify(arg2, ListPair(NH, NT)):
      for l3 in remove_pos(H, NH):
        for l4 in remove_pos(T, NT):
          yield False
      return
  A = Variable()
  B = Variable()
  NA = Variable()
  NB = Variable()
  for l1 in YP.unify(arg1, Functor2(",", A, B)):
    for l2 in YP.unify(arg2, Functor2(",", NA, NB)):
      for l3 in remove_pos(A, NA):
        for l4 in remove_pos(B, NB):
          yield False
      return
  Atom_1 = Variable()
  _F = Variable()
  for l1 in YP.unify(arg1, Atom_1):
    for l2 in YP.unify(arg2, Atom_1):
      for l3 in YP.functor(Atom_1, _F, 0):
        yield False
  Term = arg1
  NewTerm = arg2
  Func = Variable()
  _Pos = Variable()
  Args = Variable()
  NArgs = Variable()
  if YP.nonvar(Term):
    for l2 in YP.univ(Term, ListPair(Func, ListPair(_Pos, Args))):
      for l3 in remove_pos(Args, NArgs):
        for l4 in YP.univ(NewTerm, ListPair(Func, NArgs)):
          yield False

def portable_read_position(Term, PosTerm, Syntax):
  for l1 in portable_read(PosTerm, Syntax):
    for l2 in remove_pos(PosTerm, Term):
      yield False

def portable_read(Answer, Syntax):
  Tokens = Variable()
  ParseTokens = Variable()
  for l1 in read_tokens1(Tokens):
    for l2 in remove_comments(Tokens, ParseTokens, Syntax):
      for l3 in parse2(ParseTokens, Answer):
        yield False

def portable_read3(Answer, Variables, Syntax):
  Tokens = Variable()
  ParseTokens = Variable()
  for l1 in read_tokens2(Tokens, Variables):
    for l2 in remove_comments(Tokens, ParseTokens, Syntax):
      for l3 in parse2(ParseTokens, Answer):
        yield False

def remove_comments(arg1, arg2, arg3):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Atom.NIL):
      for l3 in YP.unify(arg3, Atom.NIL):
        yield False
  Ys = arg2
  S = Variable()
  E = Variable()
  Xs = Variable()
  Zs = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("comment", S, E), Xs)):
    for l2 in YP.unify(arg3, ListPair(Functor2("comment", S, E), Zs)):
      for l3 in remove_comments(Xs, Ys, Zs):
        yield False
      return
  Pos = Variable()
  Xs = Variable()
  Ys = Variable()
  Pos2 = Variable()
  Zs = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("/", Atom.a("["), Pos), Xs)):
    for l2 in YP.unify(arg2, ListPair(Atom.a("["), Ys)):
      for l3 in YP.unify(arg3, ListPair(Functor2("list", Pos, Pos2), Zs)):
        for l4 in YP.unify(Pos2, YP.add(Pos, 1)):
          for l5 in remove_comments(Xs, Ys, Zs):
            yield False
        return
  Pos = Variable()
  Xs = Variable()
  Ys = Variable()
  Pos2 = Variable()
  Zs = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("/", Atom.a("]"), Pos), Xs)):
    for l2 in YP.unify(arg2, ListPair(Atom.a("]"), Ys)):
      for l3 in YP.unify(arg3, ListPair(Functor2("list", Pos, Pos2), Zs)):
        for l4 in YP.unify(Pos2, YP.add(Pos, 1)):
          for l5 in remove_comments(Xs, Ys, Zs):
            yield False
        return
  Zs = arg3
  Token = Variable()
  Xs = Variable()
  Ys = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, Xs)):
    for l2 in YP.unify(arg2, ListPair(Token, Ys)):
      for l3 in remove_comments(Xs, Ys, Zs):
        yield False

def expect(Token, arg2, arg3):
  Rest = arg3
  for l1 in YP.unify(arg2, ListPair(Token, Rest)):
    yield True
    return
  S0 = arg2
  x3 = arg3
  for l1 in syntax_error(ListPair.make([Token, Atom.a("or"), Atom.a("operator"), Atom.a("expected")]), S0):
    yield False

def parse2(Tokens, Answer):
  Term = Variable()
  LeftOver = Variable()
  for l1 in clear_errors():
    for l2 in parse(Tokens, 1200, Term, LeftOver):
      for l3 in all_read(LeftOver):
        for l4 in YP.unify(Answer, Term):
          yield False
        return
    for l2 in syntax_error1(Tokens):
      yield False

def all_read(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    yield False
  Token = Variable()
  S = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, S)):
    for l2 in syntax_error(ListPair.make([Atom.a("operator"), Atom.a("expected"), Atom.a("after"), Atom.a("expression")]), ListPair(Token, S)):
      yield False

def parse(arg1, arg2, arg3, arg4):
  x1 = arg2
  x2 = arg3
  x3 = arg4
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in syntax_error(ListPair(Atom.a("expression"), ListPair(Atom.a("expected"), Atom.NIL)), Atom.NIL):
      yield False
  Precedence = arg2
  Term = arg3
  LeftOver = arg4
  Token = Variable()
  RestTokens = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, RestTokens)):
    for l2 in parse5(Token, RestTokens, Precedence, Term, LeftOver):
      yield False

def parse5(arg1, arg2, arg3, arg4, arg5):
  doBreak = False
  for _ in [1]:
    S0 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    for l2 in YP.unify(arg1, Atom.a("}")):
      for l3 in cannot_start(Atom.a("}"), S0):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    for l2 in YP.unify(arg1, Atom.a("]")):
      for l3 in cannot_start(Atom.a("]"), S0):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    for l2 in YP.unify(arg1, Atom.a(")")):
      for l3 in cannot_start(Atom.a(")"), S0):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    for l2 in YP.unify(arg1, Atom.a(",")):
      for l3 in cannot_start(Atom.a(","), S0):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    for l2 in YP.unify(arg1, Atom.a("|")):
      for l3 in cannot_start(Atom.a("|"), S0):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Codes = Variable()
    Term = Variable()
    A = Variable()
    for l2 in YP.unify(arg1, Functor1("string", Codes)):
      cutIf1 = False
      for _ in [1]:
        for l4 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("atom")):
          for l5 in YP.atom_codes(Term, Codes):
            for l6 in exprtl0(S0, Term, Precedence, Answer, S):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        cutIf2 = False
        for _ in [1]:
          for l5 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("chars")):
            for l6 in YP.atom_codes(A, Codes):
              for l7 in YP.atom_chars(A, Term):
                for l8 in exprtl0(S0, Term, Precedence, Answer, S):
                  yield False
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
          for l5 in YP.unify(Term, Codes):
            for l6 in exprtl0(S0, Term, Precedence, Answer, S):
              yield False
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
  for _ in [1]:
    S0 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Number = Variable()
    for l2 in YP.unify(arg1, Functor1("number", Number)):
      for l3 in exprtl0(S0, Number, Precedence, Answer, S):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = Variable()
    for l2 in YP.unify(arg1, Atom.a("[")):
      for l3 in YP.unify(arg2, ListPair(Atom.a("]"), S1)):
        for l4 in read_atom(Functor2("/", Atom.NIL, 0), S1, Precedence, Answer, S):
          yield False
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S1 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Arg1 = Variable()
    S2 = Variable()
    RestArgs = Variable()
    S3 = Variable()
    for l2 in YP.unify(arg1, Atom.a("[")):
      for l3 in parse(S1, 999, Arg1, S2):
        for l4 in read_list(S2, RestArgs, S3):
          for l5 in exprtl0(S3, ListPair(Arg1, RestArgs), Precedence, Answer, S):
            yield False
          if doBreak:
            break
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S1 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Term = Variable()
    S2 = Variable()
    S3 = Variable()
    for l2 in YP.unify(arg1, Atom.a("(")):
      for l3 in parse(S1, 1200, Term, S2):
        for l4 in expect(Atom.a(")"), S2, S3):
          for l5 in exprtl0(S3, Term, Precedence, Answer, S):
            yield False
          if doBreak:
            break
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S1 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Term = Variable()
    S2 = Variable()
    S3 = Variable()
    for l2 in YP.unify(arg1, Atom.a(" (")):
      for l3 in parse(S1, 1200, Term, S2):
        for l4 in expect(Atom.a(")"), S2, S3):
          for l5 in exprtl0(S3, Term, Precedence, Answer, S):
            yield False
          if doBreak:
            break
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Precedence = arg3
    Answer = arg4
    S = arg5
    _Pos = Variable()
    S1 = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Atom.a("{"), _Pos)):
      for l3 in YP.unify(arg2, ListPair(Atom.a("}"), S1)):
        for l4 in read_atom(Atom.a("{}"), S1, Precedence, Answer, S):
          yield False
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S1 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Pos = Variable()
    Term = Variable()
    S2 = Variable()
    S3 = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Atom.a("{"), Pos)):
      for l3 in parse(S1, 1200, Term, S2):
        for l4 in expect(Atom.a("}"), S2, S3):
          for l5 in exprtl0(S3, Functor2("{}", Pos, Term), Precedence, Answer, S):
            yield False
          if doBreak:
            break
          return
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Precedence = arg3
    Answer = arg4
    S = arg5
    Variable_1 = Variable()
    Name = Variable()
    Pos = Variable()
    S1 = Variable()
    Arg1 = Variable()
    S2 = Variable()
    RestArgs = Variable()
    S3 = Variable()
    Term = Variable()
    for l2 in YP.unify(arg1, Functor3("var", Variable_1, Name, Pos)):
      for l3 in YP.unify(arg2, ListPair(Atom.a("("), S1)):
        for l4 in parse(S1, 999, Arg1, S2):
          for l5 in read_args(S2, RestArgs, S3):
            for l6 in YP.univ(Term, ListPair(Atom.a("call"), ListPair(Functor3("$VAR", Pos, Name, Variable_1), ListPair(Arg1, RestArgs)))):
              for l7 in exprtl0(S3, Term, Precedence, Answer, S):
                yield False
              if doBreak:
                break
            if doBreak:
              break
            return
          if doBreak:
            break
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Variable_1 = Variable()
    Name = Variable()
    Pos = Variable()
    for l2 in YP.unify(arg1, Functor3("var", Variable_1, Name, Pos)):
      for l3 in exprtl0(S0, Functor3("$VAR", Pos, Name, Variable_1), Precedence, Answer, S):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    Atom_1 = Variable()
    P = Variable()
    for l2 in YP.unify(arg1, Functor2("atom", Atom_1, P)):
      for l3 in read_atom(Functor2("/", Atom_1, P), S0, Precedence, Answer, S):
        yield False
      if doBreak:
        break
    if doBreak:
      break

def read_atom(arg1, arg2, Precedence, Answer, S):
  doBreak = False
  for _ in [1]:
    _Pos = Variable()
    Number = Variable()
    S1 = Variable()
    Negative = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Atom.a("-"), _Pos)):
      for l3 in YP.unify(arg2, ListPair(Functor1("number", Number), S1)):
        for l4 in YP.unify(Negative, YP.negate(Number)):
          for l5 in exprtl0(S1, Negative, Precedence, Answer, S):
            yield False
          if doBreak:
            break
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Functor_1 = Variable()
    Pos = Variable()
    S1 = Variable()
    Arg1 = Variable()
    S2 = Variable()
    RestArgs = Variable()
    S3 = Variable()
    Term = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Functor_1, Pos)):
      for l3 in YP.unify(arg2, ListPair(Atom.a("("), S1)):
        for l4 in parse(S1, 999, Arg1, S2):
          for l5 in read_args(S2, RestArgs, S3):
            for l6 in YP.univ(Term, ListPair(Functor_1, ListPair(Pos, ListPair(Arg1, RestArgs)))):
              for l7 in exprtl0(S3, Term, Precedence, Answer, S):
                yield False
              if doBreak:
                break
            if doBreak:
              break
            return
          if doBreak:
            break
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    Op = Variable()
    Pos = Variable()
    Oprec = Variable()
    Aprec = Variable()
    Flag = Variable()
    Term = Variable()
    Arg = Variable()
    S1 = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Op, Pos)):
      for l3 in prefixop(Op, Oprec, Aprec):
        for l4 in possible_right_operand(S0, Flag):
          cutIf1 = False
          for _ in [1]:
            if YP.lessThan(Flag, 0):
              for l7 in YP.univ(Term, ListPair(Op, ListPair(Pos, Atom.NIL))):
                for l8 in exprtl0(S0, Term, Precedence, Answer, S):
                  yield False
                if doBreak:
                  break
              if doBreak:
                break
              cutIf1 = True
              doBreak = True
              break
            cutIf2 = False
            for _ in [1]:
              if YP.greaterThan(Oprec, Precedence):
                for l8 in syntax_error(ListPair.make([Atom.a("prefix"), Atom.a("operator"), Op, Atom.a("in"), Atom.a("context"), Atom.a("with"), Atom.a("precedence"), Precedence]), S0):
                  yield False
                if doBreak:
                  break
                cutIf2 = True
                doBreak = True
                break
              cutIf3 = False
              for _ in [1]:
                if YP.greaterThan(Flag, 0):
                  for l9 in parse(S0, Aprec, Arg, S1):
                    for l10 in YP.univ(Term, ListPair.make([Op, Pos, Arg])):
                      for l11 in exprtl(S1, Oprec, Term, Precedence, Answer, S):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                    return
                  if doBreak:
                    break
                  cutIf3 = True
                  doBreak = True
                  break
                for l8 in peepop(S0, S1):
                  for l9 in prefix_is_atom(S1, Oprec):
                    for l10 in exprtl(S1, Oprec, Functor2("/", Op, Pos), Precedence, Answer, S):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                if doBreak:
                  break
                for l8 in parse(S0, Aprec, Arg, S1):
                  for l9 in YP.univ(Term, ListPair.make([Op, Pos, Arg])):
                    for l10 in exprtl(S1, Oprec, Term, Precedence, Answer, S):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  return
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
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    S0 = arg2
    Atom_1 = Variable()
    Pos = Variable()
    Term = Variable()
    for l2 in YP.unify(arg1, Functor2("/", Atom_1, Pos)):
      for l3 in YP.univ(Term, ListPair(Atom_1, ListPair(Pos, Atom.NIL))):
        for l4 in exprtl0(S0, Term, Precedence, Answer, S):
          yield False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def cannot_start(Token, S0):
  for l1 in syntax_error(ListPair.make([Token, Atom.a("cannot"), Atom.a("start"), Atom.a("an"), Atom.a("expression")]), S0):
    yield False

def read_args(arg1, arg2, arg3):
  S = arg3
  S1 = Variable()
  Term = Variable()
  Rest = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, ListPair(Atom.a(","), S1)):
    for l2 in YP.unify(arg2, ListPair(Term, Rest)):
      for l3 in parse(S1, 999, Term, S2):
        for l4 in read_args(S2, Rest, S):
          yield False
        return
      return
  S = arg3
  for l1 in YP.unify(arg1, ListPair(Atom.a(")"), S)):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield True
      return
  S = arg1
  x2 = arg2
  x3 = arg3
  for l1 in syntax_error(ListPair.make([Atom.a(", or )"), Atom.a("expected"), Atom.a("in"), Atom.a("arguments")]), S):
    yield False

def read_list(arg1, arg2, arg3):
  x1 = arg2
  x2 = arg3
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), Atom.NIL):
      yield False
  Rest = arg2
  S = arg3
  Token = Variable()
  S1 = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, S1)):
    for l2 in read_list4(Token, S1, Rest, S):
      yield False

def read_list4(arg1, arg2, arg3, arg4):
  S1 = arg2
  S = arg4
  Term = Variable()
  Rest = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a(",")):
    for l2 in YP.unify(arg3, ListPair(Term, Rest)):
      for l3 in parse(S1, 999, Term, S2):
        for l4 in read_list(S2, Rest, S):
          yield False
        return
      return
  S1 = arg2
  Rest = arg3
  S = arg4
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("|")):
    for l2 in parse(S1, 999, Rest, S2):
      for l3 in expect(Atom.a("]"), S2, S):
        yield False
      return
    return
  S1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("]")):
    for l2 in YP.unify(arg2, S1):
      for l3 in YP.unify(arg3, Atom.NIL):
        for l4 in YP.unify(arg4, S1):
          yield True
          return
  Token = arg1
  S1 = arg2
  x3 = arg3
  x4 = arg4
  for l1 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), ListPair(Token, S1)):
    yield False

def possible_right_operand(arg1, arg2):
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, -1):
      yield False
  Flag = arg2
  H = Variable()
  T = Variable()
  for l1 in YP.unify(arg1, ListPair(H, T)):
    for l2 in possible_right_operand3(H, Flag, T):
      yield False

def possible_right_operand3(arg1, arg2, arg3):
  x4 = arg3
  x1 = Variable()
  x2 = Variable()
  x3 = Variable()
  for l1 in YP.unify(arg1, Functor3("var", x1, x2, x3)):
    for l2 in YP.unify(arg2, 1):
      yield False
  x2 = arg3
  x1 = Variable()
  for l1 in YP.unify(arg1, Functor1("number", x1)):
    for l2 in YP.unify(arg2, 1):
      yield False
  x2 = arg3
  x1 = Variable()
  for l1 in YP.unify(arg1, Functor1("string", x1)):
    for l2 in YP.unify(arg2, 1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a(" (")):
    for l2 in YP.unify(arg2, 1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("(")):
    for l2 in YP.unify(arg2, 0):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a(")")):
    for l2 in YP.unify(arg2, -1):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("[")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, ListPair(Atom.a("]"), x1)):
        yield True
        return
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("[")):
    for l2 in YP.unify(arg2, 1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("]")):
    for l2 in YP.unify(arg2, -1):
      yield False
  x1 = Variable()
  for l1 in YP.unify(arg1, Atom.a("{")):
    for l2 in YP.unify(arg2, 0):
      for l3 in YP.unify(arg3, ListPair(Atom.a("}"), x1)):
        yield True
        return
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("{")):
    for l2 in YP.unify(arg2, 1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("}")):
    for l2 in YP.unify(arg2, -1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a(",")):
    for l2 in YP.unify(arg2, -1):
      yield False
  x1 = arg3
  for l1 in YP.unify(arg1, Atom.a("|")):
    for l2 in YP.unify(arg2, -1):
      yield False
  x3 = arg3
  x1 = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, Functor2("atom", x1, x2)):
    for l2 in YP.unify(arg2, 0):
      yield False

def peepop(arg1, arg2):
  F = Variable()
  Pos = Variable()
  S1 = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("atom", F, Pos), ListPair(Atom.a("("), S1))):
    for l2 in YP.unify(arg2, ListPair(Functor2("atom", F, Pos), ListPair(Atom.a("("), S1))):
      yield True
      return
  F = Variable()
  Pos = Variable()
  S1 = Variable()
  L = Variable()
  P = Variable()
  R = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("atom", F, Pos), S1)):
    for l2 in YP.unify(arg2, ListPair(Functor(Atom.a("infixop", Atom.a("")), [Functor2("/", F, Pos), L, P, R]), S1)):
      for l3 in infixop(F, L, P, R):
        yield False
  F = Variable()
  Pos = Variable()
  S1 = Variable()
  L = Variable()
  P = Variable()
  for l1 in YP.unify(arg1, ListPair(Functor2("atom", F, Pos), S1)):
    for l2 in YP.unify(arg2, ListPair(Functor3(Atom.a("postfixop", Atom.a("")), Functor2("/", F, Pos), L, P), S1)):
      for l3 in postfixop(F, L, P):
        yield False
  S0 = Variable()
  for l1 in YP.unify(arg1, S0):
    for l2 in YP.unify(arg2, S0):
      yield False

def prefix_is_atom(arg1, arg2):
  Precedence = arg2
  Token = Variable()
  x2 = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, x2)):
    for l2 in prefix_is_atom(Token, Precedence):
      yield False
  P = arg2
  x1 = Variable()
  L = Variable()
  x3 = Variable()
  x4 = Variable()
  for l1 in YP.unify(arg1, Functor(Atom.a("infixop", Atom.a("")), [x1, L, x3, x4])):
    if YP.greaterThanOrEqual(L, P):
      yield False
  P = arg2
  x1 = Variable()
  L = Variable()
  x3 = Variable()
  for l1 in YP.unify(arg1, Functor3(Atom.a("postfixop", Atom.a("")), x1, L, x3)):
    if YP.greaterThanOrEqual(L, P):
      yield False
  x1 = arg2
  for l1 in YP.unify(arg1, Atom.a(")")):
    yield False
  x1 = arg2
  for l1 in YP.unify(arg1, Atom.a("]")):
    yield False
  x1 = arg2
  for l1 in YP.unify(arg1, Atom.a("}")):
    yield False
  P = arg2
  for l1 in YP.unify(arg1, Atom.a("|")):
    if YP.greaterThanOrEqual(1100, P):
      yield False
  P = arg2
  for l1 in YP.unify(arg1, Atom.a(",")):
    if YP.greaterThanOrEqual(1000, P):
      yield False
  x1 = arg2
  for l1 in YP.unify(arg1, Atom.NIL):
    yield False

def exprtl0(arg1, arg2, arg3, arg4, arg5):
  x2 = arg3
  Term = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, Term):
      for l3 in YP.unify(arg4, Term):
        for l4 in YP.unify(arg5, Atom.NIL):
          yield False
  Term = arg2
  Precedence = arg3
  Answer = arg4
  S = arg5
  Token = Variable()
  S1 = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, S1)):
    for l2 in exprtl0_6(Token, Term, Precedence, Answer, S, S1):
      yield False

def exprtl0_6(arg1, arg2, arg3, arg4, arg5, arg6):
  doBreak = False
  for _ in [1]:
    x2 = arg3
    S1 = arg6
    Term = Variable()
    for l2 in YP.unify(arg1, Atom.a("}")):
      for l3 in YP.unify(arg2, Term):
        for l4 in YP.unify(arg4, Term):
          for l5 in YP.unify(arg5, ListPair(Atom.a("}"), S1)):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x2 = arg3
    S1 = arg6
    Term = Variable()
    for l2 in YP.unify(arg1, Atom.a("]")):
      for l3 in YP.unify(arg2, Term):
        for l4 in YP.unify(arg4, Term):
          for l5 in YP.unify(arg5, ListPair(Atom.a("]"), S1)):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x2 = arg3
    S1 = arg6
    Term = Variable()
    for l2 in YP.unify(arg1, Atom.a(")")):
      for l3 in YP.unify(arg2, Term):
        for l4 in YP.unify(arg4, Term):
          for l5 in YP.unify(arg5, ListPair(Atom.a(")"), S1)):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = arg6
    Next = Variable()
    S2 = Variable()
    for l2 in YP.unify(arg1, Atom.a(",")):
      cutIf1 = False
      for _ in [1]:
        if YP.greaterThanOrEqual(Precedence, 1000):
          for l5 in parse(S1, 1000, Next, S2):
            for l6 in exprtl(S2, 1000, Functor2(",", Term, Next), Precedence, Answer, S):
              yield False
            if doBreak:
              break
            return
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        for l4 in YP.unify(Answer, Term):
          for l5 in YP.unify(S, ListPair(Atom.a(","), S1)):
            yield False
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
  for _ in [1]:
    Term = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = arg6
    Next = Variable()
    S2 = Variable()
    for l2 in YP.unify(arg1, Atom.a("|")):
      cutIf2 = False
      for _ in [1]:
        if YP.greaterThanOrEqual(Precedence, 1100):
          for l5 in parse(S1, 1100, Next, S2):
            for l6 in exprtl(S2, 1100, Functor2(";", Term, Next), Precedence, Answer, S):
              yield False
            if doBreak:
              break
            return
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
          break
        for l4 in YP.unify(Answer, Term):
          for l5 in YP.unify(S, ListPair(Atom.a("|"), S1)):
            yield False
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
    x2 = arg2
    x3 = arg3
    x4 = arg4
    x5 = arg5
    S1 = arg6
    S = Variable()
    for l2 in YP.unify(arg1, Functor1("string", S)):
      for l3 in cannot_follow(Atom.a("chars"), Functor1("string", S), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x2 = arg2
    x3 = arg3
    x4 = arg4
    x5 = arg5
    S1 = arg6
    N = Variable()
    for l2 in YP.unify(arg1, Functor1("number", N)):
      for l3 in cannot_follow(Atom.a("number"), Functor1("number", N), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = Variable()
    for l2 in YP.unify(arg1, Atom.a("{")):
      for l3 in YP.unify(arg6, ListPair(Atom.a("}"), S1)):
        for l4 in exprtl0_atom(Atom.a("{}"), Term, Precedence, Answer, S, S1):
          yield False
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x1 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    S1 = arg6
    for l2 in YP.unify(arg1, Atom.a("{")):
      for l3 in cannot_follow(Atom.a("brace"), Atom.a("{"), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = Variable()
    for l2 in YP.unify(arg1, Atom.a("[")):
      for l3 in YP.unify(arg6, ListPair(Atom.a("]"), S1)):
        for l4 in exprtl0_atom(Atom.NIL, Term, Precedence, Answer, S, S1):
          yield False
        if doBreak:
          break
        return
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x1 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    S1 = arg6
    for l2 in YP.unify(arg1, Atom.a("[")):
      for l3 in cannot_follow(Atom.a("bracket"), Atom.a("["), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x1 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    S1 = arg6
    for l2 in YP.unify(arg1, Atom.a("(")):
      for l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x1 = arg2
    x2 = arg3
    x3 = arg4
    x4 = arg5
    S1 = arg6
    for l2 in YP.unify(arg1, Atom.a(" (")):
      for l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    x4 = arg2
    x5 = arg3
    x6 = arg4
    x7 = arg5
    S1 = arg6
    A = Variable()
    B = Variable()
    P = Variable()
    for l2 in YP.unify(arg1, Functor3("var", A, B, P)):
      for l3 in cannot_follow(Atom.a("variable"), Functor3("var", A, B, P), S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Term = arg2
    Precedence = arg3
    Answer = arg4
    S = arg5
    S1 = arg6
    F = Variable()
    P = Variable()
    for l2 in YP.unify(arg1, Functor2("atom", F, P)):
      for l3 in exprtl0_atom(Functor2("/", F, P), Term, Precedence, Answer, S, S1):
        yield False
      if doBreak:
        break
    if doBreak:
      break

def exprtl0_atom(arg1, arg2, arg3, arg4, arg5, S1):
  Term = arg2
  Precedence = arg3
  Answer = arg4
  S = arg5
  F = Variable()
  Pos = Variable()
  L1 = Variable()
  O1 = Variable()
  R1 = Variable()
  L2 = Variable()
  O2 = Variable()
  for l1 in YP.unify(arg1, Functor2("/", F, Pos)):
    for l2 in ambigop(F, Precedence, L1, O1, R1, L2, O2):
      for l3 in prefix_is_atom(S1, Precedence):
        for l4 in exprtl(ListPair(Functor3(Atom.a("postfixop", Atom.a("")), Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S):
          yield False
        return
      for l3 in exprtl(ListPair(Functor(Atom.a("infixop", Atom.a("")), [Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S):
        yield False
      for l3 in exprtl(ListPair(Functor3(Atom.a("postfixop", Atom.a("")), Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S):
        yield False
      return
  Term = arg2
  Precedence = arg3
  Answer = arg4
  S = arg5
  F = Variable()
  Pos = Variable()
  L1 = Variable()
  O1 = Variable()
  R1 = Variable()
  for l1 in YP.unify(arg1, Functor2("/", F, Pos)):
    for l2 in infixop(F, L1, O1, R1):
      for l3 in exprtl(ListPair(Functor(Atom.a("infixop", Atom.a("")), [Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S):
        yield False
      return
  Term = arg2
  Precedence = arg3
  Answer = arg4
  S = arg5
  F = Variable()
  Pos = Variable()
  L2 = Variable()
  O2 = Variable()
  for l1 in YP.unify(arg1, Functor2("/", F, Pos)):
    for l2 in postfixop(F, L2, O2):
      for l3 in exprtl(ListPair(Functor3(Atom.a("postfixop", Atom.a("")), Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S):
        yield False
      return
  X = arg1
  x2 = arg2
  x3 = arg3
  x4 = arg4
  x5 = arg5
  x7 = Variable()
  for l1 in syntax_error(ListPair.make([Functor2("-", Atom.a("non"), Atom.a("operator")), X, Atom.a("follows"), Atom.a("expression")]), ListPair(Functor2("atom", X, x7), S1)):
    yield False
  return

def cannot_follow(Type, Token, Tokens):
  for l1 in syntax_error(ListPair.make([Type, Atom.a("follows"), Atom.a("expression")]), ListPair(Token, Tokens)):
    yield False

def exprtl(arg1, arg2, arg3, arg4, arg5, arg6):
  x1 = arg2
  x3 = arg4
  Term = Variable()
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg3, Term):
      for l3 in YP.unify(arg5, Term):
        for l4 in YP.unify(arg6, Atom.NIL):
          yield False
  C = arg2
  Term = arg3
  Precedence = arg4
  Answer = arg5
  S = arg6
  Token = Variable()
  Tokens = Variable()
  for l1 in YP.unify(arg1, ListPair(Token, Tokens)):
    for l2 in exprtl_7(Token, C, Term, Precedence, Answer, S, Tokens):
      yield False

def exprtl_7(arg1, arg2, arg3, arg4, arg5, arg6, arg7):
  C = arg2
  Term = arg3
  Precedence = arg4
  Answer = arg5
  S = arg6
  S1 = arg7
  F = Variable()
  Pos = Variable()
  L = Variable()
  O = Variable()
  R = Variable()
  Other = Variable()
  S2 = Variable()
  Expr = Variable()
  for l1 in YP.unify(arg1, Functor(Atom.a("infixop", Atom.a("")), [Functor2("/", F, Pos), L, O, R])):
    if YP.greaterThanOrEqual(Precedence, O):
      if YP.lessThanOrEqual(C, L):
        for l4 in parse(S1, R, Other, S2):
          for l5 in YP.univ(Expr, ListPair.make([F, Pos, Term, Other])):
            for l6 in exprtl(S2, O, Expr, Precedence, Answer, S):
              yield False
        return
  C = arg2
  Term = arg3
  Precedence = arg4
  Answer = arg5
  S = arg6
  S1 = arg7
  F = Variable()
  Pos = Variable()
  L = Variable()
  O = Variable()
  Expr = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Functor3(Atom.a("postfixop", Atom.a("")), Functor2("/", F, Pos), L, O)):
    if YP.greaterThanOrEqual(Precedence, O):
      if YP.lessThanOrEqual(C, L):
        for l4 in YP.univ(Expr, ListPair.make([F, Pos, Term])):
          for l5 in peepop(S1, S2):
            for l6 in exprtl(S2, O, Expr, Precedence, Answer, S):
              yield False
        return
  C = arg2
  Term = arg3
  Precedence = arg4
  Answer = arg5
  S = arg6
  S1 = arg7
  Next = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a(",")):
    if YP.greaterThanOrEqual(Precedence, 1000):
      if YP.lessThan(C, 1000):
        for l4 in parse(S1, 1000, Next, S2):
          for l5 in exprtl(S2, 1000, Functor2(",", Term, Next), Precedence, Answer, S):
            yield False
        return
  C = arg2
  Term = arg3
  Precedence = arg4
  Answer = arg5
  S = arg6
  S1 = arg7
  Next = Variable()
  S2 = Variable()
  for l1 in YP.unify(arg1, Atom.a("|")):
    if YP.greaterThanOrEqual(Precedence, 1100):
      if YP.lessThan(C, 1100):
        for l4 in parse(S1, 1100, Next, S2):
          for l5 in exprtl(S2, 1100, Functor2(";", Term, Next), Precedence, Answer, S):
            yield False
        return
  Token = arg1
  x2 = arg2
  x4 = arg4
  Tokens = arg7
  Term = Variable()
  for l1 in YP.unify(arg3, Term):
    for l2 in YP.unify(arg5, Term):
      for l3 in YP.unify(arg6, ListPair(Token, Tokens)):
        yield False

def syntax_error(_Message, _List):
  return
  for l1 in YP.fail():
    yield False

def syntax_error1(_List):
  return
  for l1 in YP.fail():
    yield False

def prefixop(F, O, Q):
  doBreak = False
  for _ in [1]:
    cutIf1 = False
    for _ in [1]:
      for l3 in YP.current_op(O, Atom.a("fx"), F):
        for l4 in YP.unify(Q, YP.subtract(O, 1)):
          yield False
        if doBreak:
          break
        cutIf1 = True
        doBreak = True
        break
      if doBreak:
        break
      cutIf2 = False
      for _ in [1]:
        for l4 in YP.current_op(O, Atom.a("fy"), F):
          for l5 in YP.unify(Q, O):
            yield False
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
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

def postfixop(F, P, O):
  doBreak = False
  for _ in [1]:
    cutIf1 = False
    for _ in [1]:
      for l3 in YP.current_op(O, Atom.a("xf"), F):
        for l4 in YP.unify(P, YP.subtract(O, 1)):
          yield False
        if doBreak:
          break
        cutIf1 = True
        doBreak = True
        break
      if doBreak:
        break
      cutIf2 = False
      for _ in [1]:
        for l4 in YP.current_op(O, Atom.a("yf"), F):
          for l5 in YP.unify(P, O):
            yield False
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
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

def infixop(F, P, O, Q):
  doBreak = False
  for _ in [1]:
    cutIf1 = False
    for _ in [1]:
      for l3 in YP.current_op(O, Atom.a("xfy"), F):
        for l4 in YP.unify(P, YP.subtract(O, 1)):
          for l5 in YP.unify(Q, O):
            yield False
          if doBreak:
            break
        if doBreak:
          break
        cutIf1 = True
        doBreak = True
        break
      if doBreak:
        break
      cutIf2 = False
      for _ in [1]:
        for l4 in YP.current_op(O, Atom.a("xfx"), F):
          for l5 in YP.unify(P, YP.subtract(O, 1)):
            for l6 in YP.unify(Q, P):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
          break
        if doBreak:
          break
        cutIf3 = False
        for _ in [1]:
          for l5 in YP.current_op(O, Atom.a("yfx"), F):
            for l6 in YP.unify(Q, YP.subtract(O, 1)):
              for l7 in YP.unify(P, O):
                yield False
              if doBreak:
                break
            if doBreak:
              break
            cutIf3 = True
            doBreak = True
            break
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

def ambigop(F, Precedence, L1, O1, R1, L2, O2):
  for l1 in postfixop(F, L2, O2):
    if YP.lessThanOrEqual(O2, Precedence):
      for l3 in infixop(F, L1, O1, R1):
        if YP.lessThanOrEqual(O1, Precedence):
          yield False

def read_tokens1(arg1):
  TokenList = arg1
  C1 = Variable()
  _X = Variable()
  ListOfTokens = Variable()
  for l1 in YP.get_code(C1):
    for l2 in read_tokens(C1, _X, ListOfTokens):
      for l3 in YP.unify(TokenList, ListOfTokens):
        yield False
      return
  for l1 in YP.unify(arg1, ListPair(Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL)):
    yield False

def read_tokens2(arg1, arg2):
  TokenList = arg1
  Dictionary = arg2
  C1 = Variable()
  Dict = Variable()
  ListOfTokens = Variable()
  for l1 in YP.get_code(C1):
    for l2 in read_tokens(C1, Dict, ListOfTokens):
      for l3 in terminate_list(Dict):
        for l4 in YP.unify(Dictionary, Dict):
          for l5 in YP.unify(TokenList, ListOfTokens):
            yield False
        return
  for l1 in YP.unify(arg1, ListPair(Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL)):
    for l2 in YP.unify(arg2, Atom.NIL):
      yield False

def terminate_list(arg1):
  for l1 in YP.unify(arg1, Atom.NIL):
    yield False
  x1 = Variable()
  Tail = Variable()
  for l1 in YP.unify(arg1, ListPair(x1, Tail)):
    for l2 in terminate_list(Tail):
      yield False

def read_special(arg1, Dict, arg3):
  doBreak = False
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 95):
      for l3 in read_variable(95, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 247):
      for l3 in read_symbol(247, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 215):
      for l3 in read_symbol(215, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    StartPos = Variable()
    EndPos = Variable()
    Tokens = Variable()
    Ch = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 37):
      for l3 in YP.unify(arg3, ListPair(Functor2("comment", StartPos, EndPos), Tokens)):
        for l4 in get_current_position(StartPos):
          for l5 in YP.repeat():
            for l6 in YP.get_code(Ch):
              if YP.lessThan(Ch, ListPair(32, Atom.NIL)):
                if YP.notEqual(Ch, 9):
                  if YP.termNotEqual(Ch, -1):
                    for l10 in get_current_position(EndPos):
                      for l11 in YP.get_code(NextCh):
                        for l12 in read_tokens(NextCh, Dict, Tokens):
                          yield False
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if doBreak:
                      break
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
    T = arg3
    C2 = Variable()
    StartPos = Variable()
    EndPos = Variable()
    Tokens = Variable()
    StartPos1 = Variable()
    NextCh = Variable()
    Chars = Variable()
    for l2 in YP.unify(arg1, 47):
      for l3 in YP.get_code(C2):
        cutIf1 = False
        for _ in [1]:
          if YP.equal(C2, ListPair(42, Atom.NIL)):
            for l6 in YP.unify(T, ListPair(Functor2("comment", StartPos, EndPos), Tokens)):
              for l7 in get_current_position(StartPos1):
                for l8 in YP.unify(StartPos, YP.subtract(StartPos1, 1)):
                  for l9 in read_solidus(32, NextCh):
                    for l10 in get_current_position(EndPos):
                      for l11 in read_tokens(NextCh, Dict, Tokens):
                        yield False
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
            cutIf1 = True
            doBreak = True
            break
          for l5 in YP.unify(T, Tokens):
            for l6 in rest_symbol(C2, Chars, NextCh):
              for l7 in read_after_atom4(NextCh, Dict, Tokens, ListPair(47, Chars)):
                yield False
              if doBreak:
                break
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
    Pos = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 33):
      for l3 in YP.unify(arg3, ListPair(Functor2("atom", Atom.a("!"), Pos), Tokens)):
        for l4 in get_current_position(Pos):
          for l5 in YP.get_code(NextCh):
            for l6 in read_after_atom(NextCh, Dict, Tokens):
              yield False
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
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 40):
      for l3 in YP.unify(arg3, ListPair(Atom.a(" ("), Tokens)):
        for l4 in YP.get_code(NextCh):
          for l5 in read_tokens(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 41):
      for l3 in YP.unify(arg3, ListPair(Atom.a(")"), Tokens)):
        for l4 in YP.get_code(NextCh):
          for l5 in read_tokens(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 44):
      for l3 in YP.unify(arg3, ListPair(Atom.a(","), Tokens)):
        for l4 in YP.get_code(NextCh):
          for l5 in read_tokens(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Pos = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 59):
      for l3 in YP.unify(arg3, ListPair(Functor2("atom", Atom.a(";"), Pos), Tokens)):
        for l4 in get_current_position(Pos):
          for l5 in YP.get_code(NextCh):
            for l6 in read_after_atom(NextCh, Dict, Tokens):
              yield False
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
    Pos = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 91):
      for l3 in YP.unify(arg3, ListPair(Functor2("/", Atom.a("["), Pos), Tokens)):
        for l4 in get_current_position(Pos):
          for l5 in YP.get_code(NextCh):
            for l6 in read_tokens(NextCh, Dict, Tokens):
              yield False
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
    Pos = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 93):
      for l3 in YP.unify(arg3, ListPair(Functor2("/", Atom.a("]"), Pos), Tokens)):
        for l4 in get_current_position(Pos):
          for l5 in YP.get_code(NextCh):
            for l6 in read_after_atom(NextCh, Dict, Tokens):
              yield False
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
    Pos = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 123):
      for l3 in YP.unify(arg3, ListPair(Functor2("/", Atom.a("{"), Pos), Tokens)):
        for l4 in get_current_position(Pos):
          for l5 in YP.get_code(NextCh):
            for l6 in read_tokens(NextCh, Dict, Tokens):
              yield False
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
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 124):
      for l3 in YP.unify(arg3, ListPair(Atom.a("|"), Tokens)):
        for l4 in YP.get_code(NextCh):
          for l5 in read_tokens(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 125):
      for l3 in YP.unify(arg3, ListPair(Atom.a("}"), Tokens)):
        for l4 in YP.get_code(NextCh):
          for l5 in read_after_atom(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    NextCh = Variable()
    for l2 in YP.unify(arg1, 46):
      for l3 in YP.get_code(NextCh):
        for l4 in read_fullstop(NextCh, Dict, Tokens):
          yield False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Chars = Variable()
    Tokens = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 34):
      for l3 in YP.unify(arg3, ListPair(Functor1("string", Chars), Tokens)):
        for l4 in read_string(Chars, 34, NextCh):
          for l5 in read_tokens(NextCh, Dict, Tokens):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    Chars = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg1, 39):
      for l3 in read_string(Chars, 39, NextCh):
        for l4 in read_after_atom4(NextCh, Dict, Tokens, Chars):
          yield False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 35):
      for l3 in read_symbol(35, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 36):
      for l3 in read_symbol(36, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 38):
      for l3 in read_symbol(38, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 42):
      for l3 in read_symbol(42, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 43):
      for l3 in read_symbol(43, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 45):
      for l3 in read_symbol(45, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 58):
      for l3 in read_symbol(58, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 60):
      for l3 in read_symbol(60, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 61):
      for l3 in read_symbol(61, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 62):
      for l3 in read_symbol(62, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 63):
      for l3 in read_symbol(63, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 64):
      for l3 in read_symbol(64, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 92):
      for l3 in read_symbol(92, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 94):
      for l3 in read_symbol(94, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 96):
      for l3 in read_symbol(96, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    Tokens = arg3
    for l2 in YP.unify(arg1, 126):
      for l3 in read_symbol(126, Dict, Tokens):
        yield False
      if doBreak:
        break
    if doBreak:
      break

def read_symbol(C1, Dict, Tokens):
  C2 = Variable()
  Chars = Variable()
  NextCh = Variable()
  for l1 in YP.get_code(C2):
    for l2 in rest_symbol(C2, Chars, NextCh):
      for l3 in read_after_atom4(NextCh, Dict, Tokens, ListPair(C1, Chars)):
        yield False

def rest_symbol(arg1, arg2, arg3):
  doBreak = False
  for _ in [1]:
    C2 = arg1
    LastCh = arg3
    Chars = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg2, ListPair(C2, Chars)):
      cutIf1 = False
      for _ in [1]:
        if YP.greaterThan(C2, 160):
          if YP.lessThan(C2, 192):
            if YP.notEqual(C2, 186):
              if YP.notEqual(C2, 170):
                for l8 in YP.get_code(NextCh):
                  for l9 in rest_symbol(NextCh, Chars, LastCh):
                    yield False
                  if doBreak:
                    break
                if doBreak:
                  break
                return
          cutIf1 = True
          doBreak = True
          break
        for l4 in symbol_char(C2):
          for l5 in YP.get_code(NextCh):
            for l6 in rest_symbol(NextCh, Chars, LastCh):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          return
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break
  for _ in [1]:
    C2 = Variable()
    for l2 in YP.unify(arg1, C2):
      for l3 in YP.unify(arg2, Atom.NIL):
        for l4 in YP.unify(arg3, C2):
          yield False
        if doBreak:
          break
      if doBreak:
        break
    if doBreak:
      break

def symbol_char(arg1):
  for l1 in YP.unify(arg1, 35):
    yield False
  for l1 in YP.unify(arg1, 36):
    yield False
  for l1 in YP.unify(arg1, 38):
    yield False
  for l1 in YP.unify(arg1, 42):
    yield False
  for l1 in YP.unify(arg1, 43):
    yield False
  for l1 in YP.unify(arg1, 45):
    yield False
  for l1 in YP.unify(arg1, 46):
    yield False
  for l1 in YP.unify(arg1, 47):
    yield False
  for l1 in YP.unify(arg1, 58):
    yield False
  for l1 in YP.unify(arg1, 60):
    yield False
  for l1 in YP.unify(arg1, 61):
    yield False
  for l1 in YP.unify(arg1, 62):
    yield False
  for l1 in YP.unify(arg1, 63):
    yield False
  for l1 in YP.unify(arg1, 64):
    yield False
  for l1 in YP.unify(arg1, 92):
    yield False
  for l1 in YP.unify(arg1, 94):
    yield False
  for l1 in YP.unify(arg1, 96):
    yield False
  for l1 in YP.unify(arg1, 126):
    yield False

def get_current_position(Pos):
  for l1 in YP.unify(Pos, 0):
    yield False

def read_after_atom4(Ch, Dict, arg3, Chars):
  Atom_1 = Variable()
  Pos = Variable()
  Tokens = Variable()
  for l1 in YP.unify(arg3, ListPair(Functor2("atom", Atom_1, Pos), Tokens)):
    for l2 in YP.unify(Pos, 0):
      for l3 in YP.atom_codes(Atom_1, Chars):
        for l4 in read_after_atom(Ch, Dict, Tokens):
          yield False

def read_after_atom(arg1, Dict, arg3):
  Tokens = Variable()
  NextCh = Variable()
  for l1 in YP.unify(arg1, 40):
    for l2 in YP.unify(arg3, ListPair(Atom.a("("), Tokens)):
      for l3 in YP.get_code(NextCh):
        for l4 in read_tokens(NextCh, Dict, Tokens):
          yield False
      return
  Ch = arg1
  Tokens = arg3
  for l1 in read_tokens(Ch, Dict, Tokens):
    yield False

def read_string(Chars, Quote, NextCh):
  Ch = Variable()
  Char = Variable()
  Next = Variable()
  for l1 in YP.get_code(Ch):
    for l2 in read_char(Ch, Quote, Char, Next):
      for l3 in rest_string5(Char, Next, Chars, Quote, NextCh):
        yield False

def rest_string5(arg1, arg2, arg3, arg4, arg5):
  _X = arg4
  NextCh = Variable()
  for l1 in YP.unify(arg1, -1):
    for l2 in YP.unify(arg2, NextCh):
      for l3 in YP.unify(arg3, Atom.NIL):
        for l4 in YP.unify(arg5, NextCh):
          yield True
          return
  Char = arg1
  Next = arg2
  Quote = arg4
  NextCh = arg5
  Chars = Variable()
  Char2 = Variable()
  Next2 = Variable()
  for l1 in YP.unify(arg3, ListPair(Char, Chars)):
    for l2 in read_char(Next, Quote, Char2, Next2):
      for l3 in rest_string5(Char2, Next2, Chars, Quote, NextCh):
        yield False

def escape_char(arg1, arg2):
  for l1 in YP.unify(arg1, 110):
    for l2 in YP.unify(arg2, 10):
      yield False
  for l1 in YP.unify(arg1, 78):
    for l2 in YP.unify(arg2, 10):
      yield False
  for l1 in YP.unify(arg1, 116):
    for l2 in YP.unify(arg2, 9):
      yield False
  for l1 in YP.unify(arg1, 84):
    for l2 in YP.unify(arg2, 9):
      yield False
  for l1 in YP.unify(arg1, 114):
    for l2 in YP.unify(arg2, 13):
      yield False
  for l1 in YP.unify(arg1, 82):
    for l2 in YP.unify(arg2, 13):
      yield False
  for l1 in YP.unify(arg1, 118):
    for l2 in YP.unify(arg2, 11):
      yield False
  for l1 in YP.unify(arg1, 86):
    for l2 in YP.unify(arg2, 11):
      yield False
  for l1 in YP.unify(arg1, 98):
    for l2 in YP.unify(arg2, 8):
      yield False
  for l1 in YP.unify(arg1, 66):
    for l2 in YP.unify(arg2, 8):
      yield False
  for l1 in YP.unify(arg1, 102):
    for l2 in YP.unify(arg2, 12):
      yield False
  for l1 in YP.unify(arg1, 70):
    for l2 in YP.unify(arg2, 12):
      yield False
  for l1 in YP.unify(arg1, 101):
    for l2 in YP.unify(arg2, 27):
      yield False
  for l1 in YP.unify(arg1, 69):
    for l2 in YP.unify(arg2, 27):
      yield False
  for l1 in YP.unify(arg1, 100):
    for l2 in YP.unify(arg2, 127):
      yield False
  for l1 in YP.unify(arg1, 68):
    for l2 in YP.unify(arg2, 127):
      yield False
  for l1 in YP.unify(arg1, 115):
    for l2 in YP.unify(arg2, 32):
      yield False
  for l1 in YP.unify(arg1, 83):
    for l2 in YP.unify(arg2, 32):
      yield False
  for l1 in YP.unify(arg1, 122):
    for l2 in YP.unify(arg2, -1):
      yield False
  for l1 in YP.unify(arg1, 90):
    for l2 in YP.unify(arg2, -1):
      yield False

def read_variable(C1, Dict, arg3):
  doBreak = False
  for _ in [1]:
    Var = Variable()
    Name = Variable()
    StartPos = Variable()
    Tokens = Variable()
    Chars = Variable()
    NextCh = Variable()
    for l2 in YP.unify(arg3, ListPair(Functor3("var", Var, Name, StartPos), Tokens)):
      for l3 in get_current_position(StartPos):
        for l4 in read_name(C1, Chars, NextCh):
          for l5 in YP.atom_codes(Name, Chars):
            cutIf1 = False
            for _ in [1]:
              if YP.termEqual(Name, Atom.a("_")):
                for l8 in read_after_atom(NextCh, Dict, Tokens):
                  yield False
                if doBreak:
                  break
                cutIf1 = True
                doBreak = True
                break
              for l7 in read_lookup(Dict, Name, Var):
                for l8 in read_after_atom(NextCh, Dict, Tokens):
                  yield False
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
    if doBreak:
      break

def read_lookup(arg1, Name, Var):
  doBreak = False
  for _ in [1]:
    N = Variable()
    V = Variable()
    L = Variable()
    for l2 in YP.unify(arg1, ListPair(Functor2("=", N, V), L)):
      cutIf1 = False
      for _ in [1]:
        for l4 in YP.unify(N, Name):
          for l5 in YP.unify(V, Var):
            yield False
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
        if doBreak:
          break
        for l4 in read_lookup(L, Name, Var):
          yield False
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def read_solidus(Ch, LastCh):
  doBreak = False
  for _ in [1]:
    NextCh = Variable()
    cutIf1 = False
    for _ in [1]:
      if YP.equal(Ch, 42):
        for l4 in YP.get_code(NextCh):
          cutIf2 = False
          for _ in [1]:
            if YP.equal(NextCh, 47):
              for l7 in YP.get_code(LastCh):
                yield False
              if doBreak:
                break
              cutIf2 = True
              doBreak = True
              break
            for l6 in read_solidus(NextCh, LastCh):
              yield False
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
      cutIf3 = False
      for _ in [1]:
        if YP.notEqual(Ch, -1):
          for l5 in YP.get_code(NextCh):
            for l6 in read_solidus(NextCh, LastCh):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf3 = True
          doBreak = True
          break
        for l4 in YP.unify(LastCh, Ch):
          for l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in /*comment~n"), Atom.NIL):
            yield False
          if doBreak:
            break
        if doBreak:
          break
      if cutIf3:
        doBreak = False
      if doBreak:
        break
    if cutIf1:
      doBreak = False
    if doBreak:
      break

def read_identifier(C1, Dict, Tokens):
  Chars = Variable()
  NextCh = Variable()
  for l1 in read_name(C1, Chars, NextCh):
    for l2 in read_after_atom4(NextCh, Dict, Tokens, Chars):
      yield False

def read_name(C1, arg2, LastCh):
  doBreak = False
  for _ in [1]:
    Chars = Variable()
    C2 = Variable()
    for l2 in YP.unify(arg2, ListPair(C1, Chars)):
      for l3 in YP.get_code(C2):
        cutIf1 = False
        for _ in [1]:
          if YP.greaterThanOrEqual(C2, ListPair(97, Atom.NIL)):
            cutIf2 = False
            for _ in [1]:
              if YP.lessThanOrEqual(C2, ListPair(122, Atom.NIL)):
                for l8 in read_name(C2, Chars, LastCh):
                  yield False
                if doBreak:
                  break
                cutIf2 = True
                doBreak = True
                break
              cutIf3 = False
              for _ in [1]:
                if YP.lessThan(C2, 192):
                  if YP.notEqual(YP.bitwiseOr(C2, 16), 186):
                    for l10 in YP.unify(Chars, Atom.NIL):
                      for l11 in YP.unify(LastCh, C2):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf3 = True
                    doBreak = True
                    break
                cutIf4 = False
                for _ in [1]:
                  if YP.equal(YP.bitwiseOr(C2, 32), 247):
                    for l10 in YP.unify(Chars, Atom.NIL):
                      for l11 in YP.unify(LastCh, C2):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf4 = True
                    doBreak = True
                    break
                  for l9 in read_name(C2, Chars, LastCh):
                    yield False
                  if doBreak:
                    break
                if cutIf4:
                  doBreak = False
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
            cutIf1 = True
            doBreak = True
            break
          cutIf5 = False
          for _ in [1]:
            if YP.greaterThanOrEqual(C2, ListPair(65, Atom.NIL)):
              cutIf6 = False
              for _ in [1]:
                if YP.greaterThan(C2, ListPair(90, Atom.NIL)):
                  if YP.notEqual(C2, ListPair(95, Atom.NIL)):
                    for l10 in YP.unify(Chars, Atom.NIL):
                      for l11 in YP.unify(LastCh, C2):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf6 = True
                    doBreak = True
                    break
                for l8 in read_name(C2, Chars, LastCh):
                  yield False
                if doBreak:
                  break
              if cutIf6:
                doBreak = False
              if doBreak:
                break
              cutIf5 = True
              doBreak = True
              break
            cutIf7 = False
            for _ in [1]:
              if YP.greaterThanOrEqual(C2, ListPair(48, Atom.NIL)):
                if YP.lessThanOrEqual(C2, ListPair(57, Atom.NIL)):
                  for l9 in read_name(C2, Chars, LastCh):
                    yield False
                  if doBreak:
                    break
                  cutIf7 = True
                  doBreak = True
                  break
              for l7 in YP.unify(Chars, Atom.NIL):
                for l8 in YP.unify(LastCh, C2):
                  yield False
                if doBreak:
                  break
              if doBreak:
                break
            if cutIf7:
              doBreak = False
            if doBreak:
              break
          if cutIf5:
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

def read_fullstop(Ch, Dict, Tokens):
  doBreak = False
  for _ in [1]:
    Number = Variable()
    Tokens1 = Variable()
    Chars = Variable()
    NextCh = Variable()
    cutIf1 = False
    for _ in [1]:
      if YP.lessThanOrEqual(Ch, ListPair(57, Atom.NIL)):
        if YP.greaterThanOrEqual(Ch, ListPair(48, Atom.NIL)):
          for l5 in YP.unify(Tokens, ListPair(Functor1("number", Number), Tokens1)):
            for l6 in read_float(Number, Dict, Tokens1, ListPair(48, Atom.NIL), Ch):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
      cutIf2 = False
      for _ in [1]:
        if YP.greaterThan(Ch, ListPair(32, Atom.NIL)):
          for l5 in rest_symbol(Ch, Chars, NextCh):
            for l6 in read_after_atom4(NextCh, Dict, Tokens, ListPair(46, Chars)):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
          break
        cutIf3 = False
        for _ in [1]:
          if YP.greaterThanOrEqual(Ch, 0):
            for l6 in YP.unify(Tokens, Atom.NIL):
              yield False
            if doBreak:
              break
            cutIf3 = True
            doBreak = True
            break
          for l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file just after full stop~n"), Atom.NIL):
            pass
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

def read_float(Number, Dict, Tokens, Digits, Digit):
  Chars = Variable()
  Rest = Variable()
  NextCh = Variable()
  for l1 in prepend(Digits, Chars, Rest):
    for l2 in read_float4(Digit, Rest, NextCh, Chars):
      for l3 in YP.number_codes(Number, Chars):
        for l4 in read_tokens(NextCh, Dict, Tokens):
          yield False

def prepend(arg1, arg2, arg3):
  X = arg3
  for l1 in YP.unify(arg1, Atom.NIL):
    for l2 in YP.unify(arg2, ListPair(46, X)):
      yield False
  Y = arg3
  C = Variable()
  Cs = Variable()
  X = Variable()
  for l1 in YP.unify(arg1, ListPair(C, Cs)):
    for l2 in YP.unify(arg2, ListPair(C, X)):
      for l3 in prepend(Cs, X, Y):
        yield False

def read_float4(C1, arg2, NextCh, Total):
  doBreak = False
  for _ in [1]:
    Chars = Variable()
    C2 = Variable()
    C3 = Variable()
    C4 = Variable()
    More = Variable()
    for l2 in YP.unify(arg2, ListPair(C1, Chars)):
      for l3 in YP.get_code(C2):
        cutIf1 = False
        for _ in [1]:
          if YP.greaterThanOrEqual(C2, ListPair(48, Atom.NIL)):
            if YP.lessThanOrEqual(C2, ListPair(57, Atom.NIL)):
              for l7 in read_float4(C2, Chars, NextCh, Total):
                yield False
              if doBreak:
                break
              cutIf1 = True
              doBreak = True
              break
          cutIf2 = False
          for _ in [1]:
            if YP.equal(YP.bitwiseOr(C2, 32), ListPair(101, Atom.NIL)):
              for l7 in YP.get_code(C3):
                cutIf3 = False
                for _ in [1]:
                  if YP.equal(C3, ListPair(45, Atom.NIL)):
                    for l10 in YP.get_code(C4):
                      for l11 in YP.unify(Chars, ListPair(C2, ListPair(45, More))):
                        cutIf4 = False
                        for _ in [1]:
                          if YP.greaterThanOrEqual(C4, ListPair(48, Atom.NIL)):
                            if YP.lessThanOrEqual(C4, ListPair(57, Atom.NIL)):
                              for l15 in read_exponent(C4, More, NextCh):
                                yield False
                              if doBreak:
                                break
                              cutIf4 = True
                              doBreak = True
                              break
                          for l13 in YP.unify(More, Atom.NIL):
                            for l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), ListPair(Total, Atom.NIL)):
                              pass
                            if doBreak:
                              break
                          if doBreak:
                            break
                          for l13 in YP.unify(More, ListPair(48, Atom.NIL)):
                            for l14 in YP.unify(NextCh, C4):
                              yield False
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
                    cutIf3 = True
                    doBreak = True
                    break
                  cutIf5 = False
                  for _ in [1]:
                    if YP.equal(C3, ListPair(43, Atom.NIL)):
                      for l11 in YP.get_code(C4):
                        for l12 in YP.unify(Chars, ListPair(C2, More)):
                          cutIf6 = False
                          for _ in [1]:
                            if YP.greaterThanOrEqual(C4, ListPair(48, Atom.NIL)):
                              if YP.lessThanOrEqual(C4, ListPair(57, Atom.NIL)):
                                for l16 in read_exponent(C4, More, NextCh):
                                  yield False
                                if doBreak:
                                  break
                                cutIf6 = True
                                doBreak = True
                                break
                            for l14 in YP.unify(More, Atom.NIL):
                              for l15 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), ListPair(Total, Atom.NIL)):
                                pass
                              if doBreak:
                                break
                            if doBreak:
                              break
                            for l14 in YP.unify(More, ListPair(48, Atom.NIL)):
                              for l15 in YP.unify(NextCh, C4):
                                yield False
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
                      if doBreak:
                        break
                      cutIf5 = True
                      doBreak = True
                      break
                    for l10 in YP.unify(C4, C3):
                      for l11 in YP.unify(Chars, ListPair(C2, More)):
                        cutIf7 = False
                        for _ in [1]:
                          if YP.greaterThanOrEqual(C4, ListPair(48, Atom.NIL)):
                            if YP.lessThanOrEqual(C4, ListPair(57, Atom.NIL)):
                              for l15 in read_exponent(C4, More, NextCh):
                                yield False
                              if doBreak:
                                break
                              cutIf7 = True
                              doBreak = True
                              break
                          for l13 in YP.unify(More, Atom.NIL):
                            for l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), ListPair(Total, Atom.NIL)):
                              pass
                            if doBreak:
                              break
                          if doBreak:
                            break
                          for l13 in YP.unify(More, ListPair(48, Atom.NIL)):
                            for l14 in YP.unify(NextCh, C4):
                              yield False
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
                if cutIf3:
                  doBreak = False
                if doBreak:
                  break
              if doBreak:
                break
              cutIf2 = True
              doBreak = True
              break
            for l6 in YP.unify(Chars, Atom.NIL):
              for l7 in YP.unify(NextCh, C2):
                yield False
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
    if doBreak:
      break

def read_exponent(C1, arg2, NextCh):
  doBreak = False
  for _ in [1]:
    Chars = Variable()
    C2 = Variable()
    for l2 in YP.unify(arg2, ListPair(C1, Chars)):
      for l3 in YP.get_code(C2):
        cutIf1 = False
        for _ in [1]:
          if YP.greaterThanOrEqual(C2, ListPair(48, Atom.NIL)):
            if YP.lessThanOrEqual(C2, ListPair(57, Atom.NIL)):
              for l7 in read_exponent(C2, Chars, NextCh):
                yield False
              if doBreak:
                break
              cutIf1 = True
              doBreak = True
              break
          for l5 in YP.unify(Chars, Atom.NIL):
            for l6 in YP.unify(NextCh, C2):
              yield False
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

def read_number(C1, Dict, arg3):
  doBreak = False
  for _ in [1]:
    Number = Variable()
    Tokens = Variable()
    C2 = Variable()
    N = Variable()
    C = Variable()
    C3 = Variable()
    Digits = Variable()
    for l2 in YP.unify(arg3, ListPair(Functor1("number", Number), Tokens)):
      for l3 in read_number4(C1, C2, 0, N):
        cutIf1 = False
        for _ in [1]:
          if YP.equal(C2, 39):
            cutIf2 = False
            for _ in [1]:
              if YP.greaterThanOrEqual(N, 2):
                if YP.lessThanOrEqual(N, 36):
                  for l9 in read_based(N, 0, Number, C):
                    for l10 in read_tokens(C, Dict, Tokens):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf2 = True
                  doBreak = True
                  break
              cutIf3 = False
              for _ in [1]:
                if YP.equal(N, 0):
                  for l9 in YP.get_code(C3):
                    for l10 in read_char(C3, -1, Number, C):
                      for l11 in read_tokens(C, Dict, Tokens):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf3 = True
                  doBreak = True
                  break
                for l8 in formatError(Atom.a("user_error"), Atom.a("~N** ~d' read as ~d '~n"), ListPair(N, ListPair(N, Atom.NIL))):
                  for l9 in YP.unify(Number, N):
                    for l10 in YP.unify(C, C2):
                      for l11 in read_tokens(C, Dict, Tokens):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
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
            cutIf1 = True
            doBreak = True
            break
          cutIf4 = False
          for _ in [1]:
            if YP.equal(C2, 46):
              for l7 in YP.get_code(C3):
                cutIf5 = False
                for _ in [1]:
                  if YP.greaterThanOrEqual(C3, ListPair(48, Atom.NIL)):
                    if YP.lessThanOrEqual(C3, ListPair(57, Atom.NIL)):
                      for l11 in YP.number_codes(N, Digits):
                        for l12 in read_float(Number, Dict, Tokens, Digits, C3):
                          yield False
                        if doBreak:
                          break
                      if doBreak:
                        break
                      cutIf5 = True
                      doBreak = True
                      break
                  for l9 in YP.unify(Number, N):
                    for l10 in read_fullstop(C3, Dict, Tokens):
                      yield False
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
              cutIf4 = True
              doBreak = True
              break
            for l6 in YP.unify(Number, N):
              for l7 in read_tokens(C2, Dict, Tokens):
                yield False
              if doBreak:
                break
            if doBreak:
              break
          if cutIf4:
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

def read_number4(C0, C, N0, N):
  doBreak = False
  for _ in [1]:
    N1 = Variable()
    C1 = Variable()
    cutIf1 = False
    for _ in [1]:
      if YP.greaterThanOrEqual(C0, ListPair(48, Atom.NIL)):
        if YP.lessThanOrEqual(C0, ListPair(57, Atom.NIL)):
          for l5 in YP.unify(N1, YP.add(YP.subtract(YP.multiply(N0, 10), ListPair(48, Atom.NIL)), C0)):
            for l6 in YP.get_code(C1):
              for l7 in read_number4(C1, C, N1, N):
                yield False
              if doBreak:
                break
            if doBreak:
              break
          if doBreak:
            break
          cutIf1 = True
          doBreak = True
          break
      cutIf2 = False
      for _ in [1]:
        if YP.equal(C0, 95):
          for l5 in YP.get_code(C1):
            for l6 in read_number4(C1, C, N0, N):
              yield False
            if doBreak:
              break
          if doBreak:
            break
          cutIf2 = True
          doBreak = True
          break
        for l4 in YP.unify(C, C0):
          for l5 in YP.unify(N, N0):
            yield False
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

def read_based(Base, N0, N, C):
  doBreak = False
  for _ in [1]:
    C1 = Variable()
    Digit = Variable()
    N1 = Variable()
    for l2 in YP.get_code(C1):
      cutIf1 = False
      for _ in [1]:
        if YP.greaterThanOrEqual(C1, ListPair(48, Atom.NIL)):
          if YP.lessThanOrEqual(C1, ListPair(57, Atom.NIL)):
            for l6 in YP.unify(Digit, YP.subtract(C1, ListPair(48, Atom.NIL))):
              cutIf2 = False
              for _ in [1]:
                if YP.lessThan(Digit, Base):
                  for l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit)):
                    for l10 in read_based(Base, N1, N, C):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf2 = True
                  doBreak = True
                  break
                cutIf3 = False
                for _ in [1]:
                  if YP.equal(C1, ListPair(95, Atom.NIL)):
                    for l10 in read_based(Base, N0, N, C):
                      yield False
                    if doBreak:
                      break
                    cutIf3 = True
                    doBreak = True
                    break
                  for l9 in YP.unify(N, N0):
                    for l10 in YP.unify(C, C1):
                      yield False
                    if doBreak:
                      break
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
            if doBreak:
              break
            cutIf1 = True
            doBreak = True
            break
        cutIf4 = False
        for _ in [1]:
          if YP.greaterThanOrEqual(C1, ListPair(65, Atom.NIL)):
            if YP.lessThanOrEqual(C1, ListPair(90, Atom.NIL)):
              for l7 in YP.unify(Digit, YP.subtract(C1, YP.subtract(ListPair(65, Atom.NIL), 10))):
                cutIf5 = False
                for _ in [1]:
                  if YP.lessThan(Digit, Base):
                    for l10 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit)):
                      for l11 in read_based(Base, N1, N, C):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                    cutIf5 = True
                    doBreak = True
                    break
                  cutIf6 = False
                  for _ in [1]:
                    if YP.equal(C1, ListPair(95, Atom.NIL)):
                      for l11 in read_based(Base, N0, N, C):
                        yield False
                      if doBreak:
                        break
                      cutIf6 = True
                      doBreak = True
                      break
                    for l10 in YP.unify(N, N0):
                      for l11 in YP.unify(C, C1):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if cutIf6:
                    doBreak = False
                  if doBreak:
                    break
                if cutIf5:
                  doBreak = False
                if doBreak:
                  break
              if doBreak:
                break
              cutIf4 = True
              doBreak = True
              break
          cutIf7 = False
          for _ in [1]:
            if YP.greaterThanOrEqual(C1, ListPair(97, Atom.NIL)):
              if YP.lessThanOrEqual(C1, ListPair(122, Atom.NIL)):
                for l8 in YP.unify(Digit, YP.subtract(C1, YP.subtract(ListPair(97, Atom.NIL), 10))):
                  cutIf8 = False
                  for _ in [1]:
                    if YP.lessThan(Digit, Base):
                      for l11 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit)):
                        for l12 in read_based(Base, N1, N, C):
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
                      if YP.equal(C1, ListPair(95, Atom.NIL)):
                        for l12 in read_based(Base, N0, N, C):
                          yield False
                        if doBreak:
                          break
                        cutIf9 = True
                        doBreak = True
                        break
                      for l11 in YP.unify(N, N0):
                        for l12 in YP.unify(C, C1):
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
                if doBreak:
                  break
                cutIf7 = True
                doBreak = True
                break
            for l6 in YP.unify(Digit, 99):
              cutIf10 = False
              for _ in [1]:
                if YP.lessThan(Digit, Base):
                  for l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit)):
                    for l10 in read_based(Base, N1, N, C):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf10 = True
                  doBreak = True
                  break
                cutIf11 = False
                for _ in [1]:
                  if YP.equal(C1, ListPair(95, Atom.NIL)):
                    for l10 in read_based(Base, N0, N, C):
                      yield False
                    if doBreak:
                      break
                    cutIf11 = True
                    doBreak = True
                    break
                  for l9 in YP.unify(N, N0):
                    for l10 in YP.unify(C, C1):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                if cutIf11:
                  doBreak = False
                if doBreak:
                  break
              if cutIf10:
                doBreak = False
              if doBreak:
                break
            if doBreak:
              break
          if cutIf7:
            doBreak = False
          if doBreak:
            break
        if cutIf4:
          doBreak = False
        if doBreak:
          break
      if cutIf1:
        doBreak = False
      if doBreak:
        break
    if doBreak:
      break

def read_char(Char, Quote, Result, Next):
  doBreak = False
  for _ in [1]:
    C1 = Variable()
    C2 = Variable()
    C3 = Variable()
    Ch = Variable()
    cutIf1 = False
    for _ in [1]:
      if YP.equal(Char, 92):
        for l4 in YP.get_code(C1):
          cutIf2 = False
          for _ in [1]:
            if YP.lessThan(C1, 0):
              for l7 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~cquoted~c~n"), ListPair(Quote, ListPair(Quote, Atom.NIL))):
                for l8 in YP.unify(Result, -1):
                  for l9 in YP.unify(Next, C1):
                    yield False
                  if doBreak:
                    break
                if doBreak:
                  break
              if doBreak:
                break
              cutIf2 = True
              doBreak = True
              break
            cutIf3 = False
            for _ in [1]:
              if YP.lessThanOrEqual(C1, ListPair(32, Atom.NIL)):
                for l8 in YP.get_code(C2):
                  for l9 in read_char(C2, Quote, Result, Next):
                    yield False
                  if doBreak:
                    break
                if doBreak:
                  break
                cutIf3 = True
                doBreak = True
                break
              cutIf4 = False
              for _ in [1]:
                if YP.equal(YP.bitwiseOr(C1, 32), ListPair(99, Atom.NIL)):
                  for l9 in YP.get_code(C2):
                    for l10 in read_char(C2, Quote, Result, Next):
                      yield False
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf4 = True
                  doBreak = True
                  break
                cutIf5 = False
                for _ in [1]:
                  if YP.lessThanOrEqual(C1, ListPair(55, Atom.NIL)):
                    if YP.greaterThanOrEqual(C1, ListPair(48, Atom.NIL)):
                      for l11 in YP.get_code(C2):
                        cutIf6 = False
                        for _ in [1]:
                          if YP.lessThanOrEqual(C2, ListPair(55, Atom.NIL)):
                            if YP.greaterThanOrEqual(C2, ListPair(48, Atom.NIL)):
                              for l15 in YP.get_code(C3):
                                cutIf7 = False
                                for _ in [1]:
                                  if YP.lessThanOrEqual(C3, ListPair(55, Atom.NIL)):
                                    if YP.greaterThanOrEqual(C3, ListPair(48, Atom.NIL)):
                                      for l19 in YP.get_code(Next):
                                        for l20 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(YP.add(YP.multiply(C1, 8), C2), 8), C3), YP.multiply(73, ListPair(48, Atom.NIL)))):
                                          yield False
                                        if doBreak:
                                          break
                                      if doBreak:
                                        break
                                      cutIf7 = True
                                      doBreak = True
                                      break
                                  for l17 in YP.unify(Next, C3):
                                    for l18 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(C1, 8), C2), YP.multiply(9, ListPair(48, Atom.NIL)))):
                                      yield False
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
                              cutIf6 = True
                              doBreak = True
                              break
                          for l13 in YP.unify(Next, C2):
                            for l14 in YP.unify(Result, YP.subtract(C1, ListPair(48, Atom.NIL))):
                              yield False
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
                  cutIf8 = False
                  for _ in [1]:
                    if YP.equal(C1, ListPair(94, Atom.NIL)):
                      for l11 in YP.get_code(C2):
                        cutIf9 = False
                        for _ in [1]:
                          if YP.lessThan(C2, 0):
                            for l14 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~c..~c^..~c~n"), ListPair.make([Quote, 92, Quote])):
                              for l15 in YP.unify(Result, -1):
                                for l16 in YP.unify(Next, C2):
                                  yield False
                                if doBreak:
                                  break
                              if doBreak:
                                break
                            if doBreak:
                              break
                            cutIf9 = True
                            doBreak = True
                            break
                          cutIf10 = False
                          for _ in [1]:
                            if YP.equal(C2, ListPair(63, Atom.NIL)):
                              for l15 in YP.unify(Result, 127):
                                for l16 in YP.get_code(Next):
                                  yield False
                                if doBreak:
                                  break
                              if doBreak:
                                break
                              cutIf10 = True
                              doBreak = True
                              break
                            for l14 in YP.unify(Result, YP.bitwiseAnd(C2, 31)):
                              for l15 in YP.get_code(Next):
                                yield False
                              if doBreak:
                                break
                            if doBreak:
                              break
                          if cutIf10:
                            doBreak = False
                          if doBreak:
                            break
                        if cutIf9:
                          doBreak = False
                        if doBreak:
                          break
                      if doBreak:
                        break
                      cutIf8 = True
                      doBreak = True
                      break
                    cutIf11 = False
                    for _ in [1]:
                      for l11 in escape_char(C1, Result):
                        for l12 in YP.get_code(Next):
                          yield False
                        if doBreak:
                          break
                        cutIf11 = True
                        doBreak = True
                        break
                      if doBreak:
                        break
                      for l11 in YP.unify(Result, C1):
                        for l12 in YP.get_code(Next):
                          yield False
                        if doBreak:
                          break
                      if doBreak:
                        break
                    if cutIf11:
                      doBreak = False
                    if doBreak:
                      break
                  if cutIf8:
                    doBreak = False
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
            if cutIf3:
              doBreak = False
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
      cutIf12 = False
      for _ in [1]:
        if YP.equal(Char, Quote):
          for l5 in YP.get_code(Ch):
            cutIf13 = False
            for _ in [1]:
              if YP.equal(Ch, Quote):
                for l8 in YP.unify(Result, Quote):
                  for l9 in YP.get_code(Next):
                    yield False
                  if doBreak:
                    break
                if doBreak:
                  break
                cutIf13 = True
                doBreak = True
                break
              for l7 in YP.unify(Result, -1):
                for l8 in YP.unify(Next, Ch):
                  yield False
                if doBreak:
                  break
              if doBreak:
                break
            if cutIf13:
              doBreak = False
            if doBreak:
              break
          if doBreak:
            break
          cutIf12 = True
          doBreak = True
          break
        cutIf14 = False
        for _ in [1]:
          if YP.lessThan(Char, ListPair(32, Atom.NIL)):
            if YP.notEqual(Char, 9):
              if YP.notEqual(Char, 10):
                if YP.notEqual(Char, 13):
                  for l9 in YP.unify(Result, -1):
                    for l10 in YP.unify(Next, Char):
                      for l11 in formatError(Atom.a("user_error"), Atom.a("~N** Strange character ~d ends ~ctoken~c~n"), ListPair.make([Char, Quote, Quote])):
                        yield False
                      if doBreak:
                        break
                    if doBreak:
                      break
                  if doBreak:
                    break
                  cutIf14 = True
                  doBreak = True
                  break
          for l5 in YP.unify(Result, Char):
            for l6 in YP.get_code(Next):
              yield False
            if doBreak:
              break
          if doBreak:
            break
        if cutIf14:
          doBreak = False
        if doBreak:
          break
      if cutIf12:
        doBreak = False
      if doBreak:
        break
    if cutIf1:
      doBreak = False
    if doBreak:
      break


