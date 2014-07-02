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

function Parser() {
}

Parser.read_term2 = function(Term, Options) {
    var Answer = new Variable();
    var Variables = new Variable();
    for each (var l1 in Parser.read_termOptions(Options, Variables)) {
        for each (var l2 in portable_read3(Answer, Variables, new Variable())) {
            for each (var l3 in remove_pos(Answer, Term))
                yield false;
        }
    }
}

Parser.read_term3 = function(Input, Term, Options) {
    var SaveInput = new Variable();
    var Answer = new Variable();
    var Variables = new Variable();
    for each (var l1 in Parser.read_termOptions(Options, Variables)) {
        for each (var l2 in YP.current_input(SaveInput)) {
            try {
                YP.see(Input);
                for each (var l3 in portable_read3(Answer, Variables, new Variable())) {
                    for each (var l4 in remove_pos(Answer, Term))
                        yield false;
                }
            }
            finally {
                YP.see(SaveInput);
            }
        }
    }
}

// For read_term, check if Options has variable_names(Variables).
// Otherwise, ignore Options.
Parser.read_termOptions = function(Options, Variables) {
    Options = YP.getValue(Options);
    if (Options instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"), "Options is an unbound variable");
    // First try to match Options = [variable_names(Variables)]
    for each (var l1 in YP.unify(Options, ListPair.make(new Functor1("variable_names", Variables)))) {
        yield false;
        return;
    }
    // Default: Ignore Options.
    yield false;
}

Parser.read1 = function(Term) {
    return Parser.read_term2(Term, Atom.NIL);
}

Parser.read2 = function(Input, Term) {
    return Parser.read_term3(Input, Term, Atom.NIL);
}

function formatError(Output, Format, Arguments) {
    // Debug: Simple implementation for now.
    YP.write(Format);
    YP.write(Arguments);
    YP.nl();
    yield false;
}

// Debug: Hand-modify this central predicate to do tail recursion.
function read_tokens(arg1, arg2, arg3) {
    var repeat = true;
    while (repeat) {
        repeat = false;

        cutIf9:
        cutIf8:
        cutIf7:
        cutIf6:
        cutIf5:
        cutIf4:
        cutIf3:
        cutIf2:
        cutIf1:
        {
            var C1 = arg1;
            var Dict = arg2;
            var Tokens = arg3;
            var C2 = new Variable();
            if (YP.lessThanOrEqual(C1, new ListPair(32, Atom.NIL))) {
                if (YP.greaterThanOrEqual(C1, 0)) {
                    for each (var l4 in YP.get_code(C2)) {
/*
                        for each (var l5 in read_tokens(C2, Dict, Tokens)) {
                            yield false;
                        }
*/
                        arg1 = YP.getValue(C2);
                        arg2 = YP.getValue(Dict);
                        arg3 = YP.getValue(Tokens);
                        repeat = true;
                    }
                }
                break cutIf1;
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(97, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(122, Atom.NIL))) {
                    for each (var l4 in read_identifier(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf2;
                }
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(65, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(90, Atom.NIL))) {
                    for each (var l4 in read_variable(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf3;
                }
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(57, Atom.NIL))) {
                    for each (var l4 in read_number(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf4;
                }
            }
            if (YP.lessThan(C1, 127)) {
                for each (var l3 in read_special(C1, Dict, Tokens)) {
                    yield false;
                }
                break cutIf5;
            }
            if (YP.lessThanOrEqual(C1, 160)) {
                for each (var l3 in YP.get_code(C2)) {
/*
                    for each (var l4 in read_tokens(C2, Dict, Tokens)) {
                        yield false;
                    }
*/
                    arg1 = YP.getValue(C2);
                    arg2 = YP.getValue(Dict);
                    arg3 = YP.getValue(Tokens);
                    repeat = true;
                }
                break cutIf6;
            }
            if (YP.greaterThanOrEqual(C1, 223)) {
                if (YP.notEqual(C1, 247)) {
                    for each (var l4 in read_identifier(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf7;
                }
            }
            if (YP.greaterThanOrEqual(C1, 192)) {
                if (YP.notEqual(C1, 215)) {
                    for each (var l4 in read_variable(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf8;
                }
            }
            if (YP.notEqual(C1, 170)) {
                if (YP.notEqual(C1, 186)) {
                    for each (var l4 in read_symbol(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf9;
                }
            }
            for each (var l2 in read_identifier(C1, Dict, Tokens)) {
                yield false;
            }
        }
    }
}

// Compiler output follows.

function getDeclaringClass() { return null; }

function parseInput(TermList) {
  {
    var TermAndVariables = new Variable();
    var findallAnswers1 = new FindallAnswers(TermAndVariables);
    for each (var l2 in parseInputHelper(TermAndVariables)) {
      findallAnswers1.add();
    }
    for each (var l2 in findallAnswers1.result(TermList)) {
      yield false;
    }
  }
}

function parseInputHelper(arg1) {
  {
    var Term = new Variable();
    var Variables = new Variable();
    var Answer = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("f", Term, Variables))) {
      for each (var l3 in YP.repeat()) {
        for each (var l4 in portable_read3(Answer, Variables, x4)) {
          for each (var l5 in remove_pos(Answer, Term)) {
            cutIf1:
            {
              if (YP.termEqual(Term, Atom.a("end_of_file"))) {
                return;
                break cutIf1;
              }
              yield false;
            }
          }
        }
      }
    }
  }
}

function clear_errors() {
  {
    yield false;
  }
}

function remove_pos(arg1, arg2) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, X)) {
      for each (var l3 in YP.unify(arg2, X)) {
        if (YP.var(X)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var X = arg2;
    var _Pos = new Variable();
    var _Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("$VAR", _Pos, _Name, X))) {
      if (YP.var(X)) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var H = new Variable();
    var T = new Variable();
    var NH = new Variable();
    var NT = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      for each (var l3 in YP.unify(arg2, new ListPair(NH, NT))) {
        for each (var l4 in remove_pos(H, NH)) {
          for each (var l5 in remove_pos(T, NT)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var A = new Variable();
    var B = new Variable();
    var NA = new Variable();
    var NB = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in YP.unify(arg2, new Functor2(",", NA, NB))) {
        for each (var l4 in remove_pos(A, NA)) {
          for each (var l5 in remove_pos(B, NB)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Atom_1 = new Variable();
    var _F = new Variable();
    for each (var l2 in YP.unify(arg1, Atom_1)) {
      for each (var l3 in YP.unify(arg2, Atom_1)) {
        for each (var l4 in YP.functor(Atom_1, _F, 0)) {
          yield false;
        }
      }
    }
  }
  {
    var Term = arg1;
    var NewTerm = arg2;
    var Func = new Variable();
    var _Pos = new Variable();
    var Args = new Variable();
    var NArgs = new Variable();
    if (YP.nonvar(Term)) {
      for each (var l3 in YP.univ(Term, new ListPair(Func, new ListPair(_Pos, Args)))) {
        for each (var l4 in remove_pos(Args, NArgs)) {
          for each (var l5 in YP.univ(NewTerm, new ListPair(Func, NArgs))) {
            yield false;
          }
        }
      }
    }
  }
}

function portable_read_position(Term, PosTerm, Syntax) {
  {
    for each (var l2 in portable_read(PosTerm, Syntax)) {
      for each (var l3 in remove_pos(PosTerm, Term)) {
        yield false;
      }
    }
  }
}

function portable_read(Answer, Syntax) {
  {
    var Tokens = new Variable();
    var ParseTokens = new Variable();
    for each (var l2 in read_tokens1(Tokens)) {
      for each (var l3 in remove_comments(Tokens, ParseTokens, Syntax)) {
        for each (var l4 in parse2(ParseTokens, Answer)) {
          yield false;
        }
      }
    }
  }
}

function portable_read3(Answer, Variables, Syntax) {
  {
    var Tokens = new Variable();
    var ParseTokens = new Variable();
    for each (var l2 in read_tokens2(Tokens, Variables)) {
      for each (var l3 in remove_comments(Tokens, ParseTokens, Syntax)) {
        for each (var l4 in parse2(ParseTokens, Answer)) {
          yield false;
        }
      }
    }
  }
}

function remove_comments(arg1, arg2, arg3) {
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
    var Ys = arg2;
    var S = new Variable();
    var E = new Variable();
    var Xs = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("comment", S, E), Xs))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("comment", S, E), Zs))) {
        for each (var l4 in remove_comments(Xs, Ys, Zs)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Pos = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    var Pos2 = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("/", Atom.a("["), Pos), Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("["), Ys))) {
        for each (var l4 in YP.unify(arg3, new ListPair(new Functor2("list", Pos, Pos2), Zs))) {
          for each (var l5 in YP.unify(Pos2, YP.add(Pos, 1))) {
            for each (var l6 in remove_comments(Xs, Ys, Zs)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    var Pos2 = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("/", Atom.a("]"), Pos), Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("]"), Ys))) {
        for each (var l4 in YP.unify(arg3, new ListPair(new Functor2("list", Pos, Pos2), Zs))) {
          for each (var l5 in YP.unify(Pos2, YP.add(Pos, 1))) {
            for each (var l6 in remove_comments(Xs, Ys, Zs)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Zs = arg3;
    var Token = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Token, Ys))) {
        for each (var l4 in remove_comments(Xs, Ys, Zs)) {
          yield false;
        }
      }
    }
  }
}

function expect(Token, arg2, arg3) {
  {
    var Rest = arg3;
    for each (var l2 in YP.unify(arg2, new ListPair(Token, Rest))) {
      yield true;
      return;
    }
  }
  {
    var S0 = arg2;
    var x3 = arg3;
    for each (var l2 in syntax_error(ListPair.make([Token, Atom.a("or"), Atom.a("operator"), Atom.a("expected")]), S0)) {
      yield false;
    }
  }
}

function parse2(Tokens, Answer) {
  {
    var Term = new Variable();
    var LeftOver = new Variable();
    for each (var l2 in clear_errors()) {
      for each (var l3 in parse(Tokens, 1200, Term, LeftOver)) {
        for each (var l4 in all_read(LeftOver)) {
          for each (var l5 in YP.unify(Answer, Term)) {
            yield false;
          }
          return;
        }
      }
      for each (var l3 in syntax_error1(Tokens)) {
        yield false;
      }
    }
  }
}

function all_read(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
  {
    var Token = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S))) {
      for each (var l3 in syntax_error(ListPair.make([Atom.a("operator"), Atom.a("expected"), Atom.a("after"), Atom.a("expression")]), new ListPair(Token, S))) {
        yield false;
      }
    }
  }
}

function parse(arg1, arg2, arg3, arg4) {
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in syntax_error(new ListPair(Atom.a("expression"), new ListPair(Atom.a("expected"), Atom.NIL)), Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Precedence = arg2;
    var Term = arg3;
    var LeftOver = arg4;
    var Token = new Variable();
    var RestTokens = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, RestTokens))) {
      for each (var l3 in parse5(Token, RestTokens, Precedence, Term, LeftOver)) {
        yield false;
      }
    }
  }
}

function parse5(arg1, arg2, arg3, arg4, arg5) {
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in cannot_start(Atom.a("}"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in cannot_start(Atom.a("]"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in cannot_start(Atom.a(")"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in cannot_start(Atom.a(","), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in cannot_start(Atom.a("|"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Codes = new Variable();
    var Term = new Variable();
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", Codes))) {
      cutIf1:
      {
        for each (var l4 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("atom"))) {
          for each (var l5 in YP.atom_codes(Term, Codes)) {
            for each (var l6 in exprtl0(S0, Term, Precedence, Answer, S)) {
              yield false;
            }
          }
          break cutIf1;
        }
        cutIf2:
        {
          for each (var l5 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("chars"))) {
            for each (var l6 in YP.atom_codes(A, Codes)) {
              for each (var l7 in YP.atom_chars(A, Term)) {
                for each (var l8 in exprtl0(S0, Term, Precedence, Answer, S)) {
                  yield false;
                }
              }
            }
            break cutIf2;
          }
          for each (var l5 in YP.unify(Term, Codes)) {
            for each (var l6 in exprtl0(S0, Term, Precedence, Answer, S)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Number = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", Number))) {
      for each (var l3 in exprtl0(S0, Number, Precedence, Answer, S)) {
        yield false;
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("]"), S1))) {
        for each (var l4 in read_atom(new Functor2("/", Atom.NIL, 0), S1, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in parse(S1, 999, Arg1, S2)) {
        for each (var l4 in read_list(S2, RestArgs, S3)) {
          for each (var l5 in exprtl0(S3, new ListPair(Arg1, RestArgs), Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a(")"), S2, S3)) {
          for each (var l5 in exprtl0(S3, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a(")"), S2, S3)) {
          for each (var l5 in exprtl0(S3, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var _Pos = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("{"), _Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("}"), S1))) {
        for each (var l4 in read_atom(Atom.a("{}"), S1, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Pos = new Variable();
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("{"), Pos))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a("}"), S2, S3)) {
          for each (var l5 in exprtl0(S3, new Functor2("{}", Pos, Term), Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Variable_1 = new Variable();
    var Name = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", Variable_1, Name, Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("("), S1))) {
        for each (var l4 in parse(S1, 999, Arg1, S2)) {
          for each (var l5 in read_args(S2, RestArgs, S3)) {
            for each (var l6 in YP.univ(Term, new ListPair(Atom.a("call"), new ListPair(new Functor3("$VAR", Pos, Name, Variable_1), new ListPair(Arg1, RestArgs))))) {
              for each (var l7 in exprtl0(S3, Term, Precedence, Answer, S)) {
                yield false;
              }
            }
            return;
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Variable_1 = new Variable();
    var Name = new Variable();
    var Pos = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", Variable_1, Name, Pos))) {
      for each (var l3 in exprtl0(S0, new Functor3("$VAR", Pos, Name, Variable_1), Precedence, Answer, S)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Atom_1 = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", Atom_1, P))) {
      for each (var l3 in read_atom(new Functor2("/", Atom_1, P), S0, Precedence, Answer, S)) {
        yield false;
      }
    }
  }
}

function read_atom(arg1, arg2, Precedence, Answer, S) {
  {
    var _Pos = new Variable();
    var Number = new Variable();
    var S1 = new Variable();
    var Negative = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("-"), _Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", Number), S1))) {
        for each (var l4 in YP.unify(Negative, YP.negate(Number))) {
          for each (var l5 in exprtl0(S1, Negative, Precedence, Answer, S)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Functor_1 = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Functor_1, Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("("), S1))) {
        for each (var l4 in parse(S1, 999, Arg1, S2)) {
          for each (var l5 in read_args(S2, RestArgs, S3)) {
            for each (var l6 in YP.univ(Term, new ListPair(Functor_1, new ListPair(Pos, new ListPair(Arg1, RestArgs))))) {
              for each (var l7 in exprtl0(S3, Term, Precedence, Answer, S)) {
                yield false;
              }
            }
            return;
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Op = new Variable();
    var Pos = new Variable();
    var Oprec = new Variable();
    var Aprec = new Variable();
    var Flag = new Variable();
    var Term = new Variable();
    var Arg = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Op, Pos))) {
      for each (var l3 in prefixop(Op, Oprec, Aprec)) {
        for each (var l4 in possible_right_operand(S0, Flag)) {
          cutIf1:
          {
            if (YP.lessThan(Flag, 0)) {
              for each (var l7 in YP.univ(Term, new ListPair(Op, new ListPair(Pos, Atom.NIL)))) {
                for each (var l8 in exprtl0(S0, Term, Precedence, Answer, S)) {
                  yield false;
                }
              }
              break cutIf1;
            }
            cutIf2:
            {
              if (YP.greaterThan(Oprec, Precedence)) {
                for each (var l8 in syntax_error(ListPair.make([Atom.a("prefix"), Atom.a("operator"), Op, Atom.a("in"), Atom.a("context"), Atom.a("with"), Atom.a("precedence"), Precedence]), S0)) {
                  yield false;
                }
                break cutIf2;
              }
              cutIf3:
              {
                if (YP.greaterThan(Flag, 0)) {
                  for each (var l9 in parse(S0, Aprec, Arg, S1)) {
                    for each (var l10 in YP.univ(Term, ListPair.make([Op, Pos, Arg]))) {
                      for each (var l11 in exprtl(S1, Oprec, Term, Precedence, Answer, S)) {
                        yield false;
                      }
                    }
                    return;
                  }
                  break cutIf3;
                }
                for each (var l8 in peepop(S0, S1)) {
                  for each (var l9 in prefix_is_atom(S1, Oprec)) {
                    for each (var l10 in exprtl(S1, Oprec, new Functor2("/", Op, Pos), Precedence, Answer, S)) {
                      yield false;
                    }
                  }
                }
                for each (var l8 in parse(S0, Aprec, Arg, S1)) {
                  for each (var l9 in YP.univ(Term, ListPair.make([Op, Pos, Arg]))) {
                    for each (var l10 in exprtl(S1, Oprec, Term, Precedence, Answer, S)) {
                      yield false;
                    }
                  }
                  return;
                }
              }
            }
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Atom_1 = new Variable();
    var Pos = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom_1, Pos))) {
      for each (var l3 in YP.univ(Term, new ListPair(Atom_1, new ListPair(Pos, Atom.NIL)))) {
        for each (var l4 in exprtl0(S0, Term, Precedence, Answer, S)) {
          yield false;
        }
      }
    }
  }
}

function cannot_start(Token, S0) {
  {
    for each (var l2 in syntax_error(ListPair.make([Token, Atom.a("cannot"), Atom.a("start"), Atom.a("an"), Atom.a("expression")]), S0)) {
      yield false;
    }
  }
}

function read_args(arg1, arg2, arg3) {
  {
    var S = arg3;
    var S1 = new Variable();
    var Term = new Variable();
    var Rest = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a(","), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Term, Rest))) {
        for each (var l4 in parse(S1, 999, Term, S2)) {
          for each (var l5 in read_args(S2, Rest, S)) {
            yield false;
          }
          return;
        }
        return;
      }
    }
  }
  {
    var S = arg3;
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a(")"), S))) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var S = arg1;
    var x2 = arg2;
    var x3 = arg3;
    for each (var l2 in syntax_error(ListPair.make([Atom.a(", or )"), Atom.a("expected"), Atom.a("in"), Atom.a("arguments")]), S)) {
      yield false;
    }
  }
}

function read_list(arg1, arg2, arg3) {
  {
    var x1 = arg2;
    var x2 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Rest = arg2;
    var S = arg3;
    var Token = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S1))) {
      for each (var l3 in read_list4(Token, S1, Rest, S)) {
        yield false;
      }
    }
  }
}

function read_list4(arg1, arg2, arg3, arg4) {
  {
    var S1 = arg2;
    var S = arg4;
    var Term = new Variable();
    var Rest = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Term, Rest))) {
        for each (var l4 in parse(S1, 999, Term, S2)) {
          for each (var l5 in read_list(S2, Rest, S)) {
            yield false;
          }
          return;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Rest = arg3;
    var S = arg4;
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in parse(S1, 999, Rest, S2)) {
        for each (var l4 in expect(Atom.a("]"), S2, S)) {
          yield false;
        }
        return;
      }
      return;
    }
  }
  {
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, S1)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          for each (var l5 in YP.unify(arg4, S1)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Token = arg1;
    var S1 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    for each (var l2 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), new ListPair(Token, S1))) {
      yield false;
    }
  }
}

function possible_right_operand(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var Flag = arg2;
    var H = new Variable();
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      for each (var l3 in possible_right_operand3(H, Flag, T)) {
        yield false;
      }
    }
  }
}

function possible_right_operand3(arg1, arg2, arg3) {
  {
    var x4 = arg3;
    var x1 = new Variable();
    var x2 = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", x1, x2, x3))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg3;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", x1))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg3;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", x1))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, new ListPair(Atom.a("]"), x1))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, new ListPair(Atom.a("}"), x1))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x3 = arg3;
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", x1, x2))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        yield false;
      }
    }
  }
}

function peepop(arg1, arg2) {
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), new ListPair(Atom.a("("), S1)))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom", F, Pos), new ListPair(Atom.a("("), S1)))) {
        yield true;
        return;
      }
    }
  }
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var L = new Variable();
    var P = new Variable();
    var R = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L, P, R]), S1))) {
        for each (var l4 in infixop(F, L, P, R)) {
          yield false;
        }
      }
    }
  }
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var L = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L, P), S1))) {
        for each (var l4 in postfixop(F, L, P)) {
          yield false;
        }
      }
    }
  }
  {
    var S0 = new Variable();
    for each (var l2 in YP.unify(arg1, S0)) {
      for each (var l3 in YP.unify(arg2, S0)) {
        yield false;
      }
    }
  }
}

function prefix_is_atom(arg1, arg2) {
  {
    var Precedence = arg2;
    var Token = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, x2))) {
      for each (var l3 in prefix_is_atom(Token, Precedence)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    var x1 = new Variable();
    var L = new Variable();
    var x3 = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor(Atom.a("infixop", Atom.a("")), [x1, L, x3, x4]))) {
      if (YP.greaterThanOrEqual(L, P)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    var x1 = new Variable();
    var L = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3(Atom.a("postfixop", Atom.a("")), x1, L, x3))) {
      if (YP.greaterThanOrEqual(L, P)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      yield false;
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      yield false;
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      yield false;
    }
  }
  {
    var P = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      if (YP.greaterThanOrEqual(1100, P)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      if (YP.greaterThanOrEqual(1000, P)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
}

function exprtl0(arg1, arg2, arg3, arg4, arg5) {
  {
    var x2 = arg3;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Token = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S1))) {
      for each (var l3 in exprtl0_6(Token, Term, Precedence, Answer, S, S1)) {
        yield false;
      }
    }
  }
}

function exprtl0_6(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a("}"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a("]"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a(")"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      cutIf1:
      {
        if (YP.greaterThanOrEqual(Precedence, 1000)) {
          for each (var l5 in parse(S1, 1000, Next, S2)) {
            for each (var l6 in exprtl(S2, 1000, new Functor2(",", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
            return;
          }
          break cutIf1;
        }
        for each (var l4 in YP.unify(Answer, Term)) {
          for each (var l5 in YP.unify(S, new ListPair(Atom.a(","), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      cutIf2:
      {
        if (YP.greaterThanOrEqual(Precedence, 1100)) {
          for each (var l5 in parse(S1, 1100, Next, S2)) {
            for each (var l6 in exprtl(S2, 1100, new Functor2(";", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
            return;
          }
          break cutIf2;
        }
        for each (var l4 in YP.unify(Answer, Term)) {
          for each (var l5 in YP.unify(S, new ListPair(Atom.a("|"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var S1 = arg6;
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", S))) {
      for each (var l3 in cannot_follow(Atom.a("chars"), new Functor1("string", S), S1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var S1 = arg6;
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", N))) {
      for each (var l3 in cannot_follow(Atom.a("number"), new Functor1("number", N), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg6, new ListPair(Atom.a("}"), S1))) {
        for each (var l4 in exprtl0_atom(Atom.a("{}"), Term, Precedence, Answer, S, S1)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in cannot_follow(Atom.a("brace"), Atom.a("{"), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg6, new ListPair(Atom.a("]"), S1))) {
        for each (var l4 in exprtl0_atom(Atom.NIL, Term, Precedence, Answer, S, S1)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in cannot_follow(Atom.a("bracket"), Atom.a("["), S1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1)) {
        yield false;
      }
    }
  }
  {
    var x4 = arg2;
    var x5 = arg3;
    var x6 = arg4;
    var x7 = arg5;
    var S1 = arg6;
    var A = new Variable();
    var B = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", A, B, P))) {
      for each (var l3 in cannot_follow(Atom.a("variable"), new Functor3("var", A, B, P), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var F = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", F, P))) {
      for each (var l3 in exprtl0_atom(new Functor2("/", F, P), Term, Precedence, Answer, S, S1)) {
        yield false;
      }
    }
  }
}

function exprtl0_atom(arg1, arg2, arg3, arg4, arg5, S1) {
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L1 = new Variable();
    var O1 = new Variable();
    var R1 = new Variable();
    var L2 = new Variable();
    var O2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in ambigop(F, Precedence, L1, O1, R1, L2, O2)) {
        for each (var l4 in prefix_is_atom(S1, Precedence)) {
          for each (var l5 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
        for each (var l4 in exprtl(new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        for each (var l4 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L1 = new Variable();
    var O1 = new Variable();
    var R1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in infixop(F, L1, O1, R1)) {
        for each (var l4 in exprtl(new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L2 = new Variable();
    var O2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in postfixop(F, L2, O2)) {
        for each (var l4 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var X = arg1;
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var x7 = new Variable();
    for each (var l2 in syntax_error(ListPair.make([new Functor2("-", Atom.a("non"), Atom.a("operator")), X, Atom.a("follows"), Atom.a("expression")]), new ListPair(new Functor2("atom", X, x7), S1))) {
      yield false;
    }
    return;
  }
}

function cannot_follow(Type, Token, Tokens) {
  {
    for each (var l2 in syntax_error(ListPair.make([Type, Atom.a("follows"), Atom.a("expression")]), new ListPair(Token, Tokens))) {
      yield false;
    }
  }
}

function exprtl(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x1 = arg2;
    var x3 = arg4;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Term)) {
        for each (var l4 in YP.unify(arg5, Term)) {
          for each (var l5 in YP.unify(arg6, Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var Token = new Variable();
    var Tokens = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, Tokens))) {
      for each (var l3 in exprtl_7(Token, C, Term, Precedence, Answer, S, Tokens)) {
        yield false;
      }
    }
  }
}

function exprtl_7(arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var F = new Variable();
    var Pos = new Variable();
    var L = new Variable();
    var O = new Variable();
    var R = new Variable();
    var Other = new Variable();
    var S2 = new Variable();
    var Expr = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L, O, R]))) {
      if (YP.greaterThanOrEqual(Precedence, O)) {
        if (YP.lessThanOrEqual(C, L)) {
          for each (var l5 in parse(S1, R, Other, S2)) {
            for each (var l6 in YP.univ(Expr, ListPair.make([F, Pos, Term, Other]))) {
              for each (var l7 in exprtl(S2, O, Expr, Precedence, Answer, S)) {
                yield false;
              }
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var F = new Variable();
    var Pos = new Variable();
    var L = new Variable();
    var O = new Variable();
    var Expr = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L, O))) {
      if (YP.greaterThanOrEqual(Precedence, O)) {
        if (YP.lessThanOrEqual(C, L)) {
          for each (var l5 in YP.univ(Expr, ListPair.make([F, Pos, Term]))) {
            for each (var l6 in peepop(S1, S2)) {
              for each (var l7 in exprtl(S2, O, Expr, Precedence, Answer, S)) {
                yield false;
              }
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      if (YP.greaterThanOrEqual(Precedence, 1000)) {
        if (YP.lessThan(C, 1000)) {
          for each (var l5 in parse(S1, 1000, Next, S2)) {
            for each (var l6 in exprtl(S2, 1000, new Functor2(",", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      if (YP.greaterThanOrEqual(Precedence, 1100)) {
        if (YP.lessThan(C, 1100)) {
          for each (var l5 in parse(S1, 1100, Next, S2)) {
            for each (var l6 in exprtl(S2, 1100, new Functor2(";", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Token = arg1;
    var x2 = arg2;
    var x4 = arg4;
    var Tokens = arg7;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg3, Term)) {
      for each (var l3 in YP.unify(arg5, Term)) {
        for each (var l4 in YP.unify(arg6, new ListPair(Token, Tokens))) {
          yield false;
        }
      }
    }
  }
}

function syntax_error(_Message, _List) {
  {
    return;
  }
  {
    for each (var l2 in YP.fail()) {
      yield false;
    }
  }
}

function syntax_error1(_List) {
  {
    return;
  }
  {
    for each (var l2 in YP.fail()) {
      yield false;
    }
  }
}

function prefixop(F, O, Q) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("fx"), F)) {
        for each (var l4 in YP.unify(Q, YP.subtract(O, 1))) {
          yield false;
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("fy"), F)) {
          for each (var l5 in YP.unify(Q, O)) {
            yield false;
          }
          break cutIf2;
        }
      }
    }
  }
}

function postfixop(F, P, O) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("xf"), F)) {
        for each (var l4 in YP.unify(P, YP.subtract(O, 1))) {
          yield false;
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("yf"), F)) {
          for each (var l5 in YP.unify(P, O)) {
            yield false;
          }
          break cutIf2;
        }
      }
    }
  }
}

function infixop(F, P, O, Q) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("xfy"), F)) {
        for each (var l4 in YP.unify(P, YP.subtract(O, 1))) {
          for each (var l5 in YP.unify(Q, O)) {
            yield false;
          }
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("xfx"), F)) {
          for each (var l5 in YP.unify(P, YP.subtract(O, 1))) {
            for each (var l6 in YP.unify(Q, P)) {
              yield false;
            }
          }
          break cutIf2;
        }
        cutIf3:
        {
          for each (var l5 in YP.current_op(O, Atom.a("yfx"), F)) {
            for each (var l6 in YP.unify(Q, YP.subtract(O, 1))) {
              for each (var l7 in YP.unify(P, O)) {
                yield false;
              }
            }
            break cutIf3;
          }
        }
      }
    }
  }
}

function ambigop(F, Precedence, L1, O1, R1, L2, O2) {
  {
    for each (var l2 in postfixop(F, L2, O2)) {
      if (YP.lessThanOrEqual(O2, Precedence)) {
        for each (var l4 in infixop(F, L1, O1, R1)) {
          if (YP.lessThanOrEqual(O1, Precedence)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_tokens1(arg1) {
  {
    var TokenList = arg1;
    var C1 = new Variable();
    var _X = new Variable();
    var ListOfTokens = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      for each (var l3 in read_tokens(C1, _X, ListOfTokens)) {
        for each (var l4 in YP.unify(TokenList, ListOfTokens)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL))) {
      yield false;
    }
  }
}

function read_tokens2(arg1, arg2) {
  {
    var TokenList = arg1;
    var Dictionary = arg2;
    var C1 = new Variable();
    var Dict = new Variable();
    var ListOfTokens = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      for each (var l3 in read_tokens(C1, Dict, ListOfTokens)) {
        for each (var l4 in terminate_list(Dict)) {
          for each (var l5 in YP.unify(Dictionary, Dict)) {
            for each (var l6 in YP.unify(TokenList, ListOfTokens)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield false;
      }
    }
  }
}

function terminate_list(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
  {
    var x1 = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, Tail))) {
      for each (var l3 in terminate_list(Tail)) {
        yield false;
      }
    }
  }
}

function read_special(arg1, Dict, arg3) {
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 95)) {
      for each (var l3 in read_variable(95, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 247)) {
      for each (var l3 in read_symbol(247, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 215)) {
      for each (var l3 in read_symbol(215, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var StartPos = new Variable();
    var EndPos = new Variable();
    var Tokens = new Variable();
    var Ch = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 37)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("comment", StartPos, EndPos), Tokens))) {
        for each (var l4 in get_current_position(StartPos)) {
          for each (var l5 in YP.repeat()) {
            for each (var l6 in YP.get_code(Ch)) {
              if (YP.lessThan(Ch, new ListPair(32, Atom.NIL))) {
                if (YP.notEqual(Ch, 9)) {
                  if (YP.termNotEqual(Ch, -1)) {
                    for each (var l10 in get_current_position(EndPos)) {
                      for each (var l11 in YP.get_code(NextCh)) {
                        for each (var l12 in read_tokens(NextCh, Dict, Tokens)) {
                          yield false;
                        }
                      }
                    }
                  }
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
    var T = arg3;
    var C2 = new Variable();
    var StartPos = new Variable();
    var EndPos = new Variable();
    var Tokens = new Variable();
    var StartPos1 = new Variable();
    var NextCh = new Variable();
    var Chars = new Variable();
    for each (var l2 in YP.unify(arg1, 47)) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.equal(C2, new ListPair(42, Atom.NIL))) {
            for each (var l6 in YP.unify(T, new ListPair(new Functor2("comment", StartPos, EndPos), Tokens))) {
              for each (var l7 in get_current_position(StartPos1)) {
                for each (var l8 in YP.unify(StartPos, YP.subtract(StartPos1, 1))) {
                  for each (var l9 in read_solidus(32, NextCh)) {
                    for each (var l10 in get_current_position(EndPos)) {
                      for each (var l11 in read_tokens(NextCh, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
          for each (var l5 in YP.unify(T, Tokens)) {
            for each (var l6 in rest_symbol(C2, Chars, NextCh)) {
              for each (var l7 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(47, Chars))) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 33)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom.a("!"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 40)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(" ("), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 41)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(")"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 44)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(","), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 59)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom.a(";"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 91)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("["), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_tokens(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 93)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("]"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 123)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("{"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_tokens(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 124)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("|"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 125)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("}"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_after_atom(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = arg3;
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 46)) {
      for each (var l3 in YP.get_code(NextCh)) {
        for each (var l4 in read_fullstop(NextCh, Dict, Tokens)) {
          yield false;
        }
      }
    }
  }
  {
    var Chars = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 34)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("string", Chars), Tokens))) {
        for each (var l4 in read_string(Chars, 34, NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = arg3;
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 39)) {
      for each (var l3 in read_string(Chars, 39, NextCh)) {
        for each (var l4 in read_after_atom4(NextCh, Dict, Tokens, Chars)) {
          yield false;
        }
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 35)) {
      for each (var l3 in read_symbol(35, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 36)) {
      for each (var l3 in read_symbol(36, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 38)) {
      for each (var l3 in read_symbol(38, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 42)) {
      for each (var l3 in read_symbol(42, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 43)) {
      for each (var l3 in read_symbol(43, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 45)) {
      for each (var l3 in read_symbol(45, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 58)) {
      for each (var l3 in read_symbol(58, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 60)) {
      for each (var l3 in read_symbol(60, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 61)) {
      for each (var l3 in read_symbol(61, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 62)) {
      for each (var l3 in read_symbol(62, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 63)) {
      for each (var l3 in read_symbol(63, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 64)) {
      for each (var l3 in read_symbol(64, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 92)) {
      for each (var l3 in read_symbol(92, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 94)) {
      for each (var l3 in read_symbol(94, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 96)) {
      for each (var l3 in read_symbol(96, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 126)) {
      for each (var l3 in read_symbol(126, Dict, Tokens)) {
        yield false;
      }
    }
  }
}

function read_symbol(C1, Dict, Tokens) {
  {
    var C2 = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.get_code(C2)) {
      for each (var l3 in rest_symbol(C2, Chars, NextCh)) {
        for each (var l4 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(C1, Chars))) {
          yield false;
        }
      }
    }
  }
}

function rest_symbol(arg1, arg2, arg3) {
  {
    var C2 = arg1;
    var LastCh = arg3;
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C2, Chars))) {
      cutIf1:
      {
        if (YP.greaterThan(C2, 160)) {
          if (YP.lessThan(C2, 192)) {
            if (YP.notEqual(C2, 186)) {
              if (YP.notEqual(C2, 170)) {
                for each (var l8 in YP.get_code(NextCh)) {
                  for each (var l9 in rest_symbol(NextCh, Chars, LastCh)) {
                    yield false;
                  }
                }
                return;
              }
            }
          }
          break cutIf1;
        }
        for each (var l4 in symbol_char(C2)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in rest_symbol(NextCh, Chars, LastCh)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg1, C2)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, C2)) {
          yield false;
        }
      }
    }
  }
}

function symbol_char(arg1) {
  {
    for each (var l2 in YP.unify(arg1, 35)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 36)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 38)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 42)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 43)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 45)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 46)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 47)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 58)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 60)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 61)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 62)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 63)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 64)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 92)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 94)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 96)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 126)) {
      yield false;
    }
  }
}

function get_current_position(Pos) {
  {
    for each (var l2 in YP.unify(Pos, 0)) {
      yield false;
    }
  }
}

function read_after_atom4(Ch, Dict, arg3, Chars) {
  {
    var Atom_1 = new Variable();
    var Pos = new Variable();
    var Tokens = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom_1, Pos), Tokens))) {
      for each (var l3 in YP.unify(Pos, 0)) {
        for each (var l4 in YP.atom_codes(Atom_1, Chars)) {
          for each (var l5 in read_after_atom(Ch, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_after_atom(arg1, Dict, arg3) {
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 40)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("("), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Ch = arg1;
    var Tokens = arg3;
    for each (var l2 in read_tokens(Ch, Dict, Tokens)) {
      yield false;
    }
  }
}

function read_string(Chars, Quote, NextCh) {
  {
    var Ch = new Variable();
    var Char = new Variable();
    var Next = new Variable();
    for each (var l2 in YP.get_code(Ch)) {
      for each (var l3 in read_char(Ch, Quote, Char, Next)) {
        for each (var l4 in rest_string5(Char, Next, Chars, Quote, NextCh)) {
          yield false;
        }
      }
    }
  }
}

function rest_string5(arg1, arg2, arg3, arg4, arg5) {
  {
    var _X = arg4;
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, -1)) {
      for each (var l3 in YP.unify(arg2, NextCh)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          for each (var l5 in YP.unify(arg5, NextCh)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Char = arg1;
    var Next = arg2;
    var Quote = arg4;
    var NextCh = arg5;
    var Chars = new Variable();
    var Char2 = new Variable();
    var Next2 = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(Char, Chars))) {
      for each (var l3 in read_char(Next, Quote, Char2, Next2)) {
        for each (var l4 in rest_string5(Char2, Next2, Chars, Quote, NextCh)) {
          yield false;
        }
      }
    }
  }
}

function escape_char(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, 110)) {
      for each (var l3 in YP.unify(arg2, 10)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 78)) {
      for each (var l3 in YP.unify(arg2, 10)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 116)) {
      for each (var l3 in YP.unify(arg2, 9)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 84)) {
      for each (var l3 in YP.unify(arg2, 9)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 114)) {
      for each (var l3 in YP.unify(arg2, 13)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 82)) {
      for each (var l3 in YP.unify(arg2, 13)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 118)) {
      for each (var l3 in YP.unify(arg2, 11)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 86)) {
      for each (var l3 in YP.unify(arg2, 11)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 98)) {
      for each (var l3 in YP.unify(arg2, 8)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 66)) {
      for each (var l3 in YP.unify(arg2, 8)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 102)) {
      for each (var l3 in YP.unify(arg2, 12)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 70)) {
      for each (var l3 in YP.unify(arg2, 12)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 101)) {
      for each (var l3 in YP.unify(arg2, 27)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 69)) {
      for each (var l3 in YP.unify(arg2, 27)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 100)) {
      for each (var l3 in YP.unify(arg2, 127)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 68)) {
      for each (var l3 in YP.unify(arg2, 127)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 115)) {
      for each (var l3 in YP.unify(arg2, 32)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 83)) {
      for each (var l3 in YP.unify(arg2, 32)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 122)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 90)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
}

function read_variable(C1, Dict, arg3) {
  {
    var Var = new Variable();
    var Name = new Variable();
    var StartPos = new Variable();
    var Tokens = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor3("var", Var, Name, StartPos), Tokens))) {
      for each (var l3 in get_current_position(StartPos)) {
        for each (var l4 in read_name(C1, Chars, NextCh)) {
          for each (var l5 in YP.atom_codes(Name, Chars)) {
            cutIf1:
            {
              if (YP.termEqual(Name, Atom.a("_"))) {
                for each (var l8 in read_after_atom(NextCh, Dict, Tokens)) {
                  yield false;
                }
                break cutIf1;
              }
              for each (var l7 in read_lookup(Dict, Name, Var)) {
                for each (var l8 in read_after_atom(NextCh, Dict, Tokens)) {
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

function read_lookup(arg1, Name, Var) {
  {
    var N = new Variable();
    var V = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", N, V), L))) {
      cutIf1:
      {
        for each (var l4 in YP.unify(N, Name)) {
          for each (var l5 in YP.unify(V, Var)) {
            yield false;
          }
          break cutIf1;
        }
        for each (var l4 in read_lookup(L, Name, Var)) {
          yield false;
        }
      }
    }
  }
}

function read_solidus(Ch, LastCh) {
  {
    var NextCh = new Variable();
    cutIf1:
    {
      if (YP.equal(Ch, 42)) {
        for each (var l4 in YP.get_code(NextCh)) {
          cutIf2:
          {
            if (YP.equal(NextCh, 47)) {
              for each (var l7 in YP.get_code(LastCh)) {
                yield false;
              }
              break cutIf2;
            }
            for each (var l6 in read_solidus(NextCh, LastCh)) {
              yield false;
            }
          }
        }
        break cutIf1;
      }
      cutIf3:
      {
        if (YP.notEqual(Ch, -1)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_solidus(NextCh, LastCh)) {
              yield false;
            }
          }
          break cutIf3;
        }
        for each (var l4 in YP.unify(LastCh, Ch)) {
          for each (var l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in /*comment~n"), Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_identifier(C1, Dict, Tokens) {
  {
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in read_name(C1, Chars, NextCh)) {
      for each (var l3 in read_after_atom4(NextCh, Dict, Tokens, Chars)) {
        yield false;
      }
    }
  }
}

function read_name(C1, arg2, LastCh) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(97, Atom.NIL))) {
            cutIf2:
            {
              if (YP.lessThanOrEqual(C2, new ListPair(122, Atom.NIL))) {
                for each (var l8 in read_name(C2, Chars, LastCh)) {
                  yield false;
                }
                break cutIf2;
              }
              cutIf3:
              {
                if (YP.lessThan(C2, 192)) {
                  if (YP.notEqual(YP.bitwiseOr(C2, 16), 186)) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf3;
                  }
                }
                cutIf4:
                {
                  if (YP.equal(YP.bitwiseOr(C2, 32), 247)) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf4;
                  }
                  for each (var l9 in read_name(C2, Chars, LastCh)) {
                    yield false;
                  }
                }
              }
            }
            break cutIf1;
          }
          cutIf5:
          {
            if (YP.greaterThanOrEqual(C2, new ListPair(65, Atom.NIL))) {
              cutIf6:
              {
                if (YP.greaterThan(C2, new ListPair(90, Atom.NIL))) {
                  if (YP.notEqual(C2, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf6;
                  }
                }
                for each (var l8 in read_name(C2, Chars, LastCh)) {
                  yield false;
                }
              }
              break cutIf5;
            }
            cutIf7:
            {
              if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
                if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
                  for each (var l9 in read_name(C2, Chars, LastCh)) {
                    yield false;
                  }
                  break cutIf7;
                }
              }
              for each (var l7 in YP.unify(Chars, Atom.NIL)) {
                for each (var l8 in YP.unify(LastCh, C2)) {
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

function read_fullstop(Ch, Dict, Tokens) {
  {
    var Number = new Variable();
    var Tokens1 = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    cutIf1:
    {
      if (YP.lessThanOrEqual(Ch, new ListPair(57, Atom.NIL))) {
        if (YP.greaterThanOrEqual(Ch, new ListPair(48, Atom.NIL))) {
          for each (var l5 in YP.unify(Tokens, new ListPair(new Functor1("number", Number), Tokens1))) {
            for each (var l6 in read_float(Number, Dict, Tokens1, new ListPair(48, Atom.NIL), Ch)) {
              yield false;
            }
          }
          break cutIf1;
        }
      }
      cutIf2:
      {
        if (YP.greaterThan(Ch, new ListPair(32, Atom.NIL))) {
          for each (var l5 in rest_symbol(Ch, Chars, NextCh)) {
            for each (var l6 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(46, Chars))) {
              yield false;
            }
          }
          break cutIf2;
        }
        cutIf3:
        {
          if (YP.greaterThanOrEqual(Ch, 0)) {
            for each (var l6 in YP.unify(Tokens, Atom.NIL)) {
              yield false;
            }
            break cutIf3;
          }
          for each (var l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file just after full stop~n"), Atom.NIL)) {
          }
        }
      }
    }
  }
}

function read_float(Number, Dict, Tokens, Digits, Digit) {
  {
    var Chars = new Variable();
    var Rest = new Variable();
    var NextCh = new Variable();
    for each (var l2 in prepend(Digits, Chars, Rest)) {
      for each (var l3 in read_float4(Digit, Rest, NextCh, Chars)) {
        for each (var l4 in YP.number_codes(Number, Chars)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
}

function prepend(arg1, arg2, arg3) {
  {
    var X = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, new ListPair(46, X))) {
        yield false;
      }
    }
  }
  {
    var Y = arg3;
    var C = new Variable();
    var Cs = new Variable();
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(C, Cs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(C, X))) {
        for each (var l4 in prepend(Cs, X, Y)) {
          yield false;
        }
      }
    }
  }
}

function read_float4(C1, arg2, NextCh, Total) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    var C3 = new Variable();
    var C4 = new Variable();
    var More = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
            if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
              for each (var l7 in read_float4(C2, Chars, NextCh, Total)) {
                yield false;
              }
              break cutIf1;
            }
          }
          cutIf2:
          {
            if (YP.equal(YP.bitwiseOr(C2, 32), new ListPair(101, Atom.NIL))) {
              for each (var l7 in YP.get_code(C3)) {
                cutIf3:
                {
                  if (YP.equal(C3, new ListPair(45, Atom.NIL))) {
                    for each (var l10 in YP.get_code(C4)) {
                      for each (var l11 in YP.unify(Chars, new ListPair(C2, new ListPair(45, More)))) {
                        cutIf4:
                        {
                          if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                            if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                              for each (var l15 in read_exponent(C4, More, NextCh)) {
                                yield false;
                              }
                              break cutIf4;
                            }
                          }
                          for each (var l13 in YP.unify(More, Atom.NIL)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                            }
                          }
                          for each (var l13 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                            for each (var l14 in YP.unify(NextCh, C4)) {
                              yield false;
                            }
                          }
                        }
                      }
                    }
                    break cutIf3;
                  }
                  cutIf5:
                  {
                    if (YP.equal(C3, new ListPair(43, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C4)) {
                        for each (var l12 in YP.unify(Chars, new ListPair(C2, More))) {
                          cutIf6:
                          {
                            if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                              if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                                for each (var l16 in read_exponent(C4, More, NextCh)) {
                                  yield false;
                                }
                                break cutIf6;
                              }
                            }
                            for each (var l14 in YP.unify(More, Atom.NIL)) {
                              for each (var l15 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                              }
                            }
                            for each (var l14 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                              for each (var l15 in YP.unify(NextCh, C4)) {
                                yield false;
                              }
                            }
                          }
                        }
                      }
                      break cutIf5;
                    }
                    for each (var l10 in YP.unify(C4, C3)) {
                      for each (var l11 in YP.unify(Chars, new ListPair(C2, More))) {
                        cutIf7:
                        {
                          if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                            if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                              for each (var l15 in read_exponent(C4, More, NextCh)) {
                                yield false;
                              }
                              break cutIf7;
                            }
                          }
                          for each (var l13 in YP.unify(More, Atom.NIL)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                            }
                          }
                          for each (var l13 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                            for each (var l14 in YP.unify(NextCh, C4)) {
                              yield false;
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
            for each (var l6 in YP.unify(Chars, Atom.NIL)) {
              for each (var l7 in YP.unify(NextCh, C2)) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
}

function read_exponent(C1, arg2, NextCh) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
            if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
              for each (var l7 in read_exponent(C2, Chars, NextCh)) {
                yield false;
              }
              break cutIf1;
            }
          }
          for each (var l5 in YP.unify(Chars, Atom.NIL)) {
            for each (var l6 in YP.unify(NextCh, C2)) {
              yield false;
            }
          }
        }
      }
    }
  }
}

function read_number(C1, Dict, arg3) {
  {
    var Number = new Variable();
    var Tokens = new Variable();
    var C2 = new Variable();
    var N = new Variable();
    var C = new Variable();
    var C3 = new Variable();
    var Digits = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor1("number", Number), Tokens))) {
      for each (var l3 in read_number4(C1, C2, 0, N)) {
        cutIf1:
        {
          if (YP.equal(C2, 39)) {
            cutIf2:
            {
              if (YP.greaterThanOrEqual(N, 2)) {
                if (YP.lessThanOrEqual(N, 36)) {
                  for each (var l9 in read_based(N, 0, Number, C)) {
                    for each (var l10 in read_tokens(C, Dict, Tokens)) {
                      yield false;
                    }
                  }
                  break cutIf2;
                }
              }
              cutIf3:
              {
                if (YP.equal(N, 0)) {
                  for each (var l9 in YP.get_code(C3)) {
                    for each (var l10 in read_char(C3, -1, Number, C)) {
                      for each (var l11 in read_tokens(C, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                  break cutIf3;
                }
                for each (var l8 in formatError(Atom.a("user_error"), Atom.a("~N** ~d' read as ~d '~n"), new ListPair(N, new ListPair(N, Atom.NIL)))) {
                  for each (var l9 in YP.unify(Number, N)) {
                    for each (var l10 in YP.unify(C, C2)) {
                      for each (var l11 in read_tokens(C, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
          cutIf4:
          {
            if (YP.equal(C2, 46)) {
              for each (var l7 in YP.get_code(C3)) {
                cutIf5:
                {
                  if (YP.greaterThanOrEqual(C3, new ListPair(48, Atom.NIL))) {
                    if (YP.lessThanOrEqual(C3, new ListPair(57, Atom.NIL))) {
                      for each (var l11 in YP.number_codes(N, Digits)) {
                        for each (var l12 in read_float(Number, Dict, Tokens, Digits, C3)) {
                          yield false;
                        }
                      }
                      break cutIf5;
                    }
                  }
                  for each (var l9 in YP.unify(Number, N)) {
                    for each (var l10 in read_fullstop(C3, Dict, Tokens)) {
                      yield false;
                    }
                  }
                }
              }
              break cutIf4;
            }
            for each (var l6 in YP.unify(Number, N)) {
              for each (var l7 in read_tokens(C2, Dict, Tokens)) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
}

function read_number4(C0, C, N0, N) {
  {
    var N1 = new Variable();
    var C1 = new Variable();
    cutIf1:
    {
      if (YP.greaterThanOrEqual(C0, new ListPair(48, Atom.NIL))) {
        if (YP.lessThanOrEqual(C0, new ListPair(57, Atom.NIL))) {
          for each (var l5 in YP.unify(N1, YP.add(YP.subtract(YP.multiply(N0, 10), new ListPair(48, Atom.NIL)), C0))) {
            for each (var l6 in YP.get_code(C1)) {
              for each (var l7 in read_number4(C1, C, N1, N)) {
                yield false;
              }
            }
          }
          break cutIf1;
        }
      }
      cutIf2:
      {
        if (YP.equal(C0, 95)) {
          for each (var l5 in YP.get_code(C1)) {
            for each (var l6 in read_number4(C1, C, N0, N)) {
              yield false;
            }
          }
          break cutIf2;
        }
        for each (var l4 in YP.unify(C, C0)) {
          for each (var l5 in YP.unify(N, N0)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_based(Base, N0, N, C) {
  {
    var C1 = new Variable();
    var Digit = new Variable();
    var N1 = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      cutIf1:
      {
        if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
          if (YP.lessThanOrEqual(C1, new ListPair(57, Atom.NIL))) {
            for each (var l6 in YP.unify(Digit, YP.subtract(C1, new ListPair(48, Atom.NIL)))) {
              cutIf2:
              {
                if (YP.lessThan(Digit, Base)) {
                  for each (var l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                    for each (var l10 in read_based(Base, N1, N, C)) {
                      yield false;
                    }
                  }
                  break cutIf2;
                }
                cutIf3:
                {
                  if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in read_based(Base, N0, N, C)) {
                      yield false;
                    }
                    break cutIf3;
                  }
                  for each (var l9 in YP.unify(N, N0)) {
                    for each (var l10 in YP.unify(C, C1)) {
                      yield false;
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
        }
        cutIf4:
        {
          if (YP.greaterThanOrEqual(C1, new ListPair(65, Atom.NIL))) {
            if (YP.lessThanOrEqual(C1, new ListPair(90, Atom.NIL))) {
              for each (var l7 in YP.unify(Digit, YP.subtract(C1, YP.subtract(new ListPair(65, Atom.NIL), 10)))) {
                cutIf5:
                {
                  if (YP.lessThan(Digit, Base)) {
                    for each (var l10 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                      for each (var l11 in read_based(Base, N1, N, C)) {
                        yield false;
                      }
                    }
                    break cutIf5;
                  }
                  cutIf6:
                  {
                    if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                      for each (var l11 in read_based(Base, N0, N, C)) {
                        yield false;
                      }
                      break cutIf6;
                    }
                    for each (var l10 in YP.unify(N, N0)) {
                      for each (var l11 in YP.unify(C, C1)) {
                        yield false;
                      }
                    }
                  }
                }
              }
              break cutIf4;
            }
          }
          cutIf7:
          {
            if (YP.greaterThanOrEqual(C1, new ListPair(97, Atom.NIL))) {
              if (YP.lessThanOrEqual(C1, new ListPair(122, Atom.NIL))) {
                for each (var l8 in YP.unify(Digit, YP.subtract(C1, YP.subtract(new ListPair(97, Atom.NIL), 10)))) {
                  cutIf8:
                  {
                    if (YP.lessThan(Digit, Base)) {
                      for each (var l11 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                        for each (var l12 in read_based(Base, N1, N, C)) {
                          yield false;
                        }
                      }
                      break cutIf8;
                    }
                    cutIf9:
                    {
                      if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                        for each (var l12 in read_based(Base, N0, N, C)) {
                          yield false;
                        }
                        break cutIf9;
                      }
                      for each (var l11 in YP.unify(N, N0)) {
                        for each (var l12 in YP.unify(C, C1)) {
                          yield false;
                        }
                      }
                    }
                  }
                }
                break cutIf7;
              }
            }
            for each (var l6 in YP.unify(Digit, 99)) {
              cutIf10:
              {
                if (YP.lessThan(Digit, Base)) {
                  for each (var l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                    for each (var l10 in read_based(Base, N1, N, C)) {
                      yield false;
                    }
                  }
                  break cutIf10;
                }
                cutIf11:
                {
                  if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in read_based(Base, N0, N, C)) {
                      yield false;
                    }
                    break cutIf11;
                  }
                  for each (var l9 in YP.unify(N, N0)) {
                    for each (var l10 in YP.unify(C, C1)) {
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

function read_char(Char, Quote, Result, Next) {
  {
    var C1 = new Variable();
    var C2 = new Variable();
    var C3 = new Variable();
    var Ch = new Variable();
    cutIf1:
    {
      if (YP.equal(Char, 92)) {
        for each (var l4 in YP.get_code(C1)) {
          cutIf2:
          {
            if (YP.lessThan(C1, 0)) {
              for each (var l7 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~cquoted~c~n"), new ListPair(Quote, new ListPair(Quote, Atom.NIL)))) {
                for each (var l8 in YP.unify(Result, -1)) {
                  for each (var l9 in YP.unify(Next, C1)) {
                    yield false;
                  }
                }
              }
              break cutIf2;
            }
            cutIf3:
            {
              if (YP.lessThanOrEqual(C1, new ListPair(32, Atom.NIL))) {
                for each (var l8 in YP.get_code(C2)) {
                  for each (var l9 in read_char(C2, Quote, Result, Next)) {
                    yield false;
                  }
                }
                break cutIf3;
              }
              cutIf4:
              {
                if (YP.equal(YP.bitwiseOr(C1, 32), new ListPair(99, Atom.NIL))) {
                  for each (var l9 in YP.get_code(C2)) {
                    for each (var l10 in read_char(C2, Quote, Result, Next)) {
                      yield false;
                    }
                  }
                  break cutIf4;
                }
                cutIf5:
                {
                  if (YP.lessThanOrEqual(C1, new ListPair(55, Atom.NIL))) {
                    if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C2)) {
                        cutIf6:
                        {
                          if (YP.lessThanOrEqual(C2, new ListPair(55, Atom.NIL))) {
                            if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
                              for each (var l15 in YP.get_code(C3)) {
                                cutIf7:
                                {
                                  if (YP.lessThanOrEqual(C3, new ListPair(55, Atom.NIL))) {
                                    if (YP.greaterThanOrEqual(C3, new ListPair(48, Atom.NIL))) {
                                      for each (var l19 in YP.get_code(Next)) {
                                        for each (var l20 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(YP.add(YP.multiply(C1, 8), C2), 8), C3), YP.multiply(73, new ListPair(48, Atom.NIL))))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf7;
                                    }
                                  }
                                  for each (var l17 in YP.unify(Next, C3)) {
                                    for each (var l18 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(C1, 8), C2), YP.multiply(9, new ListPair(48, Atom.NIL))))) {
                                      yield false;
                                    }
                                  }
                                }
                              }
                              break cutIf6;
                            }
                          }
                          for each (var l13 in YP.unify(Next, C2)) {
                            for each (var l14 in YP.unify(Result, YP.subtract(C1, new ListPair(48, Atom.NIL)))) {
                              yield false;
                            }
                          }
                        }
                      }
                      break cutIf5;
                    }
                  }
                  cutIf8:
                  {
                    if (YP.equal(C1, new ListPair(94, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C2)) {
                        cutIf9:
                        {
                          if (YP.lessThan(C2, 0)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~c..~c^..~c~n"), ListPair.make([Quote, 92, Quote]))) {
                              for each (var l15 in YP.unify(Result, -1)) {
                                for each (var l16 in YP.unify(Next, C2)) {
                                  yield false;
                                }
                              }
                            }
                            break cutIf9;
                          }
                          cutIf10:
                          {
                            if (YP.equal(C2, new ListPair(63, Atom.NIL))) {
                              for each (var l15 in YP.unify(Result, 127)) {
                                for each (var l16 in YP.get_code(Next)) {
                                  yield false;
                                }
                              }
                              break cutIf10;
                            }
                            for each (var l14 in YP.unify(Result, YP.bitwiseAnd(C2, 31))) {
                              for each (var l15 in YP.get_code(Next)) {
                                yield false;
                              }
                            }
                          }
                        }
                      }
                      break cutIf8;
                    }
                    cutIf11:
                    {
                      for each (var l11 in escape_char(C1, Result)) {
                        for each (var l12 in YP.get_code(Next)) {
                          yield false;
                        }
                        break cutIf11;
                      }
                      for each (var l11 in YP.unify(Result, C1)) {
                        for each (var l12 in YP.get_code(Next)) {
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
        break cutIf1;
      }
      cutIf12:
      {
        if (YP.equal(Char, Quote)) {
          for each (var l5 in YP.get_code(Ch)) {
            cutIf13:
            {
              if (YP.equal(Ch, Quote)) {
                for each (var l8 in YP.unify(Result, Quote)) {
                  for each (var l9 in YP.get_code(Next)) {
                    yield false;
                  }
                }
                break cutIf13;
              }
              for each (var l7 in YP.unify(Result, -1)) {
                for each (var l8 in YP.unify(Next, Ch)) {
                  yield false;
                }
              }
            }
          }
          break cutIf12;
        }
        cutIf14:
        {
          if (YP.lessThan(Char, new ListPair(32, Atom.NIL))) {
            if (YP.notEqual(Char, 9)) {
              if (YP.notEqual(Char, 10)) {
                if (YP.notEqual(Char, 13)) {
                  for each (var l9 in YP.unify(Result, -1)) {
                    for each (var l10 in YP.unify(Next, Char)) {
                      for each (var l11 in formatError(Atom.a("user_error"), Atom.a("~N** Strange character ~d ends ~ctoken~c~n"), ListPair.make([Char, Quote, Quote]))) {
                        yield false;
                      }
                    }
                  }
                  break cutIf14;
                }
              }
            }
          }
          for each (var l5 in YP.unify(Result, Char)) {
            for each (var l6 in YP.get_code(Next)) {
              yield false;
            }
          }
        }
      }
    }
  }
}


