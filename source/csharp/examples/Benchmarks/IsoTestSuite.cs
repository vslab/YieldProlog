using System; 
using System.Collections.Generic;
using YieldProlog;

namespace YieldPrologBenchmarks
{
    public class IsoTestSuite
    {
        static void Main(string[] args)
        {
            long startTicks = DateTime.Now.Ticks;
            int nAnswers = 0;
            foreach (bool l1 in run_all_tests())
            {
                ++nAnswers;
            }
            long finishTicks = DateTime.Now.Ticks;
            Console.WriteLine("ISO Test Suite: " +
                (finishTicks - startTicks) / 10000000.0 + " seconds, " + nAnswers + " answers");

            Console.WriteLine("\nPress Enter to finish.");
            Console.ReadLine();
        }

        // Following is the compiled code from YieldProlog/source/prolog/isoTestSuite.P

        public class YPInnerClass { }
        public static Type getDeclaringClass() { return typeof(YPInnerClass).DeclaringType; }

        public static IEnumerable<bool> run_all_tests()
        {
            {
                Variable F = new Variable();
                Variable Files = new Variable();
                FindallAnswers findallAnswers1 = new FindallAnswers(F);
                foreach (bool l2 in file(F))
                {
                    findallAnswers1.add();
                }
                foreach (bool l2 in findallAnswers1.result(Files))
                {
                    foreach (bool l3 in test_all(Files))
                    {
                        foreach (bool l4 in write_results())
                        {
                            yield return true;
                            yield break;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> test_all(object arg1)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    yield return false;
                }
            }
            {
                Variable F = new Variable();
                Variable Fs = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(F, Fs)))
                {
                    foreach (bool l3 in run_tests(F))
                    {
                        foreach (bool l4 in test_all(Fs))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> write_results()
        {
            {
                Variable F = new Variable();
                Variable ErrorBips = new Variable();
                FindallAnswers findallAnswers1 = new FindallAnswers(F);
                foreach (bool l2 in inerror(F))
                {
                    findallAnswers1.add();
                }
                foreach (bool l2 in findallAnswers1.result(ErrorBips))
                {
                    YP.write(Atom.a("--------------------"));
                    YP.nl();
                    foreach (bool l3 in YP.unify(ErrorBips, Atom.NIL))
                    {
                        YP.write(Atom.a("All bips passed -------------"));
                        YP.nl();
                        yield return false;
                        goto cutIf2;
                    }
                    YP.nl();
                    YP.write(Atom.a("The following BIPs gave unexpected answers:"));
                    YP.nl();
                    YP.write(Atom.a("The results should be examined carefully."));
                    YP.nl();
                    YP.nl();
                    foreach (bool l3 in display_list(ErrorBips))
                    {
                        yield return false;
                    }
                cutIf2:
                    { }
                }
            }
        }

        public static IEnumerable<bool> result(object G, object Res)
        {
            {
                Variable Subs = new Variable();
                foreach (bool l2 in get_all_subs(G, Subs))
                {
                    foreach (bool l3 in special_ans_forms(Subs, Res))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> special_ans_forms(object arg1, object arg2)
        {
            {
                foreach (bool l2 in YP.unify(arg1, new ListPair(Atom.a("success"), Atom.NIL)))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.a("success")))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, new ListPair(Atom.a("failure"), Atom.NIL)))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.a("failure")))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object Error = arg2;
                Variable E = new Variable();
                Variable x3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(Error, Atom.NIL)))
                {
                    foreach (bool l3 in YP.univ(Error, new ListPair(E, x3)))
                    {
                        foreach (bool l4 in error_type(E))
                        {
                            yield return true;
                            yield break;
                        }
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, X))
                {
                    foreach (bool l3 in YP.unify(arg2, X))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> error_type(object arg1)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("instantiation_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("type_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("domain_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("existence_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("permission_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("representation_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("evaluation_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("resource_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("syntax_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("system_error")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("unexpected_ball")))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> vars_in_term(object T, object V)
        {
            {
                foreach (bool l2 in vars_in_term3(T, Atom.NIL, V))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> vars_in_term3(object arg1, object VarsIn, object arg3)
        {
            {
                object Term = arg1;
                object VarsOut = arg3;
                if (YP.atomic(Term))
                {
                    foreach (bool l3 in YP.unify(VarsOut, VarsIn))
                    {
                        yield return false;
                    }
                    yield break;
                }
            }
            {
                object Term = arg1;
                object VarsOut = arg3;
                if (YP.var(Term))
                {
                    foreach (bool l3 in already_appears(Term, VarsIn))
                    {
                        foreach (bool l4 in YP.unify(VarsOut, VarsIn))
                        {
                            yield return false;
                        }
                        goto cutIf1;
                    }
                    foreach (bool l3 in append(VarsIn, new ListPair(Term, Atom.NIL), VarsOut))
                    {
                        yield return false;
                    }
                cutIf1:
                    yield break;
                }
            }
            {
                object Vars = arg3;
                Variable A = new Variable();
                Variable B = new Variable();
                Variable V1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(A, B)))
                {
                    foreach (bool l3 in vars_in_term3(A, VarsIn, V1))
                    {
                        foreach (bool l4 in vars_in_term3(B, V1, Vars))
                        {
                            yield return false;
                        }
                    }
                    yield break;
                }
            }
            {
                object T = arg1;
                object VarList = arg3;
                Variable _F = new Variable();
                Variable A = new Variable();
                Variable Args = new Variable();
                Variable Inter = new Variable();
                foreach (bool l2 in YP.univ(T, new ListPair(_F, new ListPair(A, Args))))
                {
                    foreach (bool l3 in vars_in_term3(A, VarsIn, Inter))
                    {
                        foreach (bool l4 in vars_in_term3(Args, Inter, VarList))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> already_appears(object Var, object arg2)
        {
            {
                Variable V1 = new Variable();
                Variable _Vlist = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(V1, _Vlist)))
                {
                    if (YP.termEqual(Var, V1))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable _V1 = new Variable();
                Variable Vlist = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(_V1, Vlist)))
                {
                    foreach (bool l3 in already_appears(Var, Vlist))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> call_goal_get_subs(object G, object Sub)
        {
            {
                Variable GT = new Variable();
                Variable Vars = new Variable();
                Variable GVars = new Variable();
                foreach (bool l2 in YP.copy_term(G, GT))
                {
                    foreach (bool l3 in vars_in_term(G, Vars))
                    {
                        foreach (bool l4 in vars_in_term(GT, GVars))
                        {
                            foreach (bool l5 in YP.getIterator(GT, getDeclaringClass()))
                            {
                                foreach (bool l6 in make_subs_list1(Vars, GVars, Sub))
                                {
                                    yield return false;
                                }
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> make_subs_list1(object arg1, object arg2, object arg3)
        {
            {
                object _V = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("success")))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.a("success")))
                    {
                        yield return false;
                    }
                }
            }
            {
                object _V = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("failure")))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.a("failure")))
                    {
                        yield return false;
                    }
                }
            }
            {
                object _V = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("impl_def")))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.a("impl_def")))
                    {
                        yield return false;
                    }
                }
            }
            {
                object _V = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("undefined")))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.a("undefined")))
                    {
                        yield return false;
                    }
                }
            }
            {
                object _V = arg1;
                Variable Error = new Variable();
                Variable E = new Variable();
                Variable x4 = new Variable();
                foreach (bool l2 in YP.unify(arg2, Error))
                {
                    foreach (bool l3 in YP.unify(arg3, Error))
                    {
                        foreach (bool l4 in YP.univ(Error, new ListPair(E, x4)))
                        {
                            foreach (bool l5 in error_type(E))
                            {
                                yield return true;
                                yield break;
                            }
                        }
                    }
                }
            }
            {
                object Vs = arg1;
                object GVs = arg2;
                object Sub = arg3;
                Variable S = new Variable();
                foreach (bool l2 in make_subs_list(Vs, GVs, S))
                {
                    foreach (bool l3 in compress_sub_list(Vs, S, Sub))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> make_subs_list(object arg1, object arg2, object arg3)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.NIL))
                    {
                        foreach (bool l4 in YP.unify(arg3, Atom.NIL))
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                object Subs = arg3;
                Variable V = new Variable();
                Variable Rest = new Variable();
                Variable Ans = new Variable();
                Variable ARest = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(V, Rest)))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Ans, ARest)))
                    {
                        if (YP.termEqual(V, Ans))
                        {
                            foreach (bool l5 in make_subs_list(Rest, ARest, Subs))
                            {
                                yield return false;
                            }
                            yield break;
                        }
                    }
                }
            }
            {
                Variable V = new Variable();
                Variable Rest = new Variable();
                Variable Ans = new Variable();
                Variable ARest = new Variable();
                Variable SubsRest = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(V, Rest)))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Ans, ARest)))
                    {
                        foreach (bool l4 in YP.unify(arg3, new ListPair(new Functor2("<--", V, Ans), SubsRest)))
                        {
                            foreach (bool l5 in make_subs_list(Rest, ARest, SubsRest))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> list_make_subs_list(object arg1, object arg2, object arg3)
        {
            {
                object x1 = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(Atom.a("failure"), Atom.NIL)))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object V = arg1;
                object GTV = arg2;
                object S = arg3;
                foreach (bool l2 in list_make_subs_list_aux(V, GTV, S))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> list_make_subs_list_aux(object arg1, object arg2, object arg3)
        {
            {
                object _Vars = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object Vars = arg1;
                Variable GV1 = new Variable();
                Variable GVRest = new Variable();
                Variable Sub1 = new Variable();
                Variable SubRest = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(GV1, GVRest)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(Sub1, SubRest)))
                    {
                        foreach (bool l4 in make_subs_list1(Vars, GV1, Sub1))
                        {
                            foreach (bool l5 in list_make_subs_list_aux(Vars, GVRest, SubRest))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> call_with_result(object arg1, object arg2)
        {
            {
                object G = arg1;
                object R = arg2;
                Variable Sub = new Variable();
                foreach (bool l2 in call_goal_get_subs(G, Sub))
                {
                    foreach (bool l3 in YP.unify(Sub, Atom.NIL))
                    {
                        foreach (bool l4 in YP.unify(R, Atom.a("success")))
                        {
                            yield return false;
                        }
                        goto cutIf1;
                    }
                    foreach (bool l3 in YP.unify(R, Sub))
                    {
                        yield return false;
                    }
                cutIf1:
                    { }
                }
            }
            {
                object _G = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("failure")))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> protected_call_results(object G, object R)
        {
            {
                Variable B = new Variable();
                YP.Catch catchGoal1 = new YP.Catch(new Functor2(Atom.a("call_with_result", Atom.a("")), G, R), getDeclaringClass());
                foreach (bool l2 in catchGoal1)
                {
                    yield return false;
                }
                foreach (bool l2 in catchGoal1.unifyExceptionOrThrow(B))
                {
                    foreach (bool l3 in YP.unify(R, B))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> get_all_subs(object G, object AllSubs)
        {
            {
                Variable GT = new Variable();
                Variable GVars = new Variable();
                Variable GTAns = new Variable();
                Variable GTAnsList = new Variable();
                foreach (bool l2 in YP.copy_term(G, GT))
                {
                    foreach (bool l3 in vars_in_term(G, GVars))
                    {
                        FindallAnswers findallAnswers1 = new FindallAnswers(GTAns);
                        foreach (bool l4 in protect_call_result(GT, GTAns))
                        {
                            findallAnswers1.add();
                        }
                        foreach (bool l4 in findallAnswers1.result(GTAnsList))
                        {
                            foreach (bool l5 in list_make_subs_list(GVars, GTAnsList, AllSubs))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> call_result(object G, object R)
        {
            {
                Variable GVars = new Variable();
                foreach (bool l2 in vars_in_term(G, GVars))
                {
                    foreach (bool l3 in YP.getIterator(G, getDeclaringClass()))
                    {
                        foreach (bool l4 in YP.unify(R, GVars))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> protect_call_result(object G, object R)
        {
            {
                Variable B = new Variable();
                YP.Catch catchGoal1 = new YP.Catch(new Functor2(Atom.a("call_result", Atom.a("")), G, R), getDeclaringClass());
                foreach (bool l2 in catchGoal1)
                {
                    yield return false;
                }
                foreach (bool l2 in catchGoal1.unifyExceptionOrThrow(B))
                {
                    foreach (bool l3 in extract_error(B, R))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> extract_error(object arg1, object arg2)
        {
            {
                object R = arg2;
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("error", R, x2)))
                {
                    yield return true;
                    yield break;
                }
            }
            {
                object B = arg1;
                foreach (bool l2 in YP.unify(arg2, new Functor1("unexpected_ball", B)))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> compress_sub_list(object arg1, object arg2, object arg3)
        {
            {
                object x1 = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.a("success")))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object Vars = arg1;
                Variable X = new Variable();
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(new Functor2("<--", X, A), Atom.NIL)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(new Functor2("<--", X, A), Atom.NIL)))
                    {
                        if (YP.termNotEqual(X, A))
                        {
                            foreach (bool l5 in in_vars(A, Vars))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
            {
                object Vars = arg1;
                object LIn = arg2;
                object LOut = arg3;
                Variable X = new Variable();
                Variable A = new Variable();
                Variable Before = new Variable();
                Variable After = new Variable();
                Variable BN = new Variable();
                Variable AN = new Variable();
                Variable L1 = new Variable();
                foreach (bool l2 in split_list(new Functor2("<--", X, A), Before, After, LIn))
                {
                    if (YP.var(A))
                    {
                        foreach (bool l4 in sub(new Functor2("<--", X, A), Before, BN))
                        {
                            foreach (bool l5 in sub(new Functor2("<--", X, A), After, AN))
                            {
                                foreach (bool l6 in append(BN, AN, L1))
                                {
                                    foreach (bool l7 in compress_sub_list(Vars, L1, LOut))
                                    {
                                        yield return false;
                                    }
                                }
                            }
                        }
                        yield break;
                    }
                }
            }
            {
                object x1 = arg1;
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg2, L))
                {
                    foreach (bool l3 in YP.unify(arg3, L))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> in_vars(object V, object arg2)
        {
            {
                Variable V1 = new Variable();
                Variable _Vs = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(V1, _Vs)))
                {
                    if (YP.termEqual(V, V1))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                Variable _V1 = new Variable();
                Variable Vs = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(_V1, Vs)))
                {
                    foreach (bool l3 in in_vars(V, Vs))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> sub(object arg1, object arg2, object arg3)
        {
            {
                Variable _X = new Variable();
                Variable _A = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", _X, _A)))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.NIL))
                    {
                        foreach (bool l4 in YP.unify(arg3, Atom.NIL))
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable A = new Variable();
                Variable H = new Variable();
                Variable T = new Variable();
                Variable H1 = new Variable();
                Variable T1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", X, A)))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(H, T)))
                    {
                        foreach (bool l4 in YP.unify(arg3, new ListPair(H1, T1)))
                        {
                            foreach (bool l5 in sub1(new Functor2("<--", X, A), H, H1))
                            {
                                foreach (bool l6 in sub(new Functor2("<--", X, A), T, T1))
                                {
                                    yield return false;
                                }
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> sub1(object arg1, object arg2, object arg3)
        {
            {
                Variable X = new Variable();
                Variable A = new Variable();
                Variable Y = new Variable();
                Variable Old = new Variable();
                Variable New = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", X, A)))
                {
                    foreach (bool l3 in YP.unify(arg2, new Functor2("<--", Y, Old)))
                    {
                        foreach (bool l4 in YP.unify(arg3, new Functor2("<--", Y, New)))
                        {
                            foreach (bool l5 in exp_sub(new Functor2("<--", X, A), Old, New))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> exp_sub(object arg1, object B, object New)
        {
            {
                Variable X = new Variable();
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", X, A)))
                {
                    if (YP.var(B))
                    {
                        if (YP.termEqual(B, A))
                        {
                            foreach (bool l5 in YP.unify(New, X))
                            {
                                yield return false;
                            }
                            yield break;
                        }
                    }
                }
            }
            {
                Variable _X = new Variable();
                Variable _A = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", _X, _A)))
                {
                    if (YP.var(B))
                    {
                        foreach (bool l4 in YP.unify(New, B))
                        {
                            yield return false;
                        }
                        yield break;
                    }
                }
            }
            {
                Variable _X = new Variable();
                Variable _A = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", _X, _A)))
                {
                    if (YP.atomic(B))
                    {
                        foreach (bool l4 in YP.unify(New, B))
                        {
                            yield return false;
                        }
                        yield break;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable A = new Variable();
                Variable x5 = new Variable();
                Variable x6 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", X, A)))
                {
                    foreach (bool l3 in YP.unify(B, new ListPair(x5, x6)))
                    {
                        foreach (bool l4 in list_exp_sub(new Functor2("<--", X, A), B, New))
                        {
                            yield return false;
                        }
                        yield break;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable A = new Variable();
                Variable F = new Variable();
                Variable L = new Variable();
                Variable L1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", X, A)))
                {
                    foreach (bool l3 in YP.univ(B, new ListPair(F, L)))
                    {
                        foreach (bool l4 in list_exp_sub(new Functor2("<--", X, A), L, L1))
                        {
                            foreach (bool l5 in YP.univ(New, new ListPair(F, L1)))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> list_exp_sub(object arg1, object arg2, object arg3)
        {
            {
                object _S = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object S = arg1;
                Variable E = new Variable();
                Variable ER = new Variable();
                Variable EN = new Variable();
                Variable ERN = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(E, ER)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(EN, ERN)))
                    {
                        foreach (bool l4 in exp_sub(S, E, EN))
                        {
                            foreach (bool l5 in list_exp_sub(S, ER, ERN))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> split_list(object Element, object Before, object After, object List)
        {
            {
                foreach (bool l2 in append(Before, new ListPair(Element, After), List))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> compare_subst_lists(object arg1, object arg2, object arg3, object arg4)
        {
            {
                object F = arg1;
                object S = arg2;
                Variable x3 = new Variable();
                Variable x4 = new Variable();
                Variable x5 = new Variable();
                Variable x6 = new Variable();
                foreach (bool l2 in YP.unify(arg3, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg4, Atom.NIL))
                    {
                        foreach (bool l4 in YP.unify(F, new ListPair(x3, x4)))
                        {
                            goto cutIf1;
                        }
                        foreach (bool l4 in YP.unify(S, new ListPair(x5, x6)))
                        {
                            goto cutIf2;
                        }
                        foreach (bool l4 in YP.unify(F, S))
                        {
                            yield return true;
                            yield break;
                        }
                    cutIf2:
                    cutIf1:
                        { }
                    }
                }
            }
            {
                Variable F = new Variable();
                Variable S = new Variable();
                Variable x3 = new Variable();
                Variable x4 = new Variable();
                Variable x5 = new Variable();
                Variable x6 = new Variable();
                foreach (bool l2 in YP.unify(arg1, F))
                {
                    foreach (bool l3 in YP.unify(arg2, S))
                    {
                        foreach (bool l4 in YP.unify(arg3, F))
                        {
                            foreach (bool l5 in YP.unify(arg4, S))
                            {
                                foreach (bool l6 in YP.unify(F, new ListPair(x3, x4)))
                                {
                                    goto cutIf3;
                                }
                                foreach (bool l6 in YP.unify(S, new ListPair(x5, x6)))
                                {
                                    goto cutIf4;
                                }
                                yield return true;
                                yield break;
                            cutIf4:
                            cutIf3:
                                { }
                            }
                        }
                    }
                }
            }
            {
                object F = arg1;
                object S = arg2;
                object FNS = arg3;
                object SNF = arg4;
                Variable x5 = new Variable();
                Variable x6 = new Variable();
                foreach (bool l2 in YP.unify(F, new ListPair(x5, x6)))
                {
                    goto cutIf5;
                }
                foreach (bool l2 in del_item(F, S, SNF))
                {
                    foreach (bool l3 in member(F, S))
                    {
                        foreach (bool l4 in YP.unify(FNS, Atom.NIL))
                        {
                            yield return false;
                        }
                        goto cutIf6;
                    }
                    foreach (bool l3 in YP.unify(FNS, F))
                    {
                        yield return false;
                    }
                cutIf6:
                    { }
                }
                yield break;
            cutIf5:
                { }
            }
            {
                object F = arg1;
                object S = arg2;
                object FNS = arg3;
                object SNF = arg4;
                Variable x5 = new Variable();
                Variable x6 = new Variable();
                foreach (bool l2 in YP.unify(S, new ListPair(x5, x6)))
                {
                    goto cutIf7;
                }
                foreach (bool l2 in del_item(S, F, FNS))
                {
                    foreach (bool l3 in member(S, F))
                    {
                        foreach (bool l4 in YP.unify(SNF, Atom.NIL))
                        {
                            yield return false;
                        }
                        goto cutIf8;
                    }
                    foreach (bool l3 in YP.unify(SNF, S))
                    {
                        yield return false;
                    }
                cutIf8:
                    { }
                }
                yield break;
            cutIf7:
                { }
            }
            {
                object F = arg1;
                object S = arg2;
                Variable F1 = new Variable();
                Variable S1 = new Variable();
                foreach (bool l2 in YP.unify(arg3, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg4, Atom.NIL))
                    {
                        foreach (bool l4 in YP.unify(F, new ListPair(F1, Atom.NIL)))
                        {
                            foreach (bool l5 in YP.unify(S, new ListPair(S1, Atom.NIL)))
                            {
                                foreach (bool l6 in same_subst(F1, S1))
                                {
                                    yield return true;
                                    yield break;
                                }
                            }
                        }
                    }
                }
            }
            {
                Variable F = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, F))
                {
                    foreach (bool l3 in YP.unify(arg2, S))
                    {
                        foreach (bool l4 in YP.unify(arg3, F))
                        {
                            foreach (bool l5 in YP.unify(arg4, S))
                            {
                                foreach (bool l6 in length(F, 1))
                                {
                                    foreach (bool l7 in length(S, 1))
                                    {
                                        yield return true;
                                        yield break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            {
                object F = arg1;
                object S = arg2;
                object FNS = arg3;
                object SNF = arg4;
                foreach (bool l2 in length(F, 1))
                {
                    foreach (bool l3 in del_item(F, S, SNF))
                    {
                        foreach (bool l4 in member(F, S))
                        {
                            foreach (bool l5 in YP.unify(FNS, Atom.NIL))
                            {
                                yield return false;
                            }
                            goto cutIf9;
                        }
                        foreach (bool l4 in YP.unify(FNS, F))
                        {
                            yield return false;
                        }
                    cutIf9:
                        { }
                    }
                    yield break;
                }
            }
            {
                object F = arg1;
                object S = arg2;
                object FNS = arg3;
                object SNF = arg4;
                foreach (bool l2 in length(S, 1))
                {
                    foreach (bool l3 in del_item(S, F, FNS))
                    {
                        foreach (bool l4 in member(S, F))
                        {
                            foreach (bool l5 in YP.unify(SNF, Atom.NIL))
                            {
                                yield return false;
                            }
                            goto cutIf10;
                        }
                        foreach (bool l4 in YP.unify(SNF, S))
                        {
                            yield return false;
                        }
                    cutIf10:
                        { }
                    }
                }
            }
            {
                object F = arg1;
                object S = arg2;
                object FNS = arg3;
                object SNF = arg4;
                foreach (bool l2 in list_del_item(F, S, SNF))
                {
                    foreach (bool l3 in list_del_item(S, F, FNS))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> list_del_item(object arg1, object arg2, object arg3)
        {
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg2, L))
                    {
                        foreach (bool l4 in YP.unify(arg3, L))
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                object L1 = arg2;
                object Left = arg3;
                Variable It = new Variable();
                Variable R = new Variable();
                Variable LInter = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(It, R)))
                {
                    foreach (bool l3 in del_item(It, L1, LInter))
                    {
                        foreach (bool l4 in list_del_item(R, LInter, Left))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> del_item(object arg1, object arg2, object arg3)
        {
            {
                object _Item = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object Item = arg1;
                object R = arg3;
                Variable It = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(It, R)))
                {
                    foreach (bool l3 in same_subst(Item, It))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object Item = arg1;
                Variable It = new Variable();
                Variable Rest = new Variable();
                Variable R = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(It, Rest)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(It, R)))
                    {
                        foreach (bool l4 in del_item(Item, Rest, R))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> same_subst(object arg1, object arg2)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object Subs = arg2;
                Variable S1 = new Variable();
                Variable SRest = new Variable();
                Variable Subs1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(S1, SRest)))
                {
                    foreach (bool l3 in delmemb(S1, Subs, Subs1))
                    {
                        foreach (bool l4 in same_subst(SRest, Subs1))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> delmemb(object arg1, object arg2, object arg3)
        {
            {
                object _E = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object R = arg3;
                Variable E = new Variable();
                Variable E1 = new Variable();
                Variable F = new Variable();
                Variable F1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("<--", E, E1)))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<--", F, F1), R)))
                    {
                        if (YP.termEqual(E, F))
                        {
                            foreach (bool l5 in YP.copy_term(new Functor2("<--", E, E1), new Functor2("<--", F, F1)))
                            {
                                yield return false;
                            }
                        }
                    }
                }
            }
            {
                object E = arg1;
                Variable F = new Variable();
                Variable R = new Variable();
                Variable R1 = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(F, R)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(F, R1)))
                    {
                        foreach (bool l4 in delmemb(E, R, R1))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> run_tests(object File)
        {
            {
                Variable S = new Variable();
                YP.asserta(new Functor3("score", File, new Functor1("total", 0), new Functor1("wrong", 0)), getDeclaringClass());
                foreach (bool l2 in open(File, Atom.a("read"), S))
                {
                    foreach (bool l3 in loop_through(File, S))
                    {
                        foreach (bool l4 in close(S))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> loop_through(object arg1, object _S)
        {
            {
                object F = arg1;
                Variable X = new Variable();
                foreach (bool l2 in fileContents(F, X))
                {
                    foreach (bool l3 in reset_flags())
                    {
                        foreach (bool l4 in test(F, X))
                        {
                        }
                    }
                }
            }
            {
                object _F = arg1;
                yield return false;
            }
        }

        public static IEnumerable<bool> test(object arg1, object arg2)
        {
            {
                object x1 = arg1;
                foreach (bool l2 in YP.unify(arg2, Atom.a("end_of_file")))
                {
                    yield return false;
                }
            }
            {
                object F = arg1;
                Variable R = new Variable();
                Variable x3 = new Variable();
                foreach (bool l2 in YP.unify(arg2, new Functor2("error", R, x3)))
                {
                    YP.write(Atom.a("Error in Input: "));
                    YP.write(R);
                    YP.nl();
                    YP.nl();
                    foreach (bool l3 in update_score(F, Atom.a("non_null"), Atom.a("non_null")))
                    {
                        yield return false;
                    }
                    yield break;
                }
            }
            {
                object F = arg1;
                Variable G = new Variable();
                Variable Expected = new Variable();
                Variable R = new Variable();
                Variable Extra = new Variable();
                Variable Missing = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(G, new ListPair(Expected, Atom.NIL))))
                {
                    foreach (bool l3 in result(G, R))
                    {
                        foreach (bool l4 in compare_subst_lists(R, Expected, Extra, Missing))
                        {
                            foreach (bool l5 in write_if_wrong(F, G, Expected, Extra, Missing))
                            {
                                foreach (bool l6 in update_score(F, Missing, Extra))
                                {
                                    yield return false;
                                }
                            }
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> write_if_wrong(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            {
                object x1 = arg1;
                object x2 = arg2;
                object x3 = arg3;
                foreach (bool l2 in YP.unify(arg4, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg5, Atom.NIL))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object F = arg1;
                object G = arg2;
                object Expected = arg3;
                object Extra = arg4;
                object Missing = arg5;
                Variable x6 = new Variable();
                foreach (bool l2 in fake_numbervars(ListPair.make(new object[] { G, Expected, Missing }), 0, x6))
                {
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
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> update_score(object F, object arg2, object arg3)
        {
            {
                Variable T = new Variable();
                Variable W = new Variable();
                Variable T1 = new Variable();
                foreach (bool l2 in YP.unify(arg2, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg3, Atom.NIL))
                    {
                        foreach (bool l4 in YP.retract(new Functor3("score", F, new Functor1("total", T), new Functor1("wrong", W))))
                        {
                            foreach (bool l5 in YP.unify(T1, YP.add(T, 1)))
                            {
                                YP.asserta(new Functor3("score", F, new Functor1("total", T1), new Functor1("wrong", W)), getDeclaringClass());
                                yield return false;
                            }
                        }
                        yield break;
                    }
                }
            }
            {
                object x2 = arg2;
                object x3 = arg3;
                Variable T = new Variable();
                Variable W = new Variable();
                Variable T1 = new Variable();
                Variable W1 = new Variable();
                foreach (bool l2 in YP.retract(new Functor3("score", F, new Functor1("total", T), new Functor1("wrong", W))))
                {
                    foreach (bool l3 in YP.unify(T1, YP.add(T, 1)))
                    {
                        foreach (bool l4 in YP.unify(W1, YP.add(W, 1)))
                        {
                            YP.asserta(new Functor3("score", F, new Functor1("total", T1), new Functor1("wrong", W1)), getDeclaringClass());
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> inerror(object F)
        {
            {
                Variable _X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.matchDynamic(Atom.a("score"), new object[] { F, new Functor1("total", _X), new Functor1("wrong", Y) }))
                {
                    if (YP.notEqual(Y, 0))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> file(object arg1)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("fail")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("catch-and-throw")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_input")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_output")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("cut")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("repeat")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("retract")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("true")))
                {
                    yield return false;
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> display_list(object arg1)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    YP.nl();
                    yield return false;
                }
            }
            {
                Variable H = new Variable();
                Variable T = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(H, T)))
                {
                    YP.write(H);
                    YP.nl();
                    foreach (bool l3 in display_list(T))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> reset_flags()
        {
            {
                YP.set_prolog_flag(Atom.a("unknown"), Atom.a("error"));
                yield return false;
            }
        }

        public static IEnumerable<bool> exists(object arg1)
        {
            {
                Variable P = new Variable();
                Variable I = new Variable();
                Variable List = new Variable();
                Variable G = new Variable();
                Variable x5 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("/", P, I)))
                {
                    foreach (bool l3 in make_list(I, List))
                    {
                        foreach (bool l4 in YP.univ(G, new ListPair(P, List)))
                        {
                            YP.set_prolog_flag(Atom.a("unknown"), Atom.a("fail"));
                            YP.Catch catchGoal1 = new YP.Catch(new Functor1("call", G), getDeclaringClass());
                            foreach (bool l5 in catchGoal1)
                            {
                                foreach (bool l6 in reset_flags())
                                {
                                    yield return true;
                                    yield break;
                                }
                            }
                            foreach (bool l5 in catchGoal1.unifyExceptionOrThrow(x5))
                            {
                                foreach (bool l6 in reset_flags())
                                {
                                    yield return true;
                                    yield break;
                                }
                            }
                        }
                    }
                }
            }
            {
                Variable P = new Variable();
                Variable I = new Variable();
                foreach (bool l2 in YP.unify(arg1, new Functor2("/", P, I)))
                {
                    YP.write(Atom.a("Predicate: "));
                    YP.write(new Functor2("/", P, I));
                    YP.write(Atom.a(" not implemented"));
                    YP.nl();
                    foreach (bool l3 in reset_flags())
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> make_list(object N, object L)
        {
            {
                if (YP.greaterThanOrEqual(N, 0))
                {
                    foreach (bool l3 in make_list1(N, L))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> make_list1(object arg1, object arg2)
        {
            {
                foreach (bool l2 in YP.unify(arg1, 0))
                {
                    foreach (bool l3 in YP.unify(arg2, Atom.NIL))
                    {
                        yield return false;
                    }
                }
            }
            {
                object N = arg1;
                Variable x2 = new Variable();
                Variable L1 = new Variable();
                Variable N1 = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(x2, L1)))
                {
                    foreach (bool l3 in YP.unify(N1, YP.subtract(N, 1)))
                    {
                        foreach (bool l4 in make_list(N1, L1))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> fake_numbervars(object arg1, object arg2, object arg3)
        {
            {
                object X = arg1;
                object N = arg2;
                object M = arg3;
                if (YP.var(X))
                {
                    foreach (bool l3 in YP.univ(X, new ListPair(Atom.a("$VAR"), new ListPair(N, Atom.NIL))))
                    {
                        foreach (bool l4 in YP.unify(M, YP.add(N, 1)))
                        {
                            yield return false;
                        }
                    }
                    yield break;
                }
            }
            {
                object X = arg1;
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg2, N))
                {
                    foreach (bool l3 in YP.unify(arg3, N))
                    {
                        if (YP.atomic(X))
                        {
                            yield return true;
                            yield break;
                        }
                    }
                }
            }
            {
                object N = arg2;
                object M = arg3;
                Variable H = new Variable();
                Variable T = new Variable();
                Variable N1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(H, T)))
                {
                    foreach (bool l3 in fake_numbervars(H, N, N1))
                    {
                        foreach (bool l4 in fake_numbervars(T, N1, M))
                        {
                            yield return false;
                        }
                    }
                    yield break;
                }
            }
            {
                object T = arg1;
                object N = arg2;
                object M = arg3;
                Variable _F = new Variable();
                Variable Args = new Variable();
                foreach (bool l2 in YP.univ(T, new ListPair(_F, Args)))
                {
                    foreach (bool l3 in fake_numbervars(Args, N, M))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> member(object X, object arg2)
        {
            {
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(X, x2)))
                {
                    yield return false;
                }
            }
            {
                Variable x2 = new Variable();
                Variable Rest = new Variable();
                foreach (bool l2 in YP.unify(arg2, new ListPair(x2, Rest)))
                {
                    foreach (bool l3 in member(X, Rest))
                    {
                        yield return false;
                    }
                }
            }
        }

        public static IEnumerable<bool> append(object arg1, object arg2, object arg3)
        {
            {
                Variable List = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg2, List))
                    {
                        foreach (bool l4 in YP.unify(arg3, List))
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                object List2 = arg2;
                Variable X = new Variable();
                Variable List1 = new Variable();
                Variable List12 = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(X, List1)))
                {
                    foreach (bool l3 in YP.unify(arg3, new ListPair(X, List12)))
                    {
                        foreach (bool l4 in append(List1, List2, List12))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> length(object arg1, object arg2)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.NIL))
                {
                    foreach (bool l3 in YP.unify(arg2, 0))
                    {
                        yield return true;
                        yield break;
                    }
                }
            }
            {
                object Length = arg2;
                Variable x1 = new Variable();
                Variable Rest = new Variable();
                Variable RestLength = new Variable();
                foreach (bool l2 in YP.unify(arg1, new ListPair(x1, Rest)))
                {
                    foreach (bool l3 in length(Rest, RestLength))
                    {
                        foreach (bool l4 in YP.unify(Length, YP.add(RestLength, 1)))
                        {
                            yield return false;
                        }
                    }
                }
            }
        }

        public static IEnumerable<bool> open(object x1, object x2, object x3)
        {
            {
                yield return false;
            }
        }

        public static IEnumerable<bool> close(object x1)
        {
            {
                yield return false;
            }
        }

        public static IEnumerable<bool> intAndFloatAreDifferent()
        {
            {
                if (YP.termNotEqual(1.0, 1))
                {
                    yield return false;
                }
            }
        }

        public static IEnumerable<bool> fileContents(object arg1, object arg2)
        {
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("abolish"), 1)), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("abolish"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("foo"), Atom.a("a"))), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", Atom.a("foo"), -1)), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("current_prolog_flag", Atom.a("max_arity"), A), new Functor2(",", new Functor2("is", X, new Functor2("+", A, 1)), new Functor1("abolish", new Functor2("/", Atom.a("foo"), X)))), new ListPair(new Functor1("representation_error", Atom.a("max_arity")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("abolish")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("abolish", new Functor2("/", 5, 2)), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, 1), new Functor1("var", X)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("var", X), new Functor2("=", X, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", Atom.a("fail"), new Functor1("call", 3)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("nofoo", X), new Functor1("call", X)), new ListPair(new Functor2("existence_error", Atom.a("procedure"), new Functor2("/", Atom.a("nofoo"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("and")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, Atom.a("true")), new Functor1("call", X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("true")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", X, Atom.a("b")), Atom.a("a")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 2, new Functor3("foo", Atom.a("a"), new Functor2("f", X, Atom.a("b")), Atom.a("c")), new Functor2("f", Atom.a("a"), Y)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), new ListPair(new Functor2("<--", Y, Atom.a("b")), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor3("arg", 1, new Functor2("foo", X, Atom.a("b")), Y), new Functor2("=", X, Atom.a("a"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", Y, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("b")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("foo")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 3, new Functor2("foo", 3, 4), N), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", X, new Functor2("foo", Atom.a("a"), Atom.a("b")), Atom.a("a")), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 1, X, Atom.a("a")), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, Atom.a("atom"), A), new ListPair(new Functor2("type_error", Atom.a("compound"), Atom.a("atom")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", 0, 3, A), new ListPair(new Functor2("type_error", Atom.a("compound"), 3), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", -3, new Functor2("foo", Atom.a("a"), Atom.a("b")), A), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -3), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arg")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("arg", Atom.a("a"), new Functor2("foo", Atom.a("a"), Atom.a("b")), X), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", 0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", N, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=\\=", new Functor1("floot", 1), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", N, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", new Functor1("floot", 1), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=:=", 0.333, new Functor2("/", 1, 3)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">=", 0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">=", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">=", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">=", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(">=", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<", 0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("<", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=<", 0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=<", new Functor2("*", 3, 2), new Functor2("-", 7, 1)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=<", X, 5), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("arith_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=<", new Functor2("+", 2, new Functor1("floot", 1)), 5), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("floot"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable B = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("asserta", new Functor2(":-", new Functor1("bar", X), X)), new Functor2("clause", new Functor1("bar", X), B)), new ListPair(new ListPair(new ListPair(new Functor2("<--", B, new Functor1("call", X)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", x1), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", 4), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", new Functor2(":-", Atom.a("foo"), 4)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("asserta")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("asserta", new Functor2(":-", new Functor1("atom", x1), Atom.a("true"))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", new Functor1("foo", X), new Functor2("->", X, new Functor1("call", X)))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", x1), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", 4), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", Atom.a("foo"), 4)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("assertz")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("assertz", new Functor2(":-", new Functor1("atom", x1), Atom.a("true"))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.a("string")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Var = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Var), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", Atom.NIL), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", 6), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atom", 3.3), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a(""), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.NIL, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("["), new ListPair(Atom.a("]"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("'"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("'"), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("iso"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make(new object[] { Atom.a("i"), Atom.a("s"), Atom.a("o") })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, ListPair.make(new object[] { Atom.a("p"), Atom.a("r"), Atom.a("o"), Atom.a("l"), Atom.a("o"), Atom.a("g") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("North"), new ListPair(Atom.a("N"), X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make(new object[] { Atom.a("o"), Atom.a("r"), Atom.a("t"), Atom.a("h") })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", Atom.a("iso"), new ListPair(Atom.a("i"), new ListPair(Atom.a("s"), Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable E = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, ListPair.make(new object[] { Atom.a("a"), E, Atom.a("c") })), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, new ListPair(Atom.a("a"), new ListPair(Atom.a("b"), L))), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", new Functor1("f", Atom.a("a")), L), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, Atom.a("iso")), new ListPair(new Functor2("type_error", Atom.a("list"), Atom.a("iso")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_chars", A, new ListPair(Atom.a("a"), new ListPair(new Functor1("f", Atom.a("b")), Atom.NIL))), new ListPair(new Functor2("type_error", Atom.a("character"), new Functor1("f", Atom.a("b"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("atom_chars", X, new ListPair(Atom.a("1"), new ListPair(Atom.a("2"), Atom.NIL))), new Functor2("is", Y, new Functor2("+", X, 1))), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("12"), 0)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a(""), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.NIL, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(91, new ListPair(93, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("'"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(39, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("iso"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make(new object[] { 105, 115, 111 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, ListPair.make(new object[] { 112, 114, 111, 108, 111, 103 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("prolog")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("North"), new ListPair(78, L)), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make(new object[] { 111, 114, 116, 104 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", Atom.a("iso"), new ListPair(105, new ListPair(115, Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", new Functor1("f", Atom.a("a")), L), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, 120), new ListPair(new Functor2("type_error", Atom.a("list"), 120), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_codes", A, ListPair.make(new object[] { 105, 115, Atom.a("o") })), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), A), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("hello world")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable T = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", T, Atom.a(" world"), Atom.a("small world")), new ListPair(new ListPair(new ListPair(new Functor2("<--", T, Atom.a("small")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("hello"), Atom.a(" world"), Atom.a("small world")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable T1 = new Variable();
                Variable T2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", T1, T2, Atom.a("hello")), new ListPair(ListPair.make(new object[] { new ListPair(new Functor2("<--", T1, Atom.a("")), new ListPair(new Functor2("<--", T2, Atom.a("hello")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("h")), new ListPair(new Functor2("<--", T2, Atom.a("ello")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("he")), new ListPair(new Functor2("<--", T2, Atom.a("llo")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hel")), new ListPair(new Functor2("<--", T2, Atom.a("lo")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hell")), new ListPair(new Functor2("<--", T2, Atom.a("o")), Atom.NIL)), new ListPair(new Functor2("<--", T1, Atom.a("hello")), new ListPair(new Functor2("<--", T2, Atom.a("")), Atom.NIL)) }), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A1 = new Variable();
                Variable A3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", A1, Atom.a("iso"), A3), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A2 = new Variable();
                Variable A3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("iso"), A2, A3), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", new Functor1("f", Atom.a("a")), Atom.a("iso"), A3), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", Atom.a("iso"), new Functor1("f", Atom.a("a")), A3), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A1 = new Variable();
                Variable A2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_concat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("atom_concat", A1, A2, new Functor1("f", Atom.a("a"))), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("enchanted evening"), N), new ListPair(new ListPair(new ListPair(new Functor2("<--", N, 17), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a(""), N), new ListPair(new ListPair(new ListPair(new Functor2("<--", N, 0), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("scarlet"), 5), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Atom_1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom_1, 4), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", 1.23, 4), new ListPair(new Functor2("type_error", Atom.a("atom"), 1.23), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atom_length")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("atom_length", Atom.a("atom"), Atom.a("4")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("4")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Var = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Var), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", Atom.NIL), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", 6), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("atomic")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("atomic", 3.3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable Z = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2("=", X, Z)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, Atom.a("fail"), L), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Y = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", 1, new Functor2(";", new Functor2("=", Y, 1), new Functor2("=", Y, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable L = new Variable();
                Variable x4 = new Variable();
                Variable x5 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", new Functor2("f", X, Y), new Functor2(";", new Functor2("=", X, Atom.a("a")), new Functor2("=", Y, Atom.a("b"))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(new Functor2("f", Atom.a("a"), x4), new ListPair(new Functor2("f", x5, Atom.a("b")), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, new Functor2(";", new Functor2(",", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                Variable x4 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, new Functor2(";", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make(new object[] { 1, x4, 2 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("bagof", X, new Functor2(";", new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1))), new Functor2("=", X, 3)), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable Z = new Variable();
                Variable L = new Variable();
                Variable x5 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2(";", new Functor2("=", X, Z), new Functor2("=", Y, 1))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(x5, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable Z = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, new Functor2("^", Y, Z), L), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("bagof", X, 1, L), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("bagof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor1("call", 4), S), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", Atom.a("!")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), X)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), new Functor1("call", 1))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), X)), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), new Functor1("call", 1))), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", 1), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", Atom.a("fail"), 1)), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(",", Atom.a("fail"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(",", new Functor1("write", 3), 1)), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(",", new Functor1("write", 3), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("call")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("call", new Functor2(";", 1, Atom.a("true"))), new ListPair(new Functor2("type_error", Atom.a("callable"), new Functor2(";", 1, Atom.a("true"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable C = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("catch-and-throw")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor3("catch", Atom.a("true"), C, new Functor1("write", Atom.a("something"))), new Functor1("throw", Atom.a("blabla"))), new ListPair(Atom.a("system_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                Variable x3 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("catch-and-throw")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("catch", new Functor2("number_chars", A, L), new Functor2("error", Atom.a("instantiation_error"), x3), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Code = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("a"), Code), new ListPair(new ListPair(new ListPair(new Functor2("<--", Code, 97), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Char = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, 99), new ListPair(new ListPair(new ListPair(new Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Char = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, 99), new ListPair(new ListPair(new ListPair(new Functor2("<--", Char, Atom.a("c")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("b"), 98), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("b"), 4), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Code = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("ab"), Code), new ListPair(new Functor2("type_error", Atom.a("character"), Atom.a("ab")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Atom.a("a"), Atom.a("x")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("x")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Char = new Variable();
                Variable Code = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, Code), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Char = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("char_code")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("char_code", Char, -2), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Body = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("clause", Atom.a("x"), Body), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable B = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("clause", x1, B), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable B = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("clause", 4, B), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("clause", new Functor1("f", x1), 5), new ListPair(new Functor2("type_error", Atom.a("callable"), 5), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable Body = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("clause")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("clause", new Functor1("atom", x1), Body), new ListPair(new Functor3("permission_error", Atom.a("access"), Atom.a("private_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", 33.3), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", -33.3), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new Functor1("-", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", x1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", Atom.a("a")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("compound")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("compound", new ListPair(Atom.a("a"), Atom.NIL)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", X, Y), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", X, 3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", x1, Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", x1, x2), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable A = new Variable();
                Variable B = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("copy_term", new Functor2("+", new Functor2("+", X, X), Y), new Functor2("+", new Functor2("+", A, B), B)), new Functor2("=", A, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", B, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", Atom.a("a"), Atom.a("a")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", Atom.a("a"), Atom.a("b")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("copy_term", new Functor1("f", Atom.a("a")), new Functor1("f", X)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("a")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("copy_term")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b"))), new Functor2("copy_term", new Functor2("+", Atom.a("a"), X), new Functor2("+", X, Atom.a("b")))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_input")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1(Atom.a("exists", Atom.a("")), new Functor2("/", Atom.a("current_input"), 1)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_output")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1(Atom.a("exists", Atom.a("")), new Functor2("/", Atom.a("current_output"), 1)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", Atom.a("current_predicate"), 1)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", Atom.a("score"), 3)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Name = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, ListPair.make(new object[] { new Functor3("functor", new Functor1(Atom.a("run_tests", Atom.a("")), 1), Name, x2), new Functor1("current_predicate", new Functor2("/", Name, 1)), Atom.a("success") })))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", 4), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", Atom.a("dog")), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), Atom.a("dog")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_predicate")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("current_predicate", new Functor2("/", 0, Atom.a("dog"))), new ListPair(new Functor2("type_error", Atom.a("predicate_indicator"), new Functor2("/", 0, Atom.a("dog"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("debug"), Atom.a("off")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("warning"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor2("current_prolog_flag", Atom.a("unknown"), Atom.a("error"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable V = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("debug"), V), new ListPair(new ListPair(new ListPair(new Functor2("<--", V, Atom.a("off")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable V = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", 5, V), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable V = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("current_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("current_prolog_flag", Atom.a("warning"), V), new ListPair(new Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("warning")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("cut")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Atom.a("!"), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("cut")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("cut")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", new Functor1("call", Atom.a("!")), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("fail")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Atom.a("fail"), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("fail")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Atom.a("undef_pred"), new ListPair(new Functor2("existence_error", Atom.a("procedure"), new Functor2("/", Atom.a("undef_pred"), 0)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("fail")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), Atom.a("undef_pred")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("fail")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), Atom.a("undef_pred")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                Variable x4 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", new Functor2("+", X, Y), new Functor2("=", X, 1), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(new Functor2("+", 1, x4), Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, Atom.a("fail"), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, Atom.NIL), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 1)), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(1, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 1)), new ListPair(1, new ListPair(2, Atom.NIL))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new ListPair(X, new ListPair(Y, Atom.NIL))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Goal = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, Goal, S), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, 4, S), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("findall")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("findall", X, new Functor1("call", 1), S), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("float", 3.3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("float", -3.3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("float", 3), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("float", Atom.a("atom")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("float")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("float", X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), Atom.a("foo"), 3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor3("foo", Atom.a("a"), Atom.a("b"), Atom.a("c")), X, Y), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("foo")), new ListPair(new Functor2("<--", Y, 3), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable A = new Variable();
                Variable B = new Variable();
                Variable C = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), 3), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new Functor3("foo", A, B, C)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), 0), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable B = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor2("mats", A, B), A, B), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, Atom.a("mats")), new ListPair(new Functor2("<--", B, 2), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor1("foo", Atom.a("a")), Atom.a("foo"), 2), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new Functor1("foo", Atom.a("a")), Atom.a("fo"), 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", 1, X, Y), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), new ListPair(new Functor2("<--", Y, 0), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, 1.1, 0), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1.1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", new ListPair(x1, x2), Atom.a("."), 2), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", Atom.NIL, Atom.NIL, 0), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Y, 3), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), N), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", X, Atom.a("foo"), Atom.a("a")), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable F = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", F, 1.5, 1), new ListPair(new Functor2("type_error", Atom.a("atom"), 1.5), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable F = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", F, new Functor1("foo", Atom.a("a")), 1), new ListPair(new Functor2("type_error", Atom.a("atomic"), new Functor1("foo", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable X = new Variable();
                Variable T = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("current_prolog_flag", Atom.a("max_arity"), A), new Functor2(",", new Functor2("is", X, new Functor2("+", A, 1)), new Functor3("functor", T, Atom.a("foo"), X))), new ListPair(new Functor1("representation_error", Atom.a("max_arity")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable T = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("functor")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("functor", T, Atom.a("foo"), -1), new ListPair(new Functor2("domain_error", Atom.a("not_less_than_zero"), -1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("fail"), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), new Functor2("=", X, 1)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("->", Atom.a("true"), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), Atom.a("true")), Atom.a("fail")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("true")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), Atom.a("fail")), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), Atom.a("true")), Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), new Functor2("=", X, 1)), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("fail"), new Functor2("=", X, 1)), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", Atom.a("true"), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("if-then-else")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("->", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), Atom.a("true")), Atom.a("true")), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("integer", 3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("integer", -3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("integer", 3.3), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("integer", X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("integer")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("integer", Atom.a("atom")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Result = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("is", Result, new Functor2("+", 3, 11.0)), new ListPair(new ListPair(new ListPair(new Functor2("<--", Result, 14.0), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, new Functor2("+", 1, 2)), new Functor2("is", Y, new Functor2("*", X, 3))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new Functor2("+", 1, 2)), new ListPair(new Functor2("<--", Y, 9), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("is", Atom.a("foo"), 77), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable N = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("is", 77, N), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("is", 77, Atom.a("foo")), new ListPair(new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Atom.a("foo"), 0)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("is")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("is", X, new Functor1("float", 3)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 3.0), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", 33.3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", Atom.a("foo")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Foo = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", Foo), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Foo = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", Atom.a("foo"), Foo), new Functor1("nonvar", Foo)), new ListPair(new ListPair(new ListPair(new Functor2("<--", Foo, Atom.a("foo")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", x1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("nonvar")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("nonvar", new Functor1("a", Atom.a("b"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", Atom.a("!")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", new Functor2(",", Atom.a("!"), Atom.a("fail"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new Functor1("\\+", new Functor2(",", Atom.a("!"), Atom.a("fail")))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", new Functor2("=", 4, 5)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", 3), new ListPair(new Functor2("type_error", Atom.a("callable"), 3), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_provable")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("\\+", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", X, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", X, Y), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("\\=", X, Y), new Functor2("\\=", X, Atom.a("abc"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor2("f", X, Atom.a("def")), new Functor2("f", Atom.a("def"), Y)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 2), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", 1, 1.0), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor1("g", X), new Functor1("f", new Functor1("f", X))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor2("f", X, 1), new Functor1("f", new Functor1("a", X))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("not_unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\=", new Functor3("f", X, Y, X), new Functor("f", new object[] { new Functor1("a", X), new Functor1("a", Y), Y, 2 })), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("number", 3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("number", 3.3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("number", -3), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("number", Atom.a("a")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("number", X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Atom.a("3"), new ListPair(Atom.a("3"), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33, new ListPair(Atom.a("3"), new ListPair(Atom.a("3"), Atom.NIL))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 33.0, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make(new object[] { Atom.a("3"), Atom.a("3"), Atom.a("."), Atom.a("0") })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", X, ListPair.make(new object[] { Atom.a("3"), Atom.a("."), Atom.a("3"), Atom.a("E"), Atom.a("+"), Atom.a("0") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 3.3), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", 3.3, ListPair.make(new object[] { Atom.a("3"), Atom.a("."), Atom.a("3") })), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("-"), Atom.a("2"), Atom.a("5") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("\x0A"), Atom.a(" "), Atom.a("3") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, new ListPair(Atom.a("3"), new ListPair(Atom.a("x"), Atom.NIL))), new ListPair(new Functor1("syntax_error", x2), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("0"), Atom.a("x"), Atom.a("f") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("0"), Atom.a("'"), Atom.a("A") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 65), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("4"), Atom.a("."), Atom.a("2") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, ListPair.make(new object[] { Atom.a("4"), Atom.a("2"), Atom.a("."), Atom.a("0"), Atom.a("e"), Atom.a("-"), Atom.a("1") })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", Atom.a("a"), L), new ListPair(new Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, 4), new ListPair(new Functor2("type_error", Atom.a("list"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_chars")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_chars", A, new ListPair(Atom.a("4"), new ListPair(2, Atom.NIL))), new ListPair(new Functor2("type_error", Atom.a("character"), 2), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(51, new ListPair(51, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33, new ListPair(51, new ListPair(51, Atom.NIL))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33.0, L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, ListPair.make(new object[] { 51, 51, 46, 48 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", 33.0, ListPair.make(new object[] { 51, 51, 46, 48 })), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 45, 50, 53 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, -25), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, new ListPair(32, new ListPair(51, Atom.NIL))), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 3), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 48, 120, 102 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 15), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 48, 39, 97 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 97), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 52, 46, 50 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 52, 50, 46, 48, 101, 45, 49 })), new ListPair(new ListPair(new ListPair(new Functor2("<--", A, 4.2), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, L), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", Atom.a("a"), L), new ListPair(new Functor2("type_error", Atom.a("number"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, 4), new ListPair(new Functor2("type_error", Atom.a("list"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("number_codes")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("number_codes", A, ListPair.make(new object[] { 49, 50, Atom.a("3") })), new ListPair(new Functor1("representation_error", Atom.a("character_code")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("!")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor1("once", Atom.a("!")), new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("repeat")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("once", Atom.a("fail")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("once", 3), new ListPair(new Functor2("type_error", Atom.a("callable"), 3), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("once")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("once", X), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", Atom.a("true"), Atom.a("fail")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", Atom.a("!"), Atom.a("fail")), Atom.a("true")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", Atom.a("!"), new Functor1("call", 3)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2(",", new Functor2("=", X, 1), Atom.a("!")), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("or")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", X, 2), Atom.NIL), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("repeat")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", Atom.a("repeat"), new Functor2(",", Atom.a("!"), Atom.a("fail"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("retract")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("retract", new Functor2(":-", 4, X)), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("retract")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor1("retract", new Functor2(":-", new Functor1("atom", x1), new Functor2("==", X, Atom.NIL))), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"), new Functor2("/", Atom.a("atom"), 1)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable V = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("fail")), new Functor2("current_prolog_flag", Atom.a("unknown"), V)), new ListPair(new ListPair(new ListPair(new Functor2("<--", V, Atom.a("fail")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", X, Atom.a("warning")), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", 5, Atom.a("decimals")), new ListPair(new Functor2("type_error", Atom.a("atom"), 5), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("date"), Atom.a("July 1999")), new ListPair(new Functor2("domain_error", Atom.a("prolog_flag"), Atom.a("date")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("debug"), Atom.a("no")), new ListPair(new Functor2("domain_error", Atom.a("flag_value"), new Functor2("+", Atom.a("debug"), Atom.a("no"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("max_arity"), 40), new ListPair(new Functor3("permission_error", Atom.a("modify"), Atom.a("flag"), Atom.a("max_arity")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("atom")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make(new object[] { 34, 102, 114, 101, 100, 34, 46, 32 }), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("fred")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("chars")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make(new object[] { 34, 102, 114, 101, 100, 34, 46, 32 }), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make(new object[] { Atom.a("f"), Atom.a("r"), Atom.a("e"), Atom.a("d") })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("set_prolog_flag", Atom.a("double_quotes"), Atom.a("codes")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("set_prolog_flag")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("read", ListPair.make(new object[] { 34, 102, 114, 101, 100, 34, 46, 32 }), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, ListPair.make(new object[] { 102, 114, 101, 100 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", X, 2)), X), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 1)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, 2), new Functor2("=", X, 2)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(2, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, Atom.a("fail"), L), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Y = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", 1, new Functor2(";", new Functor2("=", Y, 2), new Functor2("=", Y, 1)), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(1, Atom.NIL)), new ListPair(new Functor2("<--", Y, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable L = new Variable();
                Variable x4 = new Variable();
                Variable x5 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", new Functor2("f", X, Y), new Functor2(";", new Functor2("=", X, Atom.a("a")), new Functor2("=", Y, Atom.a("b"))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(new Functor2("f", x4, Atom.a("b")), new ListPair(new Functor2("f", Atom.a("a"), x5), Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2(",", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(1, new ListPair(2, Atom.NIL))), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                Variable x4 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1)), new Functor2(",", new Functor2("=", X, 2), new Functor2("=", Y, 2)))), S), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make(new object[] { x4, 1, 2 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("setof", X, new Functor2(";", new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2("=", Y, 1))), new Functor2("=", X, 3)), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, new ListPair(3, Atom.NIL)), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable S = new Variable();
                Variable x4 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("set_prolog_flag", Atom.a("unknown"), Atom.a("warning")), new Functor3("setof", X, new Functor2("^", Y, new Functor2(";", new Functor2("=", X, 1), new Functor2(";", new Functor2("=", Y, 1), new Functor2("=", X, 3)))), S)), new ListPair(new ListPair(new ListPair(new Functor2("<--", S, ListPair.make(new object[] { x4, 1, 3 })), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                Variable Z = new Variable();
                Variable L = new Variable();
                Variable x5 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2(";", new Functor2("=", X, Y), new Functor2(";", new Functor2("=", X, Z), new Functor2("=", Y, 1))), L), new ListPair(new ListPair(new ListPair(new Functor2("<--", L, new ListPair(Y, new ListPair(Z, Atom.NIL))), Atom.NIL), new ListPair(new ListPair(new Functor2("<--", L, new ListPair(x5, Atom.NIL)), new ListPair(new Functor2("<--", Y, 1), Atom.NIL)), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, new Functor2("^", X, new Functor2(";", Atom.a("true"), 4)), L), new ListPair(new Functor2("type_error", Atom.a("callable"), 4), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable L = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("setof")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor3("setof", X, 1, L), new ListPair(new Functor2("type_error", Atom.a("callable"), 1), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("abracadabra"), 0, 5, x1, S2 }), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("abrac")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("abracadabra"), x1, 5, 0, S2 }), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("dabra")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Length = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("abracadabra"), 3, Length, 3, S2 }), new ListPair(new ListPair(new ListPair(new Functor2("<--", Length, 5), new ListPair(new Functor2("<--", S2, Atom.a("acada")), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Before = new Variable();
                Variable After = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("abracadabra"), Before, 2, After, Atom.a("ab") }), new ListPair(new ListPair(new ListPair(new Functor2("<--", Before, 0), new ListPair(new Functor2("<--", After, 9), Atom.NIL)), new ListPair(new ListPair(new Functor2("<--", Before, 7), new ListPair(new Functor2("<--", After, 2), Atom.NIL)), Atom.NIL)), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("Banana"), 3, 2, x1, S2 }), new ListPair(new ListPair(new ListPair(new Functor2("<--", S2, Atom.a("an")), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Before = new Variable();
                Variable After = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("charity"), Before, 3, After, S2 }), new ListPair(ListPair.make(new object[] { ListPair.make(new object[] { new Functor2("<--", Before, 0), new Functor2("<--", After, 4), new Functor2("<--", S2, Atom.a("cha")) }), ListPair.make(new object[] { new Functor2("<--", Before, 1), new Functor2("<--", After, 3), new Functor2("<--", S2, Atom.a("har")) }), ListPair.make(new object[] { new Functor2("<--", Before, 2), new Functor2("<--", After, 2), new Functor2("<--", S2, Atom.a("ari")) }), ListPair.make(new object[] { new Functor2("<--", Before, 3), new Functor2("<--", After, 1), new Functor2("<--", S2, Atom.a("rit")) }), ListPair.make(new object[] { new Functor2("<--", Before, 4), new Functor2("<--", After, 0), new Functor2("<--", S2, Atom.a("ity")) }) }), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Before = new Variable();
                Variable Length = new Variable();
                Variable After = new Variable();
                Variable Sub_atom = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("ab"), Before, Length, After, Sub_atom }), new ListPair(ListPair.make(new object[] { ListPair.make(new object[] { new Functor2("<--", Before, 0), new Functor2("<--", Length, 0), new Functor2("<--", After, 2), new Functor2("<--", Sub_atom, Atom.a("")) }), ListPair.make(new object[] { new Functor2("<--", Before, 0), new Functor2("<--", Length, 1), new Functor2("<--", After, 1), new Functor2("<--", Sub_atom, Atom.a("a")) }), ListPair.make(new object[] { new Functor2("<--", Before, 0), new Functor2("<--", Length, 2), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a("ab")) }), ListPair.make(new object[] { new Functor2("<--", Before, 1), new Functor2("<--", Length, 0), new Functor2("<--", After, 1), new Functor2("<--", Sub_atom, Atom.a("")) }), ListPair.make(new object[] { new Functor2("<--", Before, 1), new Functor2("<--", Length, 1), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a("b")) }), ListPair.make(new object[] { new Functor2("<--", Before, 2), new Functor2("<--", Length, 0), new Functor2("<--", After, 0), new Functor2("<--", Sub_atom, Atom.a("")) }) }), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable Banana = new Variable();
                Variable x2 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Banana, 3, 2, x2, S2 }), new ListPair(Atom.a("instantiation_error"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { new Functor1("f", Atom.a("a")), 2, 2, x1, S2 }), new ListPair(new Functor2("type_error", Atom.a("atom"), new Functor1("f", Atom.a("a"))), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("Banana"), 4, 2, x1, 2 }), new ListPair(new Functor2("type_error", Atom.a("atom"), 2), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("Banana"), Atom.a("a"), 2, x1, S2 }), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("a")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("Banana"), 4, Atom.a("n"), x1, S2 }), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("n")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable S2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("sub_atom")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor("sub_atom", new object[] { Atom.a("Banana"), 4, x1, Atom.a("m"), S2 }), new ListPair(new Functor2("type_error", Atom.a("integer"), Atom.a("m")), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", 1, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", 1, 2), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, Y), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", x1, x2), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", X, new Functor1("a", X)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_diff")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("\\==", new Functor1("f", Atom.a("a")), new Functor1("f", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", 1, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, X), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", 1, 2), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, Y), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable x1 = new Variable();
                Variable x2 = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", x1, x2), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", X, new Functor1("a", X)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_eq")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("==", new Functor1("f", Atom.a("a")), new Functor1("f", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", X, X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", 1.0, 1), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", X, X), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_gt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@>=", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", X, X), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@<", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", 1.0, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("aardvark"), Atom.a("zebra")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("short"), Atom.a("short")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", Atom.a("short"), Atom.a("shorter")), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", new Functor1("foo", Atom.a("b")), new Functor1("foo", Atom.a("a"))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", X, X), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("term_lt=")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("@=<", new Functor2("foo", Atom.a("a"), X), new Functor2("foo", Atom.a("b"), Y)), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("true")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(Atom.a("true"), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 1), new ListPair(Atom.a("success"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", X, 1), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, 1), Atom.NIL), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2(",", new Functor2("=", X, Y), new Functor2("=", X, Atom.a("abc"))), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("abc")), new ListPair(new Functor2("<--", Y, Atom.a("abc")), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor2("f", X, Atom.a("def")), new Functor2("f", Atom.a("def"), Y)), new ListPair(new ListPair(new ListPair(new Functor2("<--", X, Atom.a("def")), new ListPair(new Functor2("<--", Y, Atom.a("def")), Atom.NIL)), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 2), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", 1, 1.0), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        foreach (bool l4 in intAndFloatAreDifferent())
                        {
                            yield return false;
                        }
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor1("g", X), new Functor1("f", new Functor1("f", X))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor2("f", X, 1), new Functor1("f", new Functor1("a", X))), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable X = new Variable();
                Variable Y = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor3("f", X, Y, X), new Functor("f", new object[] { new Functor1("a", X), new Functor1("a", Y), Y, 2 })), new ListPair(Atom.a("failure"), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
            {
                Variable A = new Variable();
                Variable B = new Variable();
                Variable C = new Variable();
                Variable D = new Variable();
                foreach (bool l2 in YP.unify(arg1, Atom.a("unify")))
                {
                    foreach (bool l3 in YP.unify(arg2, new ListPair(new Functor2("=", new Functor3("f", A, B, C), new Functor3("f", new Functor2("g", B, B), new Functor2("g", C, C), new Functor2("g", D, D))), new ListPair(new ListPair(ListPair.make(new object[] { new Functor2("<--", A, new Functor2("g", new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D)), new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D)))), new Functor2("<--", B, new Functor2("g", new Functor2("g", D, D), new Functor2("g", D, D))), new Functor2("<--", C, new Functor2("g", D, D)) }), Atom.NIL), Atom.NIL))))
                    {
                        yield return false;
                    }
                }
            }
        }

    }
}
