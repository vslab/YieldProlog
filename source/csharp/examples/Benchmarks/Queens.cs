using System; 
using System.Collections.Generic;
using YieldProlog;

namespace YieldPrologBenchmarks
{
    class Queens
    {
        static void Main(string[] args)
        {
            long startTicks = DateTime.Now.Ticks;
            int nAnswers = 0;
            Variable Qs = new Variable();
            foreach (bool l1 in queens(11, Qs))
            {
                ++nAnswers;
            }
            long finishTicks = DateTime.Now.Ticks;
            Console.WriteLine("Optimized queens: " + 
                (finishTicks - startTicks) / 10000000.0 + " seconds, " + nAnswers + " answers");

            Console.WriteLine("\nPress Enter to finish.");
            Console.ReadLine();
        }

        static IEnumerable<bool> queens(int N, Variable Qs)
        {
            Variable Ns = new Variable();
            foreach (bool l1 in rangeList(1, N, Ns))
            {
                foreach (bool l2 in queens3(Ns, Atom.NIL, Qs))
                    yield return false;
            }
        }

        static IEnumerable<bool> queens3(object UnplacedQs, object SafeQs, Variable Qs)
        {
            ListPair UnplacedQsListPair = YP.getValue(UnplacedQs) as ListPair;
            if (UnplacedQsListPair != null)
            {
                Variable UnplacedQs1 = new Variable();
                Variable Q = new Variable();
                foreach (bool l1 in selectq(Q, UnplacedQsListPair, UnplacedQs1))
                {
                    if (!(SafeQs is ListPair && hasAttack((int)Q.getValue(), (ListPair)SafeQs)))
                    {
                        foreach (bool l2 in queens3(UnplacedQs1, new ListPair(Q, SafeQs), Qs))
                            yield return false;
                    }
                }
            }
            else
            {
                foreach (bool l1 in Qs.unify(SafeQs))
                    yield return false;
            }
        }

        static bool hasAttack(int X, ListPair Xs)
        {
            return hasAttack3(X, 1, Xs);
        }

        static bool hasAttack3(int X, int N, ListPair Arg3)
        {
            if (X == (int)YP.getValue(Arg3._arg1) + N || X == (int)YP.getValue(Arg3._arg1) - N)
                return true;
            if (Arg3._arg2 is ListPair)
                return hasAttack3(X, N + 1, (ListPair)YP.getValue(Arg3._arg2));
            else
                return false;
        }

        static IEnumerable<bool> rangeList(int M, int N, Variable List)
        {
            if (M >= N)
            {
                foreach (bool l1 in List.unify(new ListPair(N, Atom.NIL)))
                    yield return false;
            }
            else
            {
                Variable Tail = new Variable();
                foreach (bool l1 in rangeList(M + 1, N, Tail))
                {
                    foreach (bool l2 in List.unify(new ListPair(M, Tail)))
                        yield return false;
                }
            }
        }

        static IEnumerable<bool> selectq(Variable X, ListPair Arg2, Variable Arg3)
        {
            foreach (bool l1 in X.unify(Arg2._arg1))
            {
                foreach (bool l2 in Arg3.unify(Arg2._arg2))
                    yield return false;
            }

            ListPair Arg2Tail = YP.getValue(Arg2._arg2) as ListPair;
            if (Arg2Tail != null)
            {
                Variable Zs = new Variable();
                foreach (bool l1 in selectq(X, Arg2Tail, Zs))
                {
                    foreach (bool l2 in Arg3.unify(new ListPair(Arg2._arg1, Zs)))
                        yield return false;
                }
            }
        }
    }
}
