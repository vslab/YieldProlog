using System; 
using System.Collections.Generic;
using YieldProlog;

namespace YieldPrologBenchmarks
{
    class NaiveQueens
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
            Console.WriteLine("Naive queens: " +
                (finishTicks - startTicks) / 10000000.0 + " seconds, " + nAnswers + " answers");

            Console.WriteLine("\nPress Enter to finish.");
            Console.ReadLine();
        }

        static IEnumerable<bool> queens(object N, object Qs)
        {
            Variable Ns = new Variable();
            foreach (bool l1 in rangeList(1, N, Ns))
            {
                foreach (bool l2 in queens3(Ns, Atom.NIL, Qs))
                    yield return false;
            }
        }

        static IEnumerable<bool> queens3(object UnplacedQs, object SafeQs, object Qs)
        {
            Variable UnplacedQs1 = new Variable();
            Variable Q = new Variable();
            foreach (bool l1 in selectq(Q, UnplacedQs, UnplacedQs1))
            {
                foreach (bool l2 in notHasAttack(Q, SafeQs))
                {
                    foreach (bool l3 in queens3(UnplacedQs1, new ListPair(Q, SafeQs), Qs))
                        yield return false;
                }
            }
            foreach (bool l1 in YP.unify(UnplacedQs, Atom.NIL))
            {
                foreach (bool l2 in YP.unify(Qs, SafeQs))
                    yield return false;
            }
        }

        static IEnumerable<bool> notHasAttack(object X, object Xs)
        {
            foreach (bool l in attack(X, Xs))
                yield break;

            yield return false;
        }

        static IEnumerable<bool> attack(object X, object Xs)
        {
            foreach (bool l in attack3(X, 1, Xs))
                yield return false;
        }

        static IEnumerable<bool> attack3(object X, object N, object Arg3)
        {
            Variable Y = new Variable();
            foreach (bool l in new ListPair(Y, new Variable()).unify(Arg3))
            {
                if ((int)YP.getValue(X) == (int)Y.getValue() + (int)YP.getValue(N))
                    yield return false;
                if ((int)YP.getValue(X) == (int)Y.getValue() - (int)YP.getValue(N))
                    yield return false;
            }
            Variable Ys = new Variable();
            Variable N1 = new Variable();
            foreach (bool l1 in new ListPair(new Variable(), Ys).unify(Arg3))
            {
                foreach (bool l2 in N1.unify((int)YP.getValue(N) + 1))
                {
                    foreach (bool l3 in attack3(X, N1, Ys))
                        yield return false;
                }
            }
        }

        static IEnumerable<bool> rangeList(object M, object N, object List)
        {
            if ((int)YP.getValue(M) >= (int)YP.getValue(N))
            {
                foreach (bool l1 in YP.unify(List, new ListPair(N, Atom.NIL)))
                    yield return false;
            }
            else
            {
                Variable Tail = new Variable();
                foreach (bool l1 in rangeList((int)YP.getValue(M) + 1, (int)YP.getValue(N), Tail))
                {
                    foreach (bool l2 in YP.unify(List, new ListPair(M, Tail)))
                        yield return false;
                }
            }
        }

        static IEnumerable<bool> selectq(object X, object Arg2, object Arg3)
        {
            foreach (bool l in new ListPair(X, Arg3).unify(Arg2))
                yield return false;

            Variable Y = new Variable();
            Variable Ys = new Variable();
            Variable Zs = new Variable();
            foreach (bool l1 in new ListPair(Y, Ys).unify(Arg2))
            {
                foreach (bool l2 in new ListPair(Y, Zs).unify(Arg3))
                {
                    foreach (bool l3 in selectq(X, Ys, Zs))
                        yield return false;
                }
            }
        }
    }
}
