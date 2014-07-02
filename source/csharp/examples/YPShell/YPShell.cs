using System;
using System.IO;
using System.Collections.Generic;
using YieldProlog;

/// <summary>
/// YPShell has a Main function that provides a command prompt to evaluate Prolog statements
/// and display the results.
/// </summary>
public class YPShell
{
    static void Main(string[] args)
    {
        Console.WriteLine("Yield Prolog Shell.  Enter ctrl-Z to exit.");

        while (true)
        {
            Console.Write("?- ");
            string goalString = Console.ReadLine();
            if (goalString == null)
                break;
            if (goalString == "")
            {
                Console.WriteLine("Enter ctrl-Z to exit.");
                continue;
            }

            Variable Goal = new Variable();
            Variable VariableList = new Variable();
            foreach (bool l1 in parseGoal(goalString, Goal, VariableList))
            {
                try
                {
                    bool gotMatch = false;
                    foreach (bool l2 in YP.getIterator(Goal, null))
                    {
                        gotMatch = true;
                        if (YP.getValue(VariableList) != Atom.NIL)
                        {
                            // We are showing values, so allow the user to enter ";" to display the next match.
                            writeValues(VariableList);
                            if (!Console.ReadLine().StartsWith(";"))
                                break;
                        }
                    }

                    if (gotMatch)
                    {
                        if (YP.getValue(VariableList) == Atom.NIL)
                            // We didn't show any values.  So just write true after matching (one or more times).
                            Console.WriteLine("true");
                    }
                    else
                        Console.WriteLine("fail");
                }
                catch (PrologException exception)
                {
                    Console.WriteLine(exception._term.ToString());
                }
            }
        }
    }

    /// <summary>
    /// Parse goalString and yield for each Goal, setting VariableList to a list of 
    /// (variableAtom = Var).
    /// </summary>
    /// <param name="goalString"></param>
    /// <param name="Goal"></param>
    /// <param name="VariableList"></param>
    /// <returns></returns>
    static IEnumerable<bool> parseGoal(string goalString, object Goal, object VariableList)
    {
        // The parser requires a newline at the end.
        YP.see(new StringReader(goalString + "\n"));
        object TermList = new Variable();
        // parseInput set TermList to a list of f(Goal, VariableList).
        foreach (bool l1 in Parser.parseInput(TermList))
        {
            // Close the input now before yielding.
            YP.seen();
            // Iterate through each member of TermList.
            for (TermList = YP.getValue(TermList);
                 TermList is Functor2 && ((Functor2)TermList)._name == Atom.DOT;
                 TermList = YP.getValue(((Functor2)TermList)._arg2))
            {
                // Unify the head of the list with f(Goal, VariableList).
                foreach (bool l2 in YP.unify
                        (((Functor2)TermList)._arg1, new Functor2(Atom.F, Goal, VariableList)))
                    yield return false;
            }
            yield break;
        }
        // Close the input in case parseInput failed.
        YP.seen();
    }

    /// <summary>
    /// Write out each entry in VariableList which is a list of (variableAtom = Var)
    /// </summary>
    /// <param name="VariableList"></param>
    static void writeValues(object VariableList)
    {
        for (VariableList = YP.getValue(VariableList);
             VariableList is Functor2 && ((Functor2)VariableList)._name == Atom.DOT;
             VariableList = YP.getValue(((Functor2)VariableList)._arg2))
        {
            Functor2 variableAndValue = YP.getValue(((Functor2)VariableList)._arg1) as Functor2;
            if (variableAndValue != null)
            {
                Console.WriteLine("");
                Console.Write(YP.getValue(variableAndValue._arg1));
                Console.Write(" = ");
                Console.Write(YP.getValue(variableAndValue._arg2));
            }
        }
    }
}
