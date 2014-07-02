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

using System;
using System.IO;
using System.Collections.Generic;
using YieldProlog;

// This is the code for tutorial4.html in the doc directory.
class Tutorial4
{
    static void Main(string[] args)
    {
        YP.assertFact(Atom.a("brother"), 
            new object[] { Atom.a("Hillary"), Atom.a("Hugh") });
        YP.assertFact(Atom.a("brother"), 
            new object[] { Atom.a("Hillary"), Atom.a("Tony") });
        YP.assertFact(Atom.a("brother"), 
            new object[] { Atom.a("Bill"), Atom.a("Roger") });

        Variable Brother = new Variable();
        Console.WriteLine("Using dynamic assert:");
        foreach (bool l1 in YP.matchDynamic
                 (Atom.a("brother"), new object[] 
                  { Atom.a("Hillary"), Brother}))
            Console.WriteLine("Hillary has brother " +
                Brother.getValue() + ".");

        string prologCode =
            "uncle(Person, Uncle) :- \n" +
            "  parent(Person, Parent), \n" +
            "  brother(Parent, Uncle). \n";
        Console.WriteLine("\n// Compiled code:");
        compileAndWrite(prologCode);

        prologCode =
            ":- import('', [parent/2]). \n" +
            "uncle(Person, Uncle) :- \n" +
            "  parent(Person, Parent), \n" +
            "  brother(Parent, Uncle). \n";
        Console.WriteLine("// Calling an imported function:");
        compileAndWrite(prologCode);

        prologCode =
            "parent('Chelsea', 'Hillary'). \n" +
            "parent('Chelsea', 'Bill'). \n" +

            "uncle(Person, Uncle) :- \n" +
            "  parent(Person, Parent), \n" +
            "  brother(Parent, Uncle). \n";
        Console.WriteLine("// Calling a locally-defined function:");
        compileAndWrite(prologCode);

        prologCode =
            ":- import('', [parent/2]). \n" +
            "uncle(Person, Uncle) :- \n" +
            "  Goal = parent(Person, Parent), \n" +
            "  Goal, \n" +
            "  brother(Parent, Uncle). \n";
        Console.WriteLine("// Calling a dynamic goal:");
        compileAndWrite(prologCode);

        Console.WriteLine("Calling compiled code having a dynamic goal:");
        Variable Person = new Variable();
        Variable Uncle = new Variable();
        foreach (bool l1 in uncle(Person, Uncle))
            Console.WriteLine(Person.getValue() +
                " has uncle " + Uncle.getValue() + ".");

        Console.WriteLine("\nPress Enter to finish.");
        Console.ReadLine();
    }

    static void compileAndWrite(string prologCode)
    {
        YP.tell(Console.Out);
        YP.see(new StringReader(prologCode));
        Variable TermList = new Variable();
        Variable PseudoCode = new Variable();
        foreach (bool l1 in Parser.parseInput(TermList))
        {
            foreach (bool l2 in Compiler.makeFunctionPseudoCode
                     (TermList, PseudoCode))
                Compiler.convertFunctionCSharp(PseudoCode);
        }
        YP.seen();
    }

    public class YPInnerClass { }
    public static Type getDeclaringClass()
    {
        return typeof(YPInnerClass).DeclaringType;
    }

    public static IEnumerable<bool> parent
        (object Person, object Parent)
    {
        foreach (bool l1 in YP.unify
                 (Person, Atom.a("Chelsea")))
        {
            foreach (bool l2 in YP.unify
                     (Parent, Atom.a("Hillary")))
                yield return false;
        }
        foreach (bool l1 in YP.unify
                 (Person, Atom.a("Chelsea")))
        {
            foreach (bool l2 in YP.unify
                     (Parent, Atom.a("Bill")))
                yield return false;
        }
    }

    public static IEnumerable<bool> uncle
          (object Person, object Uncle)
    {
        {
            Variable Goal = new Variable();
            Variable Parent = new Variable();
            foreach (bool l2 in YP.unify
                     (Goal, new Functor2
                      (Atom.a("parent", Atom.a("")),
                       Person, Parent)))
            {
                foreach (bool l3 in YP.getIterator
                         (Goal, getDeclaringClass()))
                {
                    foreach (bool l4 in YP.matchDynamic
                             (Atom.a("brother"), new object[] { Parent, Uncle }))
                    {
                        yield return false;
                    }
                }
            }
        }
    }
}
