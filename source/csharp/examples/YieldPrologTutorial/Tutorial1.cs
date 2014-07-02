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
using System.Collections.Generic;

// This is the code for tutorial1.html in the doc directory.
class Tutorial1
{
    static void Main(string[] args)
    {
        Console.WriteLine("Names using a return value:");
        foreach (string p in personWithReturnValue())
            Console.WriteLine(p);

        Console.WriteLine("Names using SimpleVariable:");
        SimpleVariable P = new SimpleVariable();
        foreach (bool l1 in personWithSimpleVariable(P))
            Console.WriteLine(P._value);

        Console.WriteLine("Names using UnifyingVariable:");
        UnifyingVariable Person = new UnifyingVariable();
        foreach (bool l1 in personWithUnify(Person))
            Console.WriteLine(Person._value);

        Console.WriteLine("Use unify to check a person:");
        foreach (bool l1 in Person.unify("Hillary"))
        {
            foreach (bool l2 in personWithUnify(Person))
                Console.WriteLine("Hillary is a person.");
        }
        foreach (bool l1 in Person.unify("Buddy"))
        {
            foreach (bool l2 in personWithUnify(Person))
                // This won't print.
                Console.WriteLine("Buddy is a person.");
        }

        Console.WriteLine("Use generalUnify to check a person:");
        foreach (bool l1 in person("Hillary"))
            Console.WriteLine("Hillary is a person.");
        foreach (bool l1 in person("Buddy"))
            // This won't print.
            Console.WriteLine("Buddy is a person.");

        Console.WriteLine("Find relations:");
        UnifyingVariable Brother = new UnifyingVariable();
        foreach (bool l1 in brother("Hillary", Brother))
            Console.WriteLine("Hillary has brother " +
                Brother._value + ".");

        Console.WriteLine("Joining functions:");
        UnifyingVariable Uncle = new UnifyingVariable();
        foreach (bool l1 in uncle(Person, Uncle))
            Console.WriteLine(Person._value +
                " has uncle " + Uncle._value + ".");

        Console.WriteLine("\nPress Enter to finish.");
        Console.ReadLine();
    }

    static IEnumerable<string> personWithReturnValue()
    {
        yield return "Chelsea";
        yield return "Hillary";
        yield return "Bill";
    }

    static IEnumerable<bool> personWithSimpleVariable(SimpleVariable Person)
    {
        Person._value = "Chelsea";
        yield return false;
        Person._value = "Hillary";
        yield return false;
        Person._value = "Bill";
        yield return false;
    }

    static IEnumerable<bool> personWithUnify(UnifyingVariable Person)
    {
        foreach (bool l1 in Person.unify("Chelsea"))
            yield return false;
        foreach (bool l1 in Person.unify("Hillary"))
            yield return false;
        foreach (bool l1 in Person.unify("Bill"))
            yield return false;
    }

    static object generalGetValue(object value)
    {
        if (value is UnifyingVariable)
        {
            if (!((UnifyingVariable)value)._isBound)
                return value;
            else
                return ((UnifyingVariable)value)._value;
        }
        else
            return value;
    }

    static IEnumerable<bool> generalUnify(object arg1, object arg2)
    {
        object arg1Value = generalGetValue(arg1);
        object arg2Value = generalGetValue(arg2);
        if (arg1Value is UnifyingVariable)
        {
            foreach (bool l1 in ((UnifyingVariable)arg1Value).unify(arg2Value))
                yield return false;
        }
        else if (arg2Value is UnifyingVariable)
        {
            foreach (bool l1 in ((UnifyingVariable)arg2Value).unify(arg1Value))
                yield return false;
        }
        else
        {
            // Arguments are "normal" types.
            if (arg1Value.Equals(arg2Value))
                yield return false;
        }
    }

    static IEnumerable<bool> person(object Person)
    {
        foreach (bool l1 in generalUnify(Person, "Chelsea"))
            yield return false;
        foreach (bool l1 in generalUnify(Person, "Hillary"))
            yield return false;
        foreach (bool l1 in generalUnify(Person, "Bill"))
            yield return false;
    }

    static IEnumerable<bool> brother(object Person, object Brother)
    {
        foreach (bool l1 in generalUnify(Person, "Hillary"))
        {
            foreach (bool l2 in generalUnify(Brother, "Tony"))
                yield return false;
            foreach (bool l2 in generalUnify(Brother, "Hugh"))
                yield return false;
        }
        foreach (bool l1 in generalUnify(Person, "Bill"))
        {
            foreach (bool l2 in generalUnify(Brother, "Roger"))
                yield return false;
        }
    }

    static IEnumerable<bool> parent(object Person, object Parent)
    {
        foreach (bool l1 in generalUnify(Person, "Chelsea"))
        {
            foreach (bool l2 in generalUnify(Parent, "Hillary"))
                yield return false;
        }
        foreach (bool l1 in generalUnify(Person, "Chelsea"))
        {
            foreach (bool l2 in generalUnify(Parent, "Bill"))
                yield return false;
        }
    }

    static IEnumerable<bool> uncle(object Person, object Uncle)
    {
        UnifyingVariable Parent = new UnifyingVariable();
        foreach (bool l1 in parent(Person, Parent))
        {
            foreach (bool l2 in brother(Parent, Uncle))
                yield return false;
        }
    }
}

class SimpleVariable
{
    public object _value;
}

class UnifyingVariable
{
    public object _value;
    public bool _isBound = false;

    public IEnumerable<bool> unify(object arg)
    {
        if (!_isBound)
        {
            _value = arg;
            _isBound = true;
            yield return false;
            // Remove the binding.
            _isBound = false;
        }
        else if (_value.Equals(arg))
            yield return false;
    }
}
