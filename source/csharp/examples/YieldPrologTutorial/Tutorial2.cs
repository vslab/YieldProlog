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
using YieldProlog;

// This is the code for tutorial2.html in the doc directory.
class Tutorial2
{
    static void Main(string[] args)
    {
        Console.WriteLine("Find relations:");
        Variable Brother = new Variable();
        foreach (bool l1 in brother("Hillary", Brother))
            Console.WriteLine("Hillary has brother " +
                Brother.getValue() + ".");

        Console.WriteLine("Check if it is square:");
        foreach (bool l1 in squaredRectangle(10, 10))
            Console.WriteLine("10 by 10 rectangle is square.");

        Console.WriteLine("Make it square:");
        Variable Width = new Variable();
        Variable Height = new Variable();
        foreach (bool l1 in Width.unify(10))
        {
            foreach (bool l2 in squaredRectangle(Width, Height))
                Console.WriteLine("A square of width " +
                    Width.getValue() + " has height " +
                    Height.getValue() + ".");
        }

        Console.WriteLine("Make it square before we know the width:");
        foreach (bool l1 in squaredRectangle(Width, Height))
        {
            foreach (bool l2 in Width.unify(10))
                Console.WriteLine("A square of width " +
                    Width.getValue() + " has height " +
                    Height.getValue() + ".");
        }

        Console.WriteLine("Get one match:");
        foreach (bool l1 in anyBrother("Hillary", Brother))
            Console.WriteLine("Hillary has a brother " +
                Brother.getValue() + ".");
        foreach (bool l1 in anyBrother("Bill", Brother))
            Console.WriteLine("Bill has a brother " +
                Brother.getValue() + ".");

        Console.WriteLine("Use cut for negation:");
        foreach (bool l1 in noBrother("Hillary"))
            Console.WriteLine("Hillary has no brother.");
        foreach (bool l1 in noBrother("Chelsea"))
            Console.WriteLine("Chelsea has no brother.");

        Console.WriteLine("\nPress Enter to finish.");
        Console.ReadLine();
    }

    static IEnumerable<bool> brother(object Person, object Brother)
    {
        foreach (bool l1 in YP.unify(Person, "Hillary"))
        {
            foreach (bool l2 in YP.unify(Brother, "Tony"))
                yield return false;
            foreach (bool l2 in YP.unify(Brother, "Hugh"))
                yield return false;
        }
        foreach (bool l1 in YP.unify(Person, "Bill"))
        {
            foreach (bool l2 in YP.unify(Brother, "Roger"))
                yield return false;
        }
    }

    static IEnumerable<bool> squaredRectangle(object Width, object Height)
    {
        foreach (bool l1 in YP.unify(Width, Height))
            yield return false;
    }

    static IEnumerable<bool> anyBrother(object Person, object Brother)
    {
        foreach (bool l1 in brother(Person, Brother))
        {
            yield return false;
            break;
        }
    }

    static IEnumerable<bool> noBrother(object Person)
    {
        Variable Brother = new Variable();
        foreach (bool l1 in brother(Person, Brother))
            yield break;
        yield return false;
    }
}
