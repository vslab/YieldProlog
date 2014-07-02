# Copyright (C) 2007, Jeff Thompson
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
from PrologException import *

# An IndexedAnswers holds answers to a query based on the values of index arguments.
class IndexedAnswers(object):
    def __init__(self, arity):
        self._arity = arity
        # addAnswer adds the answer here and indexes it later.
        self._allAnswers = []
        # The key has the arity of answers with non-null values for each indexed arg.  The value
        # is a list of the matching answers.  The signature is implicit in the pattern on non-null index args.
        self._indexedAnswers = {}
        # Keeps track of whether we have started adding entries to _indexedAnswers for the signature.
        self._gotAnswersForSignature = {}

    MAX_INDEX_ARGS = 31
                                                                    
    # Append the answer to the list and update the indexes, if any.
    # Elements of answer must be ground, since arguments with unbound variables make this
    # into a dynamic rule which we don't index.
    def addAnswer(self, answer):
        self.addOrPrependAnswer(answer, False)

    # Prepend the answer to the list and clear the indexes so that they must be re-computed
    # on the next call to match.  (Only addAnswer will maintain the indexes while adding answers.)
    # Elements of answer must be ground, since arguments with unbound variables make this
    # into a dynamic rule which we don't index.
    def prependAnswer(self, answer):
        self.addOrPrependAnswer(answer, True)

    # Do the work of addAnswer or prependAnswer.
    def addOrPrependAnswer(self, answer, prepend):
        if len(answer) != self._arity:
            return

        # Store a copy of the answer array.
        copyStore = Variable.CopyStore()
        answerCopy = [YP.makeCopy(value, copyStore) for value in answer]
        if copyStore.getNUniqueVariables() > 0:
            raise "Elements of answer must be ground, but found " + `copyStore.getNUniqueVariables()` + " unbound variables"

        if prepend:
            self._allAnswers.insert(0, answerCopy)
            self.clearIndexes()
        else:
            self._allAnswers.append(answerCopy)
            # If match has already indexed answers for a signature, we need to add
            #    this to the existing indexed answers.
            for signature in self._gotAnswersForSignature.keys():
                self.indexAnswerForSignature(answerCopy, signature)
        
    def indexAnswerForSignature(self, answer, signature):
        # First find out which of the answer values can be used as an index.
        indexValues = [IndexedAnswers.getIndexValue(YP.getValue(value)) \
                        for value in answer]
        # We limit the number of indexed args in a 32-bit signature.
        for i in range(IndexedAnswers.MAX_INDEX_ARGS, len(indexValues)):
            indexValues[i] = None

        # We need an entry in indexArgs from indexValues for each 1 bit in signature.
        indexArgs = IndexedAnswers.HashedList()
        for i in range(len(indexValues)):
            if (signature & (1 << i)) == 0:
                indexArgs.append(None)
            else:
                if indexValues[i] == None:
                    # The signature wants an index value here, but we don't have one so
                    #   we can't add it as an answer for this signature.
                    return
                else:
                    indexArgs.append(indexValues[i])

        self.add(indexArgs, answer)

    # Add answer to _indexedAnswers for indexArgs, creating the entry if needed.
    def add(self, indexArgs, answer):
        try:
            answers = self._indexedAnswers[indexArgs]
        except KeyError:
            answers = []
            self._indexedAnswers[indexArgs] = answers

        answers.append(answer)

    def match(self, arguments):
        if len(arguments) != self._arity:
            return;

        # Set up indexArgs, up to arg position MAX_INDEX_ARGS.  The signature has a 1 bit for
        #   each non-null index arg.
        indexArgs = IndexedAnswers.HashedList()
        gotAllIndexArgs = True
        signature = 0
        for i in range(len(arguments)):
            indexValue = None
            if i < IndexedAnswers.MAX_INDEX_ARGS:
                # We limit the number of args in a 32-bit signature.
                indexValue = IndexedAnswers.getIndexValue(YP.getValue(arguments[i]))
                if indexValue != None:
                    signature += (1 << i)
            if indexValue == None:
                gotAllIndexArgs = False
            indexArgs.append(indexValue)

        if signature == 0:
            # No index args, so we have to match from _allAnswers.
            answers = self._allAnswers
        else:
            if not self._gotAnswersForSignature.has_key(signature):
                # We need to create the entry in _indexedAnswers.
                for answer in self._allAnswers:
                    self.indexAnswerForSignature(answer, signature)
                # Mark that we did this signature.
                self._gotAnswersForSignature[signature] = None
            try:
                answers = self._indexedAnswers[indexArgs]
            except KeyError:
                return

        if gotAllIndexArgs:
            # All the arguments were already bound, so we don't need to do bindings.
            yield False
            return

        # Find matches in answers.
        iterators = len(arguments)*[None]
        for answer in answers:
            gotMatch = True
            nIterators = 0
            # Try to bind all the arguments.
            for i in range(len(arguments)):
                if indexArgs._list[i] != None:
                    # We already matched this argument by looking up _indexedAnswers.
                    continue

                iterator = iter(YP.unify(arguments[i], answer[i]))
                iterators[nIterators] = iterator
                nIterators += 1
                # next() returns if YP.unify succeeds.
                try:
                    iterator.next()
                except StopIteration:
                    gotMatch = False
                    break

            try:
                if gotMatch:
                    yield True
            finally:
                # Manually finalize all the iterators.
                for i in range(nIterators):
                    iterators[i].close()

    def clause(self, Head, Body):
        Head = YP.getValue(Head)
        if isinstance(Head, Variable):
            raise PrologException("instantiation_error", "Head is an unbound variable")
        arguments = YP.getFunctorArgs(Head)

        # We always match Head from _allAnswers, and the Body is Atom.TRUE.
        for l1 in YP.unify(Body, Atom.TRUE):
            # The caller can assert another answer into this same predicate during yield, so we have to
            #   make a copy of the answers.
            for answer in [x for x in self._allAnswers]:
                for l2 in YP.unifyArrays(arguments, answer):
                    yield False

    def retract(self, Head, Body):
        Head = YP.getValue(Head)
        if isinstance(Head, Variable):
            raise PrologException("instantiation_error", "Head is an unbound variable")
        arguments = YP.getFunctorArgs(Head)

        # We always match Head from _allAnswers, and the Body is Atom.TRUE.
        for l1 in YP.unify(Body, Atom.TRUE):
            # The caller can assert another answer into this same predicate during yield, so we have to
            #   make a copy of the answers.
            for answer in [x for x in self._allAnswers]:
                for l2 in YP.unifyArrays(arguments, answer):
                    # Remove answer.
                    self._allAnswers.remove(answer)
                    self.clearIndexes()
                    yield False

    # After retracting or prepending an answer in _allAnswers, the indexes are invalid, so clear them.
    def clearIndexes(self):
        self._indexedAnswers = {}
        self._gotAnswersForSignature = {}
        
    # A HashedList holds a _list and has methods to get a hash and to check equality
    #   based on the elements of the list.
    class HashedList(object):
        def __init__(self):
            self._gotHashCode = False
            self._list = []

        def append(self, value):
            self._gotHashCode = False
            self._list.append(value)

        def __hash__(self):
            if not self._gotHashCode:
                hashCode = 1
                for obj in self._list:
                    hashCode = 31 * hashCode + hash(obj)
                self._hashCode = hashCode
                self._gotHashCode = True
            return self._hashCode

        def __eq__(self, obj):
            if not isinstance(obj, IndexedAnswers.HashedList):
                return False
            if len(obj._list) != len(self._list):
                return False

            for i in range(len(self._list)):
                if self._list[i] != obj._list[i]:
                    return False
            return True

        def __ne__(self, obj):
            return not self.__eq__(obj)

    # If we keep an index on value, return the value, or None if we don't index it.
    @staticmethod
    def getIndexValue(value):
        if isinstance(value, Atom) or isinstance(value, str): # or type(value) == type(1):
            return value
        else:
            return None
