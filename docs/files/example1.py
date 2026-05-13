"""
A basic model that simulates subject-verb agreement in production.
We abstract away from syntactic parsing, among other things.

Assume that the sentence you want to produce is: John definitely
sleeps.
Right now, we want to pronounce the word sleep in its right form.

We have to retrieve the subject. Then we have to match the number
subject has to the number of the verb form.
"""

import pyactr as actr

#I: create chunks

#chunktypes
actr.chunktype("read", "task current_word")
actr.chunktype("word", "form category meaning number person function")
actr.chunktype("concept", "meaning")

#chunks
chunk_john = actr.chunkstring(string="""
        isa word
        form    john
        category noun
        meaning john
        number sg
        person 3
        function subject""")

chunk_sleeps = actr.chunkstring(string="""
        isa word
        form    sleeps
        category verb
        meaning sleep
        number sg
        person 3
        function predicate""")

starting_goal = actr.chunkstring(string="""
        isa     read
        task    speaking
        current_word None""")

chunk_definitely = actr.chunkstring(string="""
        isa word
        form    definitely
        category adverb
        meaning     definitely
        function speaker_adverb""")

chunk_concept_sleep = actr.chunkstring(string="""
        isa concept
        meaning sleep""")

#II: create model

agreement = actr.ACTRModel()

#III: store chunks in the declarative memory of the model and buffers

agreement.decmem.add(chunk_john)
agreement.decmem.add(chunk_sleeps)

agreement.goal.add(starting_goal)

agreement.decmem.add(chunk_definitely)

#IV: create extra modules

agreement.set_goal(name="imaginal", delay=0.05)

agreement.goals["imaginal"].add(chunk_concept_sleep)

agreement.productionstring(name="match current word", string="""
    =g>
    isa read
    task    speaking
    =imaginal>
    isa     concept
    meaning     sleep
    ==>
    =g>
    isa read
    task recalling_subject
    +retrieval>
    isa word
    category noun
    function subject
    """)

agreement.productionstring(name="agree", string="""
    =g>
    isa read
    task recalling_subject
    =imaginal>
    isa     concept
    meaning =x
    =retrieval>
    isa word
    category noun
    function subject
    number =n
    ==>
    =g>
    isa read
    task    recalling_verb
    +retrieval>
    isa     word
    category    verb
    meaning     =x
    number      =n
    """)

agreement.productionstring(name="done", string="""
    =g>
    isa read
    task recalling_verb
    ?retrieval>
    state   free
    =retrieval>
    isa     word
    ==>
    ~imaginal>
    =g>
    isa     read
    task    done
    current_word =retrieval
    """)

if __name__ == "__main__":
    agreement_sim = agreement.simulation()
    agreement_sim.run()
    print("\nGoals at the end of the simulation:")
    print(agreement.goals)
    print("\nDeclarative memory at the end of the simulation:")
    print(agreement.decmem)
