"""
A bottom-up parser.
"""

import re
import sys

import simpy
import pyactr as actr

try:
    SIMULATION = int(sys.argv[1])
except IndexError:
    SIMULATION = 1

environment = actr.Environment(focus_position=(320, 180))

actr.chunktype("parsing_goal", "stack_1 stack_2 stack_3 stack_4 stack_5 parsed_word task")
actr.chunktype("parse_state",
               "mother daughter1 daughter2")
actr.chunktype("word", "form cat")

parser = actr.ACTRModel(environment)
dm = parser.decmem
g = parser.goal
imaginal = parser.set_goal(name="imaginal", delay=0.05)

dm.add(actr.chunkstring(string="""
    isa word
    form evidence
    cat N
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form doctor
    cat N
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form the
    cat D
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form examined
    cat V
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form talked
    cat V
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form by
    cat P
"""))
dm.add(actr.chunkstring(string="""
    isa word
    form about
    cat P
"""))

g.add(actr.chunkstring(string="""
    isa parsing_goal
    task read_word
"""))

parser.productionstring(name="press spacebar", string="""
    =g>
    isa             parsing_goal
    task            parsing
    ?manual>
    state           free
    ?imaginal>
    state           free
    ==>
    =g>
    isa             parsing_goal
    task            read_word
    +manual>
    isa             _manual
    cmd             'press_key'
    key             'space'
    ~imaginal>
""", utility=-10)

parser.productionstring(name="encode word", string="""
    =g>
    isa             parsing_goal
    task            read_word
    =visual>
    isa             _visual
    value           =val
    ==>
    =g>
    isa             parsing_goal
    task            get_word_cat
    parsed_word    =val
    ~visual>
""")

parser.productionstring(name="retrieve category", string="""
    =g>
    isa             parsing_goal
    task            get_word_cat
    parsed_word     =w
    ==>
    +retrieval>
    isa             word
    form            =w
    =g>
    isa             parsing_goal
    task            match_category
""")

parser.productionstring(name="shift word and project it", string="""
        =g>
        isa         parsing_goal
        task       match_category
        stack_1   =s1
        stack_2   =s2
        stack_3   =s3
        stack_4   =s4
        stack_5   =s5
        =retrieval>
        isa         word
        cat         =y
        ==>
        =g>
        isa         parsing_goal
        task       parsing
        stack_1   =y
        stack_2   =s1
        stack_3   =s2
        stack_4   =s3
        stack_5   =s4
        ~retrieval>""")

parser.productionstring(name="reduce: NP ==> D N", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     N
        stack_2     D
        stack_3   =s3
        stack_4   =s4
        stack_5   =s5
        ==>
        =g>
        isa         parsing_goal
        stack_1   NP
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      NP
        daughter1   D
        daughter2   N
""")

parser.productionstring(name="reduce: NP ==> NP VP", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     VP
        stack_2     NP
        stack_3     =s3
        stack_4     =s4
        stack_5     =s5
        =imaginal>
        isa         parse_state
        mother      ~NP
        ==>
        =g>
        isa         parsing_goal
        stack_1   NP
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      NP
        daughter1   NP
        daughter2   VP
""", utility=-1)#utility above 0 ensures that this rule takes precendence
#over the rule expand: NP ==> D N


parser.productionstring(name="reduce: S ==> NP VP", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     VP
        stack_2     NP
        stack_3     =s3
        stack_4     =s4
        stack_5     =s5
        ?imaginal>
        state       free
        ==>
        =g>
        isa         parsing_goal
        stack_1   S
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      S
        daughter1   NP
        daughter2   VP
""")

parser.productionstring(name="reduce: VP ==> V NP", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     NP
        stack_2     V
        stack_3     =s3
        stack_4     =s4
        stack_5     =s5
        ==>
        =g>
        isa         parsing_goal
        stack_1   VP
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      VP
        daughter1   V
        daughter2   NP
""")

parser.productionstring(name="reduce: VP ==> V PP", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     PP
        stack_2     V
        stack_3     =s3
        stack_4     =s4
        stack_5     =s5
        ==>
        =g>
        isa         parsing_goal
        stack_1   VP
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      VP
        daughter1   V
        daughter2   PP
""")

parser.productionstring(name="reduce: PP ==> P NP", string="""
        =g>
        isa         parsing_goal
        task       parsing
        stack_1     NP
        stack_2     P
        stack_3     =s3
        stack_4     =s4
        stack_5     =s5
        ==>
        =g>
        isa         parsing_goal
        stack_1   PP
        stack_2   =s3
        stack_3   =s4
        stack_4   =s5
        stack_5   None
        +imaginal>
        isa         parse_state
        mother      PP
        daughter1   P
        daughter2   NP
""")

if __name__ == "__main__":
    stimuli = [
                [{1: {'text': 'the', 'position': (320, 180)}},
               {1: {'text': 'doctor', 'position': (320, 180)}},
               {1: {'text': 'talked', 'position': (320, 180)}},
               {1: {'text': 'about', 'position': (320, 180)}},
               {1: {'text': 'the', 'position': (320, 180)}},
               {1: {'text': 'evidence', 'position': (320, 180)}},
               ],
                [{1: {'text': 'the', 'position': (320, 180)}},
               {1: {'text': 'evidence', 'position': (320, 180)}},
               {1: {'text': 'examined', 'position': (320, 180)}},
               {1: {'text': 'by', 'position': (320, 180)}},
               {1: {'text': 'the', 'position': (320, 180)}},
               {1: {'text': 'doctor', 'position': (320, 180)}}
               ]]
    parser_sim = parser.simulation(
        realtime=SIMULATION,
        gui=SIMULATION,
        environment_process=environment.environment_process,
        stimuli=stimuli[0],
        triggers='space')
    if not SIMULATION:
        while True:
            try:
                parser_sim.step()
            except simpy.core.EmptySchedule:
                break
            if re.search("^KEY PRESSED: SPACE", parser_sim.current_event.action):
                print(parser.goals["g"])
                input()
    else:
        parser_sim.run(4)
    sortedDM = sorted(([item[0], time] for item in dm.items()\
                                       for time in item[1]),\
                      key=lambda item: item[1])
    print("\nParse states in declarative memory at the end of the simulation",
          "\nordered by time of (re)activation:")
    for chunk in sortedDM:
        if chunk[0].typename == "parse_state":
            print(chunk[1], "\t", chunk[0])
