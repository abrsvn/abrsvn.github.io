"""
A simple top-down parser.
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

actr.chunktype("parsing_goal", "stack_top stack_middle stack_bottom parsed_word task")
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
    form about
    cat P
"""))

dm.add(actr.chunkstring(string="""
    isa word
    form by
    cat P
"""))

g.add(actr.chunkstring(string="""
    isa parsing_goal
    task read_word
    stack_top S
"""))

parser.productionstring(name="press spacebar", string="""
    =g>
    isa             parsing_goal
    task            press_space
    stack_top       ~None
    ?manual>
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
""")

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

parser.productionstring(name="match category", string="""
    =g>
    isa             parsing_goal
    task            match_category
    ?retrieval>
    state           free
    buffer          full
    =retrieval>
    isa             word
    cat             =c
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    parsed_word     =c
""")

parser.productionstring(name="expand: S ==> NP VP", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top S
    stack_middle =x
    ==>
    =g>
    isa parsing_goal
    stack_top NP
    stack_middle VP
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      S
    daughter1   NP
    daughter2   VP
""")

parser.productionstring(name="expand: NP ==> NP VP", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top NP
    stack_middle =x
    =imaginal>
    isa         parse_state
    mother      ~NP
    ==>
    =g>
    isa parsing_goal
    stack_top NP
    stack_middle VP
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      NP
    daughter1   NP
    daughter2   VP
""", utility=-1) #utility above 0 ensures that this rule takes precendence
#over the rule expand: NP ==> D N

parser.productionstring(name="expand: NP ==> D N", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top NP
    stack_middle =x
    ?imaginal>
    state   free
    ==>
    =g>
    isa parsing_goal
    stack_top D
    stack_middle N
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      NP
    daughter1   D
    daughter2   N
""")

parser.productionstring(name="expand: VP ==> V NP", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top VP
    stack_middle =x
    ==>
    =g>
    isa parsing_goal
    stack_top V
    stack_middle NP
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      VP
    daughter1   V
    daughter2   NP
""")

parser.productionstring(name="expand: VP ==> V PP", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top VP
    stack_middle =x
    ==>
    =g>
    isa parsing_goal
    stack_top V
    stack_middle PP
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      VP
    daughter1   V
    daughter2   PP
""", utility=1) #utility>0 ensures that this rule takes precedence
#over the rule expand: VP ==> V NP

parser.productionstring(name="expand: PP ==> P NP", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top PP
    stack_middle =x
    ==>
    =g>
    isa parsing_goal
    stack_top P
    stack_middle NP
    stack_bottom =x
    +imaginal>
    isa         parse_state
    mother      PP
    daughter1   P
    daughter2   NP
""")

parser.productionstring(name="scan: word", string="""
    =g>
    isa parsing_goal
    task parsing
    stack_top =y
    stack_middle =x
    stack_bottom =b
    parsed_word =y
    ==>
    =g>
    isa parsing_goal
    task press_space
    stack_top =x
    stack_middle =b
    stack_bottom None
    parsed_word None
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
        parser_sim.run(3)
    sortedDM = sorted(([item[0], time] for item in dm.items()\
                                       for time in item[1]),\
                      key=lambda item: item[1])
    print("\nParse states in declarative memory at the end of the simulation",
          "\nordered by time of (re)activation:")
    for chunk in sortedDM:
        if chunk[0].typename == "parse_state":
            print(chunk[1], "\t", chunk[0])
