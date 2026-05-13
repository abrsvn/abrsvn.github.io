"""
A left-corner parser.
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

actr.chunktype("parsing_goal",
               "task stack_1 stack_2 stack_3 stack_4 parsed_word")
actr.chunktype("parse_state",
               "mother daughter1 daughter2")
actr.chunktype("word", "form cat")

parser = actr.ACTRModel(environment)
dm = parser.decmem
g = parser.goal
imaginal = parser.set_goal(name="imaginal", delay=0)

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
    stack_1 S
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

parser.productionstring(name="shift and project word", string="""
    =g>
    isa             parsing_goal
    task            match_category
    stack_1         =s1
    stack_2         =s2
    stack_3         =s3
    stack_4         =s4
    =retrieval>
    isa             word
    form            =w
    cat             =c
    cat             ~=s1
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    stack_1       =c
    stack_2       =s1
    stack_3       =s2
    stack_4       =s3
    ~retrieval>
""")

parser.productionstring(name="shift and project and complete word", string="""
    =g>
    isa             parsing_goal
    task            match_category
    stack_1         =s1
    stack_2         =s2
    stack_3         =s3
    stack_4         =s4
    =retrieval>
    isa             word
    form            =w
    cat             =s1
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         =s2
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    ~retrieval>
""")

parser.productionstring(name="project: NP ==> D N", string="""
    =g>
    isa             parsing_goal
    stack_1         D
    stack_2         =s2
    stack_2         ~NP       
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    stack_1         N
    stack_2         NP
    stack_3         =s2
    stack_4         =s3
    +imaginal>
    isa             parse_state
    mother          NP
    daughter1       D
    daughter2       N
""")

parser.productionstring(name="project: NP ==> NP VP", string="""
    =g>
    isa             parsing_goal
    stack_1         NP
    stack_2         S
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    stack_1         VP
    stack_2         NP
    stack_3         S
    stack_4         =s3
    +imaginal>
    isa             parse_state
    mother          NP
    daughter1       NP
    daughter2       VP
""", utility=-1)


parser.productionstring(name="project and complete: NP ==> D N", string="""
    =g>
    isa             parsing_goal
    stack_1         D
    stack_2         NP       
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    stack_1         N
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    +imaginal>
    isa             parse_state
    mother          NP
    daughter1       D
    daughter2       N
""")

parser.productionstring(name="project and complete: PP ==> P NP", string="""
    =g>
    isa             parsing_goal
    stack_1         P
    stack_2         PP       
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    stack_1         NP
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    +imaginal>
    isa             parse_state
    mother          PP
    daughter1       P
    daughter2       NP
""")


parser.productionstring(name="project and complete: S ==> NP VP", string="""
    =g>
    isa             parsing_goal
    stack_1         NP
    stack_2         S
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         VP
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    +imaginal>
    isa             parse_state
    mother          S
    daughter1       NP
    daughter2       VP
""")

parser.productionstring(name="project and complete: VP ==> V NP", string="""
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         V
    stack_2         VP
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         NP
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    +imaginal>
    isa             parse_state
    mother          VP
    daughter1       V
    daughter2       NP
""")

parser.productionstring(name="project and complete: VP ==> V PP", string="""
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         V
    stack_2         VP
    stack_3         =s3
    stack_4         =s4
    ==>
    =g>
    isa             parsing_goal
    task            parsing
    stack_1         PP
    stack_2         =s3
    stack_3         =s4
    stack_4         None
    +imaginal>
    isa             parse_state
    mother          VP
    daughter1       V
    daughter2       PP
""", utility=1)

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
