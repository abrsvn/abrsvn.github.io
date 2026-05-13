"""
Exercise: verify most (e.g., most letters are As).

Here is one possible answer to the question. This would represent a pairing strategy (people split As and Bs into pairs). However, we know that people tend not to use this strategy for most. For details, see Lidz et al., 2011, Interface transparency and the psychosemantics of most.
"""

import pyactr as actr

#create chunktype counting to fit your needs

actr.chunktype("counting", "state found")

environment = actr.Environment(focus_position=(20, 20))

counter = actr.ACTRModel(environment, #rule_firing=0.05
        rule_firing=0.5)

counter.visualBuffer("visual", "visual_location", counter.decmem,
        finst=10)#this is to keep 10 objects in finst (fingers of
                #instatiation

counter.goal.add(actr.chunkstring(name="reading", string="""
        isa     counting
        state   start
        found   None
        """))

counter.productionstring(name="move attention", string="""
        =g>
        isa     counting
        state   start
        ?visual_location>
        buffer  full
        =visual_location>
        isa     _visuallocation
        ?visual>
        buffer  empty
        state   free
        ==>
        =g>
        isa     counting
        state   encode
        ~visual_location>
        +visual>
        isa     _visual
        cmd     move_attention
        screen_pos =visual_location""")

counter.productionstring(name="move attention B", string="""
        =g>
        isa     counting
        state   startB
        ?visual_location>
        buffer  full
        =visual_location>
        isa     _visuallocation
        ?visual>
        buffer  empty
        state   free
        ==>
        =g>
        isa     counting
        state   encode
        ~visual_location>
        +visual>
        isa     _visual
        cmd     move_attention
        screen_pos =visual_location""")

counter.productionstring(name="encode A letter", string="""
        =g>
        isa     counting
        state   encode
        =visual>
        isa     _visual
        value   A
        ==>
        ~visual>
        =g>
        isa     counting
        state   searchB
        """)

counter.productionstring(name="encode B letter", string="""
        =g>
        isa     counting
        state   encode
        =visual>
        isa     _visual
        value   B
        ==>
        ~visual>
        =g>
        isa     counting
        state   searchA
        """)

counter.productionstring(name="find_A_probe", string="""
        =g>
        isa     counting
        state   searchA
        ?visual_location>
        buffer  empty
        ==>
        =g>
        isa     counting
        state   start
        ?visual_location>
        attended False
        +visual_location>
        isa _visuallocation
        value       A
        screen_x lowest""")

counter.productionstring(name="find_B_probe", string="""
        =g>
        isa     counting
        state   searchB
        ?visual_location>
        buffer  empty
        ==>
        =g>
        isa     counting
        state   startB
        ?visual_location>
        attended False
        +visual_location>
        isa _visuallocation
        value       B
        screen_x lowest""")


#uncomment and work on the the rule

counter.productionstring(name="most letters are A", string="""
        =g>
        isa     counting
        state   startB
        ?visual_location>
        state   error
        ==>
        =g>
        isa     counting
        state   success
        """)

counter.productionstring(name="not true that most letters are A", string="""
        =g>
        isa     counting
        state   start
        ?visual_location>
        state   error
        ==>
        =g>
        isa     counting
        state   failure
        """)

if __name__ == "__main__":
    text = {1: {'text': 'A', 'position': (100,100)}, 2: {'text': 'B', 'position': (240,120)}, 3: {'text': 'A', 'position': (400,200)}, 4: {'text': 'B', 'position': (150,190)}, 5: {'text': 'A', 'position': (320,230)}}
    #text = {1: {'text': 'A', 'position': (100,100)}, 2: {'text': 'B', 'position': (240,120)}, 3: {'text': 'A', 'position': (400,200)}, 4: {'text': 'B', 'position': (150,190)}, 5: {'text': 'A', 'position': (320,230)}}
    sim = counter.simulation(realtime=True, environment_process=environment.environment_process, stimuli=text, triggers='A', times=1)
    sim.run()
