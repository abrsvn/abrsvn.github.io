"""
Exercise: verify most (e.g., most letters are As).

Here is one possible answer to the question. This would represent a counting strategy (people first count As, then they count other elements and compare the cardinality of the two counted sets). This strategy is much more likely entertained by people. For details, see Lidz et al., 2011, Interface transparency and the psychosemantics of most.
"""

import pyactr as actr

#create chunktype counting to fit your needs

actr.chunktype("counting", "state countedA countedB end")

environment = actr.Environment(focus_position=(20, 20))

counter = actr.ACTRModel(environment, #rule_firing=0.05
        rule_firing=0.5, automatic_visual_search=False)

counter.visualBuffer("visual", "visual_location", counter.decmem,
        finst=10)#this is to keep 10 objects in finst (fingers of
                #instatiation

counter.goal.add(actr.chunkstring(name="reading", string="""
        isa     counting
        state   search
        countedA 0
        countedB 0
        """))

counter.productionstring(name="move attention", string="""
        =g>
        isa     counting
        state   start
        end     None
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

counter.productionstring(name="move attention for B", string="""
        =g>
        isa     counting
        state   startB
        end     ~None
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

counter.productionstring(name="encode first A letter", string="""
        =g>
        isa     counting
        state   encode
        countedA 0
        =visual>
        isa     _visual
        value   A
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        countedA 1
        """)

counter.productionstring(name="encode second A letter", string="""
        =g>
        isa     counting
        state   encode
        countedA 1
        =visual>
        isa     _visual
        value   A
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        countedA 2
        """)

counter.productionstring(name="encode third A letter", string="""
        =g>
        isa     counting
        state   encode
        countedA 2
        =visual>
        isa     _visual
        value   A
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        countedA 3
        """)

counter.productionstring(name="encode first B letter", string="""
        =g>
        isa     counting
        state   encode
        countedB 0
        =visual>
        isa     _visual
        value   B
        ==>
        ~visual>
        =g>
        isa     counting
        state   searchB
        countedB 1
        """)

counter.productionstring(name="encode second B letter", string="""
        =g>
        isa     counting
        state   encode
        countedB 1
        =visual>
        isa     _visual
        value   B
        ==>
        ~visual>
        =g>
        isa     counting
        state   searchB
        countedB 2
        """)

counter.productionstring(name="encode third B letter", string="""
        =g>
        isa     counting
        state   encode
        countedB 2
        =visual>
        isa     _visual
        value   B
        ==>
        ~visual>
        =g>
        isa     counting
        state   searchB
        countedB 3
        """)

counter.productionstring(name="find_A_probe", string="""
        =g>
        isa     counting
        state   search
        end     None
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
        end     ~None
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


counter.productionstring(name="finding_A_probe_failed", string="""
        =g>
        isa     counting
        state   start
        countedA    =x
        ?visual_location>
        state   error
        ==>
        =g>
        isa     counting
        state   searchB
        end     =x
        ~visual_location>
        """)#we will store what the number of A elements was in the slot 'end'.
#The Bs have to be below this number for 'most letters are As' to qualify as
#true

counter.productionstring(name="not true that most letters are A", string="""
        =g>
        isa     counting
        state   startB
        countedB    =x
        end         =x
        ==>
        =g>
        isa     counting
        state   failure
        """)
counter.productionstring(name="most letters are A (since finding B probe failed)", string="""
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

if __name__ == "__main__":
    text = {1: {'text': 'B', 'position': (100,100)}, 2: {'text': 'B', 'position': (240,120)}, 3: {'text': 'A', 'position': (400,200)}, 4: {'text': 'B', 'position': (150,190)}, 5: {'text': 'A', 'position': (320,230)}}
    #text = {1: {'text': 'A', 'position': (100,100)}, 2: {'text': 'B', 'position': (240,120)}, 3: {'text': 'A', 'position': (400,200)}, 4: {'text': 'B', 'position': (150,190)}, 5: {'text': 'A', 'position': (320,230)}}
    sim = counter.simulation(realtime=True, environment_process=environment.environment_process, stimuli=text, triggers='A', times=1)
    sim.run()
