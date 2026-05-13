"""
Exercise: count up to 5.

You have to add the right rules, as indicated.
"""

import pyactr as actr

actr.chunktype("counting", "state counted end")

environment = actr.Environment(focus_position=(20, 20))

counter = actr.ACTRModel(environment, #rule_firing=0.05
        rule_firing=0.5)

counter.goal.add(actr.chunkstring(name="reading", string="""
        isa     counting
        state   start
        counted 0
        end     5"""))


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

counter.productionstring(name="encode first letter", string="""
        =g>
        isa     counting
        state   encode
        counted 0
        =visual>
        isa     _visual
        value   ~None
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        counted 1
        """)

counter.productionstring(name="encode second letter", string="""
        =g>
        isa     counting
        state   encode
        counted 1
        =visual>
        isa     _visual
        value   ~None
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        counted 2
        """)

counter.productionstring(name="encode third letter", string="""
        =g>
        isa     counting
        state   encode
        counted 2
        =visual>
        isa     _visual
        value   ~None
        ==>
        ~visual>
        =g>
        isa     counting
        state   search
        counted 3
        """)

#uncomment and work on
#counter.productionstring(name="encode fourth letter", string="""
#        """)

#counter.productionstring(name="encode fifth letter", string="""
#        """)

counter.productionstring(name="stop", string="""
        =g>
        isa     counting
        state   search
        counted =x
        end     =x
        ==>
        ~g>
        """)

counter.productionstring(name="find_probe", string="""
        =g>
        isa     counting
        state   search
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
        screen_x lowest""")


if __name__ == "__main__":
    text = {1: {'text': 'A', 'position': (100,100)}, 2: {'text': 'A', 'position': (300,120)}, 3: {'text': 'A', 'position': (450,80)}, 4: {'text': 'A', 'position': (490,110)}, 5: {'text': 'A', 'position': (520,100)}}
    sim = counter.simulation(realtime=True, environment_process=environment.environment_process, stimuli=text, triggers='A', times=1)
    sim.run()
