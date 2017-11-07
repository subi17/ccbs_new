from gearbox.migrations import Migration

class AddTablerightgroup(Migration):

    database = "star"

    def up(self):
        t = self.table('rightgroup', area="Sta_Data_256", label="Right groups", dump_name="rightgrp")
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=2, order=10, help="Right group code")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=3, order=20, help="Rightgroup name")
        t.index('rightgroup', [['rightgroup']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('rightgroup')
