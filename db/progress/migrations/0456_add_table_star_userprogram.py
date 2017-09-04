from gearbox.migrations import Migration

class AddTableuserprogram(Migration):

    database = "star"

    def up(self):
        t = self.table('userprogram', area="Sta_Data_256", label="User programs", dump_name="userprog", desc='''postition ?
''')
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=2, order=20, help="Programcode")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=3, order=10, help="User code")
        t.column('positionx', 'integer', format=">>>>9", initial="0", max_width=4, label="Pos-X", position=4, order=30, help="Position x on the screen")
        t.column('positiony', 'integer', format=">>>>9", initial="0", max_width=4, label="Pos-Y", position=5, order=40, help="Position y on the screen")
        t.column('paramstr', 'character', format="X(40)", initial="", help="Parameters to program", max_width=80, label="Parameters", position=6, order=50, description="Program got these information in PRIVATE-DATA")
        t.index('userprogram', [['usercode'], ['programcode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('userprogram')
