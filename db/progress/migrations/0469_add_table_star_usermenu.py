from gearbox.migrations import Migration

class AddTableusermenu(Migration):

    database = "star"

    def up(self):
        t = self.table('usermenu', area="Sta_Data_256", label="User menus", dump_name="usermenu")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=2, order=10, help="User code")
        t.column('menucode', 'character', format="X(8)", initial="", max_width=16, label="MenuCode", position=4, order=30, help="MenuCode for menu")
        t.column('positionx', 'integer', format=">>>>9", initial="0", max_width=4, label="Pos-X", position=5, order=40, help="Position x on the screen")
        t.column('positiony', 'integer', format=">>>>9", initial="0", max_width=4, label="Pos-Y", position=6, order=50, help="Position y on the screen")
        t.column('applcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Application", position=7, order=20, help="Application code. Internal use only")
        t.column('startup', 'logical', format="yes/no", initial="no", max_width=1, label="StartUp", position=8, order=60, help="Menu will be start up automaticly on start")
        t.index('usercode', [['usercode'], ['applcode'], ['menucode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('menucode', [['applcode'], ['menucode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('usermenu')
