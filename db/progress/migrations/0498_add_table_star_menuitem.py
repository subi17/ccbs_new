from gearbox.migrations import Migration

class AddTablemenuitem(Migration):

    database = "star"

    def up(self):
        t = self.table('menuitem', area="Sta_Data_256", label="MenuItem", dump_name="menuitem")
        t.column('applcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Application", position=2, order=10, help="Application code. Internal use only")
        t.column('menucode', 'character', format="X(8)", initial="", max_width=16, label="MenuCode", position=3, order=20, help="MenuCode for menu")
        t.column('itemnumber', 'integer', format=">9", initial="0", max_width=4, label="Number", position=5, order=50, help="Itemnumber.")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=7, order=60, help="Programcode")
        t.column('submenucode', 'character', format="X(8)", initial="", max_width=16, label="SubMenuCode", position=8, order=80, help="Menumenu code for menu")
        t.column('subapplcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="SubApplication", position=9, order=70, help="Submenu application code.")
        t.index('item', [['applcode'], ['menucode'], ['itemnumber']], area="Sta_Index_2", primary=True, unique=True)
        t.index('programcode', [['programcode']], area="Sta_Index_2")
        t.index('submenu', [['subapplcode'], ['submenucode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('menuitem')
