from gearbox.migrations import Migration

class AddTablemenu(Migration):

    database = "star"

    def up(self):
        t = self.table('menu', area="Sta_Data_256", label="Menu", dump_name="menu")
        t.column('applcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Application", position=2, order=10, help="Application code. Internal use only")
        t.column('menucode', 'character', format="X(8)", initial="", max_width=16, label="MenuCode", position=3, order=20, help="MenuCode for menu")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=4, order=30, help="Menu name")
        t.column('version', 'character', format="X(8)", initial="", max_width=16, label="Version", position=5, order=40, help="Version. Blank = \"default\"")
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=6, order=50, help="Right group code")
        t.column('visible', 'logical', format="yes/no", initial="yes", max_width=1, label="Visible", position=7, order=60, help="If no, only programs can call this menu")
        t.column('menutooltip', 'character', format="X(256)", initial="", max_width=512, label="Tooltip", position=8, order=70, help="Menu tooltip.")
        t.column('sortField', 'character', format="X(12)", initial="", max_width=24, label="SortField", position=9, order=80, help="Sort order of menues")
        t.column('iconfile', 'character', format="X(40)", initial="", max_width=80, label="Iconfile", position=10, order=90, help="Icon shows in menues..")
        t.index('main', [['applcode'], ['menucode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('menucode', [['menucode']], area="Sta_Index_2")
        t.index('name', [['name']], area="Sta_Index_2")
        t.index('sortField', [['sortField']], area="Sta_Index_2")

    def down(self):
        self.drop_table('menu')
