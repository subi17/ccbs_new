from gearbox.migrations import Migration

class AddTableprogram(Migration):

    database = "star"

    def up(self):
        t = self.table('program', area="Sta_Index_2", label="Program", dump_name="program", desc="Program specification")
        t.column('program', 'character', format="X(50)", initial="", max_width=80, label="Program", position=2, order=10, help="Program with recursize directory")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=3, order=20, help="Program name. Use menu labels and program title")
        t.column('paramstr', 'character', format="X(40)", initial="", help="Parameters to program", max_width=80, label="Parameters", position=5, order=40, description="Program got these information in PRIVATE-DATA")
        t.column('iconfile', 'character', format="X(40)", initial="", max_width=80, label="Iconfile", position=6, order=50, help="Icon shows in menues..")
        t.column('menutooltip', 'character', format="X(256)", initial="", max_width=512, label="Tooltip", position=7, order=60, help="Program tooltip. Shows on menus...")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=8, order=30, help="Programcode")
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=9, order=70, help="Right group code")
        t.column('programstastus', 'logical', format="yes/no", initial="yes", max_width=1, label="Active", position=10, order=80, help="Program is runnable from menu")
        t.column('programtype', 'character', format="X(8)", initial="", max_width=16, label="Type", position=13, order=90, help="Program type")
        t.column('filegroup', 'character', format="X(12)", initial="", max_width=24, label="File Group", position=14, order=100, help="File group")
        t.column('canModified', 'logical', format="yes/no", initial="no", max_width=1, label="Modified", position=15, order=140, help="Can modified")
        t.column('canCopy', 'logical', format="yes/no", initial="no", max_width=1, label="Copy", position=16, order=120, help="Can copy")
        t.column('canDelete', 'logical', format="yes/no", initial="no", max_width=1, label="Delete", position=17, order=130, help="Can Delete")
        t.column('canCreate', 'logical', format="yes/no", initial="no", max_width=1, label="Create", position=18, order=110, help="Can create")
        t.column('PrintDestinations', 'character', format="X(40)", initial="", max_width=80, label="Print destinations", position=19, order=150, help="Available print destinations")
        t.column('archive', 'character', format="X(10)", initial="Default", help="Archive time", max_width=20, label="Archive", position=20, order=160, description='''Default
Year
Month
Week
Permanent''')
        t.index('programcode', [['programcode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('name', [['name']], area="Sta_Index_2")
        t.index('program', [['program']], description="Same program could be multible times with different parameters", area="Sta_Index_2")

    def down(self):
        self.drop_table('program')
