from gearbox.migrations import Migration

class AddTableprogwin(Migration):

    database = "star"

    def up(self):
        t = self.table('progwin', area="Sta_Data_256", label="Program Windows", dump_name="progwin")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=2, order=10, help="Programcode")
        t.column('windownumber', 'integer', format=">9", initial="0", max_width=4, label="Window", position=5, order=20, help="Window number. Number 0 is automicly started")
        t.column('objectType', 'character', format="X(8)", initial="Smart", max_width=16, label="Type", position=6, order=40, help="Object type")
        t.column('object', 'character', format="X(50)", initial="", max_width=100, label="Object", position=7, order=50, help="Object filename. Blank for browsers")
        t.column('displayfields', 'character', format="X(50)", initial="", max_width=100, label="Displayed", position=8, order=80, help="Displayed fields. Blank for static objects")
        t.column('enabledFields', 'character', format="X(50)", initial="", max_width=100, label="Enabled", position=9, order=90, help="Enabled fields. Blank for static objects")
        t.column('canCreate', 'logical', format="yes/no", initial="no", max_width=1, label="Create", position=10, order=110, help="Can create")
        t.column('canModified', 'logical', format="yes/no", initial="no", max_width=1, label="Modified", position=11, order=120, help="Can modified")
        t.column('canCopy', 'logical', format="yes/no", initial="no", max_width=1, label="Copy", position=12, order=130, help="Can copy")
        t.column('canDelete', 'logical', format="yes/no", initial="no", max_width=1, label="Delete", position=13, order=140, help="Can Delete")
        t.column('sdo', 'character', format="X(50)", initial="", max_width=100, label="SDO", position=14, order=70, help="Program with recursize directory")
        t.column('title-command', 'character', format="X(50)", initial="", max_width=100, label="Title", position=15, order=150, help="Title command... $as-nro $as-nimi")
        t.column('foreingFields', 'character', format="X(50)", initial="", max_width=100, label="ForeignFields", position=16, order=100, help="Foreign fields if needed")
        t.column('TreeType', 'character', format="X(8)", initial="Program", max_width=16, label="TreeType", position=17, order=170, help="Tree type")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Menu name", position=18, order=180, help="Menu name")
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=19, order=190, help="Right group code")
        t.column('treeCode', 'character', format="X(12)", initial="", max_width=24, label="TreeCode", position=20, order=160, help="Exsample 1.1.2")
        t.column('helptext', 'character', format="X(256)", initial="", max_width=512, label="Helptext", position=21, order=200, help="Program helptext.")
        t.column('appsrv', 'character', mandatory=True, format="X(20)", initial="", max_width=40, label="AppServer", position=22, order=60, help="Logical name of Application Server")
        t.column('parentwindow', 'integer', format=">>9", initial="0", max_width=4, label="ParentWindow", position=23, order=30, help="Parent Window number. Foreign keys should found there")
        t.index('treecode', [['programcode'], ['treeCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('windownumber', [['programcode'], ['windownumber']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('progwin')
