from gearbox.migrations import Migration

class AddTableMenuClass(Migration):

    database = "common"

    def up(self):
        t = self.table('MenuClass', area="Sta_Data_128", label="Menu Classes", dump_name="menuclas", desc="Menu classes")
        t.column('MenuClass', 'integer', format="zzz9", initial="0", max_width=4, label="No ", column_label="No.", position=2, order=10, help="Unique number for Program Class")
        t.column('MCName', 'character', format="x(40)", initial="", max_width=80, label="Name of Class", column_label="Name of Class", position=3, order=20, help="Name of Program Class")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=4, order=30, help="Memo text")
        t.index('MenuClass', [['MenuClass']], area="Sta_Index_2", primary=True, unique=True)
        t.index('MCName', [['MCName'], ['MenuClass']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MenuClass')
