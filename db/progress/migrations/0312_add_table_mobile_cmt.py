from gearbox.migrations import Migration

class AddTableCMT(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CMT', area="Sta_Data_256", dump_name="puhlaji")
        t.column('CMT', 'character', mandatory=True, format="x(2)", initial="", max_width=4, label="Number", column_label="Number", position=2, order=10, help="Number")
        t.column('CMTName', 'character', format="x(30)", initial="", max_width=60, label="Description", column_label="Description", position=3, order=20, help="Description")
        t.index('CMT', [['CMT', 'ABBREVIATED']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('CMT')
