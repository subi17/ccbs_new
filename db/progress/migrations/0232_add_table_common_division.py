from gearbox.migrations import Migration

class AddTableDivision(Migration):

    database = "common"

    def up(self):
        t = self.table('Division', area="Sta_Data_256", label="Division", dump_name="division", desc='''Uppermost level of cost accounting

''')
        t.column('Division', 'character', format="x(8)", initial="", max_width=16, label="Division", column_label="Division", position=2, order=10, help="Division")
        t.column('DvName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Name of the division")
        t.index('Division', [['Division']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DvName', [['DvName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Division')
