from gearbox.migrations import Migration

class AddTableDepartment(Migration):

    database = "common"

    def up(self):
        t = self.table('Department', area="Sta_Data_256", label="Department", dump_name="departme", desc='''One level of cost accounting


''')
        t.column('Department', 'character', format="x(8)", initial="", max_width=16, label="Department", column_label="Department", position=2, order=10, help="Department")
        t.column('DpName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Name of the department")
        t.column('Division', 'character', format="x(8)", initial="", max_width=16, label="Division", column_label="Division", position=4, order=30, help="Division of the department")
        t.index('Department', [['Department']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DpName', [['DpName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Department')
