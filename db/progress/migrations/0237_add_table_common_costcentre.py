from gearbox.migrations import Migration

class AddTableCostCentre(Migration):

    database = "common"

    def up(self):
        t = self.table('CostCentre', area="Sta_Data_256", label="Cost Centre", dump_name="costcent", desc='''Lowest level of cost accounting
''')
        t.column('CostCentre', 'character', format="x(8)", initial="", max_width=16, label="Cost Centre", column_label="Cc", position=2, order=10, help="Cost Centre")
        t.column('CCName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Name of the cost centre")
        t.column('Department', 'character', format="x(8)", initial="", max_width=16, label="Department", column_label="Dep.", position=4, order=30, help="Department of the cost centre")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('CostCentre', [['Brand'], ['CostCentre']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CCN', [['Brand'], ['CCName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CostCentre')
