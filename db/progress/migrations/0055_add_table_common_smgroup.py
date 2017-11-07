from gearbox.migrations import Migration

class AddTableSMGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('SMGroup', area="Sta_Data_256", label="Salesman Groups", dump_name="smgroup", desc="Salesman Groups")
        t.column('SmGroup', 'character', format="x(10)", initial="", max_width=20, label="Salesman Group", column_label="Salesman Group", position=2, order=10, help="Salesman Group Code")
        t.column('SGName', 'character', format="x(40)", initial="", max_width=80, label="Group Name", column_label="Group Name", position=3, order=20, help="Name of Salesman Group")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=4, order=30, help="Memo text")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('SmGroup', [['Brand'], ['SmGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('SGName', [['Brand'], ['SmGroup']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('SMGroup')
