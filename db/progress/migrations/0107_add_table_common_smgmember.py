from gearbox.migrations import Migration

class AddTableSMGMember(Migration):

    database = "common"

    def up(self):
        t = self.table('SMGMember', area="Sta_Data_256", label="Members in Salesman Groups", dump_name="smgmembe", desc="Members in Salesman Groups")
        t.column('SmGroup', 'character', mandatory=True, format="x(10)", initial="", max_width=20, label="Group", column_label="Group", position=2, order=10, help="Salesman Group Code")
        t.column('Salesman', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=3, order=20, help="Salesman's code")
        t.column('SmName', 'character', format="x(30)", initial="", max_width=60, label="Salesman", column_label="Salesman", position=4, order=30, help="Salesman's name")
        t.column('Memo', 'character', format="x(10)", initial="", max_width=20, label="Info", column_label="Info", position=5, order=40, help="Information about membership")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('SmGroup', [['Brand'], ['SmGroup'], ['Salesman']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Salesman', [['Brand'], ['Salesman'], ['SmGroup']], area="Sta_Index_2", unique=True)
        t.index('SmName', [['Brand'], ['SmGroup'], ['SmName'], ['Salesman']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('SMGMember')
