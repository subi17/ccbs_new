from gearbox.migrations import Migration

class AddTableServiceLimitGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('ServiceLimitGroup', area="Sta_Data_128", dump_name="servlg", desc='''


''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=5, help="Code Of Brand")
        t.column('GroupName', 'character', format="x(30)", initial="", max_width=60, label="Group Name", column_label="Group Name", position=3, order=20, help="Name of Servicelimit Group")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="Valid From", position=4, order=30, help="The date FROM which this servicelimit group will be used.")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="Valid To", position=5, order=40, help="The date TO (until) which this Servicelimit group will be used.")
        t.column('GroupCode', 'character', format="x(16)", initial="", max_width=32, label="GroupCode", column_label="Group Code", position=6, order=10, help="Group Code of Servicelimit")
        t.index('GroupCode', [['Brand'], ['GroupCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('GroupName', [['Brand'], ['GroupName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ServiceLimitGroup')
