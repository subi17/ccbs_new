from gearbox.migrations import Migration

class AddTableUserLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('UserLimit', area="Sta_Data_64", label="UserLimit", dump_name="userlimit", desc="User or usergroup limits")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brands")
        t.column('LimitType', 'integer', format=">9", initial="0", max_width=4, label="LimitType", column_label="LimitType", position=3, order=20, help="Limit type")
        t.column('LimitTarget', 'character', format="x(12)", initial="", max_width=24, label="LimitTarget", column_label="LimitTarget", position=4, order=30, help="Limit target type")
        t.column('LimitTargetID', 'character', format="x(12)", initial="", max_width=24, label="LimitTargetID", column_label="LimitTargetID", position=5, order=40, help="Limit target ID")
        t.column('LimitAmt', 'decimal', format=">>>>>>>>>9.99", decimals=2, initial="0", max_width=17, label="LimitAmt", column_label="LimitAmt", position=6, order=50, help="Limit amount")
        t.index('Limit', [['Brand'], ['LimitType'], ['LimitTarget'], ['LimitTargetID']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('UserLimit')
