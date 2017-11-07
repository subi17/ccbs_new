from gearbox.migrations import Migration

class AddTableUserRight(Migration):

    database = "common"

    def up(self):
        t = self.table('UserRight', area="Sta_Data_256", label="User Rights", dump_name="userrigh", desc="User rights (user id & Program Class)")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User Id", column_label="User Id", position=2, order=10, help="User Id")
        t.column('MenuClass', 'integer', format="zzz9", initial="0", max_width=4, label="Class", column_label="Class", position=3, order=20, help="Unique number for Program Class")
        t.column('UsrRight', 'integer', valexp="input UsrRight < 3", format="9", initial="0", max_width=4, label="Right", column_label="Right", position=4, order=30, valmsg="Value MUST be 0, 1 or 2 !", help="0: Prohibited  1: read only  2: read/write")
        t.index('katun', [['UserCode'], ['MenuClass']], area="Sta_Index_2", primary=True, unique=True)
        t.index('MenuClass', [['MenuClass'], ['UserCode']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('UserRight')
