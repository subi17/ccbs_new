from gearbox.migrations import Migration

class AddTableUGMember(Migration):

    database = "common"

    def up(self):
        t = self.table('UGMember', area="Sta_Data_128", label="User Group Members", dump_name="ugmember", desc="User Group Member records;  join Users&Usergroups")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=2, order=10, help="User's ID")
        t.column('UserGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=3, order=5, help="Individual Code for a User Group")
        t.column('UserName', 'character', format="x(30)", initial="", max_width=60, label="User's name", column_label="User's name", position=4, order=20, help="User's name")
        t.column('Memo', 'character', format="x(10)", initial="", max_width=20, label="Info", column_label="Info", position=5, order=30, help="Additional coded information about membership")
        t.index('UserGroup', [['UserGroup'], ['UserCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('UserCode', [['UserCode'], ['UserGroup']], area="Sta_Index_2", unique=True)
        t.index('UserName', [['UserGroup'], ['UserName'], ['UserCode']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('UGMember')
