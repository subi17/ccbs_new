from gearbox.migrations import Migration

class AddTableTMSPass(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSPass', area="Sta_Data_32", label="TMS Passwords", dump_name="tmspass", desc="TMS Passwords")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", help="User ID, 1 - 8 characters", max_width=16, label="User ID", column_label="User ID", position=2, order=10, description="Usercode")
        t.column('Password', 'character', mandatory=True, format="x(16)", initial="", help="Password, 8 - 16 characters", max_width=16, label="Password", column_label="Password", position=3, order=20, description="Password")
        t.column('CreateTS', 'decimal', mandatory=True, format="99999999.99999", decimals=5, initial="0", help="Password create timestamp", max_width=20, label="CreateTS", column_label="CreateTS", position=4, order=30, description="Timestamp of password creation")
        t.column('Creator', 'character', mandatory=True, format="x(8)", initial="", help="User ID, 1 - 8 characters", max_width=16, label="Created by", column_label="Created by", position=5, order=40, description="Creator of the password")
        t.index('UserCode', [['UserCode'], ['CreateTS', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TMSPass')
