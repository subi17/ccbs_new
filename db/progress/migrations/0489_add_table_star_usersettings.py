from gearbox.migrations import Migration

class AddTableusersettings(Migration):

    database = "star"

    def up(self):
        t = self.table('usersettings', area="Sta_Data_256", label="User settings", dump_name="userset")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=2, order=10, help="User code")
        t.column('settingtype', 'character', format="X(12)", initial="", max_width=24, label="Type", position=3, order=20, help="Setting type")
        t.column('settingkey', 'character', format="X(20)", initial="", max_width=40, label="Key", position=4, order=30, help="Key of the setting")
        t.column('settingvalue', 'character', format="X(256)", initial="", max_width=512, label="Value", position=5, order=40, help="Setting value")
        t.index('setting', [['usercode'], ['settingtype'], ['settingkey']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('usersettings')
