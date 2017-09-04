from gearbox.migrations import Migration

class AddTableuserkeys(Migration):

    database = "star"

    def up(self):
        t = self.table('userkeys', area="Sta_Data_256", dump_name="userkeys")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=2, order=10, help="User code")
        t.column('keytype', 'character', format="X(20)", initial="", max_width=40, label="Type", position=3, order=20, help="Key type")
        t.column('key', 'character', format="X(30)", initial="", max_width=60, label="Key", position=4, order=30, help="Key value")
        t.index('usercode', [['usercode'], ['keytype'], ['key']], area="Sta_Index_1", primary=True, unique=True)
        t.index('keytype', [['keytype'], ['key'], ['usercode']], area="Sta_Index_1", unique=True)

    def down(self):
        self.drop_table('userkeys')
