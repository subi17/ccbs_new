from gearbox.migrations import Migration

class AddTableMobPref(Migration):

    database = "common"

    def up(self):
        t = self.table('MobPref', area="Sta_Data_256", label="Mobile Prefixes", dump_name="mobpref", desc="Mobile prefixes")
        t.column('Prefix', 'character', format="x(8)", initial="", max_width=16, label="Prefix", column_label="Prefix", position=2, order=20, help="A prefix that identifies mobile numbers")
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="memo", column_label="memo", position=3, order=30, help="Memo")
        t.column('Operator', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="OpCode", column_label="OpCode", position=4, order=450, help="Operators code")
        t.index('Prefix', [['Prefix']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('MobPref')
