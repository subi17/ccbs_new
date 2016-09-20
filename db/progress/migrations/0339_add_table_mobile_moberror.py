from gearbox.migrations import Migration

class AddTableMobError(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MobError', area="Sta_Data_128_2", label="Error codes for Mobile Rating", dump_name="mrerr", desc='''
''')
        t.column('MobError', 'integer', format="zzz9", initial="0", max_width=4, label="Error Code", column_label="Error Code", position=2, order=10, help="Error Code 1 ... 9999")
        t.column('MEName', 'character', format="x(60)", initial="", max_width=120, label="Explanation", column_label="Explanation", position=3, order=20, help="Explanation (name) of an Error Code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo of Mobile Rating Error Code")
        t.index('MobError', [['MobError']], area="Sta_Index_3", primary=True, unique=True)
        t.index('MEName', [['MEName'], ['MobError']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('MobError')
