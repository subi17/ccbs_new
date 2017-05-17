from gearbox.migrations import Migration

class AddTableTableToken(Migration):

    database = "common"

    def up(self):
        t = self.table('TableToken', area="Sta_Data_2_256", dump_name="TableTok")
        t.column('tablename', 'character', format="X(15)", initial="", max_width=30, label="Table name", position=2, order=10, help="Database table name")
        t.column('tokencode', 'character', format="X(50)", initial="", max_width=100, label="Tokens", position=3, order=20, help="Comma separed list of Token codes")
        t.index('tablename', [['tablename']], area="Sta_Index_3", primary=True)

    def down(self):
        self.drop_table('TableToken')
