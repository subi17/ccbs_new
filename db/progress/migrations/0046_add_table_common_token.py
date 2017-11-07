from gearbox.migrations import Migration

class AddTableToken(Migration):

    database = "common"

    def up(self):
        t = self.table('Token', area="Sta_Data_128", dump_name="Token")
        t.column('tokencode', 'character', format="X(12)", initial="", max_width=24, label="Token", position=2, order=10, help="Token code")
        t.column('tokenname', 'character', format="X(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Name of token")
        t.column('AdminToken', 'logical', format="Yes/No", initial="no", max_width=1, label="Admin Token", column_label="Admin", position=4, order=30, help="Admin level token")
        t.index('token', [['tokencode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('Token')
