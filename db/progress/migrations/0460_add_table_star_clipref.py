from gearbox.migrations import Migration

class AddTableCLIPref(Migration):

    database = "star"

    def up(self):
        t = self.table('CLIPref', area="Schema Area", label="CLIPref", dump_name="clipref", desc='''Partners CLIs
''')
        t.column('Pref', 'character', format="x(4)", initial="", max_width=8, label="CLI Prefix", column_label="CLI Prefix", position=2, order=10, help="Partners prefix")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="CLI", column_label="CLI", position=3, order=20, help="CLI")
        t.column('CLIId', 'character', format="x(20)", initial="", max_width=40, label="CLI ID", column_label="CLI ID", position=4, order=30, help="CLI id")
        t.column('State', 'integer', format="9", initial="0", max_width=4, label="State", column_label="State", position=5, order=40, help="State")
        t.index('Pref', [['Pref'], ['CLI']], area="Schema Area", primary=True, unique=True)
        t.index('CLI', [['CLI'], ['Pref']], area="Schema Area", unique=True)

    def down(self):
        self.drop_table('CLIPref')
