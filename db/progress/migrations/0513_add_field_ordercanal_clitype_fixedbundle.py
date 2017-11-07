from gearbox.migrations import Migration

class AddFieldFixedBundle(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('CLIType')
        t.column('FixedBundle', 'character', format="X(12)", initial="", max_width=60, label="Fixed Bundle", column_label="FixedBundle", position=36, order=340, help="Fixed Bundle for Convergent tariffs")

    def down(self):
        t = self.alter_table('CLIType')
        t.drop_column('FixedBundle')
