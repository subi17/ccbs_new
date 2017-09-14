from gearbox.migrations import Migration

class AddFieldMaxMinLength(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BDestTrans')
        t.column('MinLength', 'integer', format=">9", initial="0", label="Minimum Length", column_label="MinLength", position=8, max_width=4, order=80, help="MinLength")
        t.column('MaxLength', 'integer', format=">9", initial="0", label="Maximum Length", column_label="MaxLength", position=9, max_width=4, order=90, help="MaxLength")

    def down(self):
        t = self.alter_table('BDestTrans')
        t.drop_column('MinLength')
        t.drop_column('MaxLength')
