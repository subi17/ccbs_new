from gearbox.migrations import Migration

class AddFieldOperators(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('MNPRetPlatform')
        t.column('Operators', 'character', format="X(30)", initial="", max_width=60, label="Operators", column_label="Operators", position=9, order=80, help="Operators")

    def down(self):
        t = self.alter_table('MNPRetPlatform')
        t.drop_column('Operators')
