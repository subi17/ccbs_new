from gearbox.migrations import Migration

class ChgTableRepText(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('RepText', buffer_pool="Alternate")

    def down(self):
        self.alter_table('RepText')
