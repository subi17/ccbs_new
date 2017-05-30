from gearbox.migrations import Migration

class ChgTableMServiceLimit(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('MServiceLimit', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MServiceLimit')
