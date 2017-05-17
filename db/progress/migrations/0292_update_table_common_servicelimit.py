from gearbox.migrations import Migration

class ChgTableServiceLimit(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('ServiceLimit', buffer_pool="Alternate")

    def down(self):
        self.alter_table('ServiceLimit')
