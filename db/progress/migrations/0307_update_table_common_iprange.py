from gearbox.migrations import Migration

class ChgTableIPRange(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('IPRange', buffer_pool="Alternate")

    def down(self):
        self.alter_table('IPRange')
