from gearbox.migrations import Migration

class ChgTableBDest(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BDest', buffer_pool="Alternate")

    def down(self):
        self.alter_table('BDest')
