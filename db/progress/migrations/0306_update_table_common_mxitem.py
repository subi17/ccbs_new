from gearbox.migrations import Migration

class ChgTableMXItem(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('MXItem', buffer_pool="Alternate")
        t.alter_indexdata('MXSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MXItem')
        t.alter_indexdata(MXSeq)
