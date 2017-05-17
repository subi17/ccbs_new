from gearbox.migrations import Migration

class ChgTableFFItem(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('FFItem')
        t.alter_indexdata('FFNum', buffer_pool="Alternate")

    def down(self):
        self.alter_table('FFItem')
        t.alter_indexdata(FFNum)
