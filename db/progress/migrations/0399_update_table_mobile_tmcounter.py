from gearbox.migrations import Migration

class ChgTableTMCounter(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('TMCounter')
        t.alter_indexdata('MsSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('TMCounter')
        t.alter_indexdata(MsSeq)
