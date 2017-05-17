from gearbox.migrations import Migration

class ChgTableInvRowCounter(Migration):

    database = "counter"

    def up(self):
        t = self.alter_table('InvRowCounter', buffer_pool="Alternate")
        t.alter_indexdata('MsSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('InvRowCounter')
        t.alter_indexdata(MsSeq)
