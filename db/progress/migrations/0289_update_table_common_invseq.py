from gearbox.migrations import Migration

class ChgTableInvSeq(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('InvSeq', buffer_pool="Alternate")
        t.alter_indexdata('InvSeq', buffer_pool="Alternate")
        t.alter_indexdata('MsSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('InvSeq')
        t.alter_indexdata(InvSeq)
        t.alter_indexdata(MsSeq)
