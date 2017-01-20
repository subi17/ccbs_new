from gearbox.migrations import Migration

class ChgTableMSOwner(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('MSOwner', buffer_pool="Alternate")
        t.alter_indexdata('CLI', buffer_pool="Alternate")
        t.alter_indexdata('imsi_s', buffer_pool="Alternate")
        t.alter_indexdata('InvCust', buffer_pool="Alternate")
        t.alter_indexdata('MsSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MSOwner')
        t.alter_indexdata(CLI)
        t.alter_indexdata(imsi_s)
        t.alter_indexdata(InvCust)
        t.alter_indexdata(MsSeq)
