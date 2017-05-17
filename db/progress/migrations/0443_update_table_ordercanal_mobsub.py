from gearbox.migrations import Migration

class ChgTableMobSub(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('MobSub', buffer_pool="Alternate")
        t.alter_indexdata('CLI', buffer_pool="Alternate")
        t.alter_indexdata('CustNum', buffer_pool="Alternate")
        t.alter_indexdata('MsSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MobSub')
        t.alter_indexdata(CLI)
        t.alter_indexdata(CustNum)
        t.alter_indexdata(MsSeq)
