from gearbox.migrations import Migration

class ChgTableMobCDR(Migration):

    database = "mcdr"

    def up(self):
        t = self.alter_table('MobCDR')
        t.alter_indexdata('CLI', buffer_pool="Alternate")
        t.alter_indexdata('invseq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MobCDR')
        t.alter_indexdata(CLI)
        t.alter_indexdata(invseq)
