from gearbox.migrations import Migration

class ChgTableSaldoCounter(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('SaldoCounter')
        t.alter_indexdata('MSSeq', buffer_pool="Alternate")

    def down(self):
        self.alter_table('SaldoCounter')
        t.alter_indexdata(MSSeq)
