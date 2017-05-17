from gearbox.migrations import Migration

class ChgTableBillTarget(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BillTarget')
        t.alter_indexdata('CustNum', buffer_pool="Alternate")

    def down(self):
        self.alter_table('BillTarget')
        t.alter_indexdata(CustNum)
