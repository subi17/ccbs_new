from gearbox.migrations import Migration

class ChgTableCustomer(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Customer', buffer_pool="Alternate")
        t.alter_indexdata('AgrCust', buffer_pool="Alternate")
        t.alter_indexdata('CustNum_s', buffer_pool="Alternate")
        t.alter_indexdata('InvCust', buffer_pool="Alternate")

    def down(self):
        self.alter_table('Customer')
        t.alter_indexdata(AgrCust)
        t.alter_indexdata(CustNum_s)
        t.alter_indexdata(InvCust)
