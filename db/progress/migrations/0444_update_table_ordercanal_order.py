from gearbox.migrations import Migration

class ChgTableOrder(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('Order')
        t.alter_indexdata('OrderID', buffer_pool="Alternate")

    def down(self):
        self.alter_table('Order')
        t.alter_indexdata(OrderID)
