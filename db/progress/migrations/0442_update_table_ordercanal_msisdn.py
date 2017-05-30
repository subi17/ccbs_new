from gearbox.migrations import Migration

class ChgTableMSISDN(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('MSISDN')
        t.alter_indexdata('POS', buffer_pool="Alternate")

    def down(self):
        self.alter_table('MSISDN')
        t.alter_indexdata(POS)
