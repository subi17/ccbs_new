from gearbox.migrations import Migration

class ChgTableRoamTariff(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('RoamTariff')
        t.alter_indexdata('PriceList', buffer_pool="Alternate")

    def down(self):
        self.alter_table('RoamTariff')
        t.alter_indexdata(PriceList)
