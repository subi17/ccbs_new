from gearbox.migrations import Migration

class ChgTableTariff(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Tariff', buffer_pool="Alternate")

    def down(self):
        self.alter_table('Tariff')
