from gearbox.migrations import Migration

class ChgTableVATCode(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('VATCode', buffer_pool="Alternate")

    def down(self):
        self.alter_table('VATCode')
