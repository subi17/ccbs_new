from gearbox.migrations import Migration

class ChgTablePListConf(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('PListConf', buffer_pool="Alternate")

    def down(self):
        self.alter_table('PListConf')
