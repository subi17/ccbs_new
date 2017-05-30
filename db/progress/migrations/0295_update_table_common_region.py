from gearbox.migrations import Migration

class ChgTableRegion(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Region', buffer_pool="Alternate")

    def down(self):
        self.alter_table('Region')
