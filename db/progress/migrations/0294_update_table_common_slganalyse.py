from gearbox.migrations import Migration

class ChgTableSLGAnalyse(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('SLGAnalyse', buffer_pool="Alternate")

    def down(self):
        self.alter_table('SLGAnalyse')
