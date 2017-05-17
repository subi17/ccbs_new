from gearbox.migrations import Migration

class ChgTableServiceLimitTarget(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('ServiceLimitTarget', buffer_pool="Alternate")

    def down(self):
        self.alter_table('ServiceLimitTarget')
