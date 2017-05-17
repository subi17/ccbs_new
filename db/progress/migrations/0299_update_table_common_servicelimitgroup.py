from gearbox.migrations import Migration

class ChgTableServiceLimitGroup(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('ServiceLimitGroup', buffer_pool="Alternate")

    def down(self):
        self.alter_table('ServiceLimitGroup')
