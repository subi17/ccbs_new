from gearbox.migrations import Migration

class ChgTableProgLimit(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('ProgLimit', buffer_pool="Alternate")

    def down(self):
        self.alter_table('ProgLimit')
