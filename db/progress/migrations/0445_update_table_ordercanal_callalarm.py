from gearbox.migrations import Migration

class ChgTableCallAlarm(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('CallAlarm')
        t.alter_indexdata('Delistat', buffer_pool="Alternate")

    def down(self):
        self.alter_table('CallAlarm')
        t.alter_indexdata(Delistat)
