from gearbox.migrations import Migration

class AddFieldCoolTime(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('DumpHPD')
        t.column('CoolTime', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Cooling time", column_label="CoolTime", position=9, order=80, help="How close to current time to dump (seconds). Zero value means to the current time.")

    def down(self):
        t = self.alter_table('DumpHPD')
        t.drop_column('CoolTime')

