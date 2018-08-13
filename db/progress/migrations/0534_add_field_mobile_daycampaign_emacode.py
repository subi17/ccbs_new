from gearbox.migrations import Migration

class AddFieldEMAcode(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('DayCampaign')
        t.column('EMAcode', 'character', format="x(20)", initial="", label="EMA code", column_label="EMA code", position=36, max_width=40, order=410, help="EMA code")

    def down(self):
        t = self.alter_table('DayCampaign')
        t.drop_column('EMAcode')

