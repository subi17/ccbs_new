from gearbox.migrations import Migration

class AddFieldBundleType(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('DayCampaign')
        t.column('BundleType', 'integer', format=">9", initial="0", label="Bundle Type", column_label="Bundle Type", position=35, max_width=4, order=400, help="Bundle Type")

    def down(self):
        t = self.alter_table('DayCampaign')
        t.drop_column('BundleType')

