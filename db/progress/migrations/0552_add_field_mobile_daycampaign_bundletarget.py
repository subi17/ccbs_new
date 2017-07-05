from gearbox.migrations import Migration

class AddFieldBundleTarget(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('DayCampaign')
        t.column('BundleTarget', 'integer', format="9", initial="0", max_width=4, label="Bundle Target", column_label="Bundle Target", position=34, order=390, help="Target of bundle")

    def down(self):
        t = self.alter_table('DayCampaign')
        t.drop_column('BundleTarget')
