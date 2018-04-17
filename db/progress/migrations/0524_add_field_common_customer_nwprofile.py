from gearbox.migrations import Migration

class AddFieldNWProfile(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Customer')
        t.column('NWProfile', 'integer', format=">9", initial="0", label="Network profile", column_label="NWProfile", position=126, max_width=4, order=1600, help="Network profile")

    def down(self):
        t = self.alter_table('Customer')
        t.drop_column('NWProfile')

