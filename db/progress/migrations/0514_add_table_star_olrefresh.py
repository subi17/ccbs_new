from gearbox.migrations import Migration

class AddTableOLRefresh(Migration):

    database = "star"

    def up(self):
        t = self.table('OLRefresh', area="Sta_Data_256", label="OLRefresh", dump_name="olrefres", desc="Log for updated files")
        t.column('State', 'logical', format="Yes/No", initial="Yes", max_width=1, label="St.", column_label="St.", position=2, order=10, help="Are there unread updates in database ?")

    def down(self):
        self.drop_table('OLRefresh')
