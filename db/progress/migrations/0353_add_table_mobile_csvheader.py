from gearbox.migrations import Migration

class AddTableCSVHeader(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CSVHeader', area="Sta_Data_32", dump_name="csvheade")
        t.column('Version', 'character', format="x(6)", initial="", max_width=12, label="Version", column_label="Version", position=2, order=10)
        t.column('CSV', 'character', format="x(8)", initial="", max_width=16, label="CSV", column_label="CSV", position=3, order=20)
        t.index('Version', [['Version']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('CSVHeader')
