from gearbox.migrations import Migration

class AddTableReportConf(Migration):

    database = "common"

    def up(self):
        t = self.table('ReportConf', area="Sta_Data_256", label="Report Configuration", dump_name="reportconf", desc="Report configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of Brand")
        t.column('ReportID', 'character', format="x(12)", initial="", max_width=24, label="Report ID", column_label="ID", position=3, order=20, help="Report ID")
        t.column('ReportName', 'character', format="x(30)", initial="", max_width=60, label="Report Name", column_label="Name", position=4, order=30, help="Report name")
        t.index('ReportID', [['Brand'], ['ReportID']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('ReportConf')
