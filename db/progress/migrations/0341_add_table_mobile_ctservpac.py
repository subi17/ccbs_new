from gearbox.migrations import Migration

class AddTableCTServPac(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CTServPac', area="Sta_Data_128", label="CLIType Service Packages", dump_name="ctservpa", desc="Service Packages of a CLI type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('CLIType', 'character', format="x(8)", initial="", max_width=16, label="CLI Type", column_label="CLIType", position=3, order=20, help="CLI type")
        t.column('ServPac', 'character', format="x(12)", initial="", max_width=24, label="Service Package", column_label="ServPack", position=4, order=30, help="Service package code")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From Date", column_label="From", position=5, order=40, help="Valid from date")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To Date", column_label="To", position=6, order=50, help="Valid to date")
        t.column('ServiceLimit', 'character', format="x(16)", initial="", max_width=32, label="Service Limit", column_label="SLimit", position=7, order=60, help="Service limit group")
        t.column('ServType', 'integer', format="9", initial="0", max_width=4, label="Package Type", column_label="Type", position=8, order=70, help="Package type")
        t.index('CLIType', [['Brand'], ['CLIType'], ['ServPac'], ['FromDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)
        t.index('ServPac', [['Brand'], ['ServPac'], ['CLIType'], ['FromDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('CTServPac')
