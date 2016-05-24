from gearbox.migrations import Migration

class AddTableReportConfRow(Migration):

    database = "common"

    def up(self):
        t = self.table('ReportConfRow', area="Sta_Data_256", label="Report Configuration Row", dump_name="reportconfrow", desc="Report configuration row")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of Brand")
        t.column('ReportID', 'character', format="x(12)", initial="", max_width=24, label="Report ID", column_label="ID", position=3, order=20, help="Report ID")
        t.column('RowType', 'character', format="x(8)", initial="", max_width=16, label="Row Type", column_label="Type", position=4, order=30, help="Row type")
        t.column('CharValue', 'character', format="x(12)", initial="", max_width=24, label="Character Value", column_label="Char", position=5, order=40, help="Character value")
        t.column('DateValue', 'date', format="99-99-99", max_width=4, label="Date Value", column_label="Date", position=6, order=50, help="Date value")
        t.column('IntValue', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Value", column_label="Int", position=7, order=60, help="Integer value")
        t.column('DecValue', 'decimal', format="->>>>>>>>9.9999", decimals=6, initial="0", max_width=21, label="Decimal Value", column_label="Dec", position=8, order=70, help="Decimal value")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=9, order=80, help="Date when configuration becomes effective")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=10, order=90, help="Date when configuration expires")
        t.column('ConfRowID', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, label="Row ID", column_label="ID", position=11, order=100, help="Unique row ID")
        t.column('LogicValue', 'logical', format="Yes/No", initial="no", max_width=1, label="Logical Value", column_label="Logic", position=12, order=110, help="Logical value")
        t.index('ConfRowID', [['ConfRowID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ReportID', [['Brand'], ['ReportID'], ['RowType'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ReportConfRow')
