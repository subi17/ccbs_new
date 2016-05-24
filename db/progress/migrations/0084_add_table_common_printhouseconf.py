from gearbox.migrations import Migration

class AddTablePrintHouseConf(Migration):

    database = "common"

    def up(self):
        t = self.table('PrintHouseConf', area="Sta_Data_256", label="Printhouse Configuration", dump_name="printhouseconf", desc="Printhouse configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('TableName', 'character', format="x(16)", initial="", max_width=32, label="Table Name", column_label="Table", position=3, order=20, help="Table name")
        t.column('KeyValue', 'character', format="x(20)", initial="", max_width=40, label="Key Value", column_label="Key", position=4, order=30, help="Key value")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Effective Date", column_label="Eff.Date", position=5, order=40, help="Date when rule becomes effective")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="End Date", column_label="To", position=6, order=50, help="Date when rule expires")
        t.column('PrintHouse', 'character', format="x(12)", initial="", max_width=24, label="Printing House", column_label="PrintHouse", position=7, order=60, help="Printing house")
        t.column('Report', 'character', format="x(8)", initial="", max_width=16, label="Report", position=8, order=70, help="Report")
        t.column('FieldName', 'character', format="x(16)", initial="", max_width=32, label="Field Name", column_label="Field", position=9, order=80, help="Field name")
        t.index('Report', [['Brand'], ['Report'], ['TableName'], ['KeyValue'], ['ToDate', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('PrintHouse', [['Brand'], ['PrintHouse'], ['Report'], ['ToDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('PrintHouseConf')
