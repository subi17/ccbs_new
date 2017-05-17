from gearbox.migrations import Migration

class AddTablePIndicator(Migration):

    database = "common"

    def up(self):
        t = self.table('PIndicator', area="Sta_Data_32", label="PIndicator", dump_name="PIndicator", desc=" Performance Indicator")
        t.column('Brand', 'character', format="x(10)", initial="", max_width=20, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brands")
        t.column('HostTable', 'character', format="x(16)", initial="", max_width=32, label="HostTable", column_label="HostTable", position=3, order=20, help="HostTable")
        t.column('KeyValue', 'character', format="x(20)", initial="", max_width=40, label="KeyValue", column_label="KeyValue", position=4, order=30, help="KeyValue")
        t.column('IndicatorType', 'integer', format="99", initial="?", max_width=4, label="Indicator Type", column_label="Type", position=5, order=40, help="Indicator type")
        t.column('IndicatorValue', 'character', format="x(20)", initial="", max_width=40, label="Indicator Value", column_label="Value", position=6, order=50, help="Indicator Value")
        t.column('TimeStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", position=7, order=60)
        t.column('Memo', 'character', format="x(20)", initial="", max_width=40, label="Indicator Memo", column_label="Indicator Memo", position=8, order=70, help="Indicator Memo")
        t.index('HostTable', [['Brand'], ['HostTable'], ['KeyValue'], ['IndicatorType'], ['TimeStamp', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('PIndicator')
