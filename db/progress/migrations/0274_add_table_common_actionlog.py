from gearbox.migrations import Migration

class AddTableActionLog(Migration):

    database = "common"

    def up(self):
        t = self.table('ActionLog', area="Dyn_Data_64", label="Action Log", dump_name="actionlo", desc='''Action log
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ActionID', 'character', format="x(8)", initial="", max_width=16, label="Action ID", column_label="ID", position=3, order=20, help="Action ID")
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Table Name", column_label="Table", position=4, order=30, help="Table related to the action")
        t.column('KeyValue', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Key Value", column_label="Key", position=5, order=40, help="Key value of the record related to the action")
        t.column('ActionPeriod', 'integer', format="999999", initial="0", max_width=4, label="Period", position=6, order=50, help="Period")
        t.column('ActionTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Action Time Stamp", column_label="Time Stamp", position=7, order=60, help="Action time stamp")
        t.column('ActionChar', 'character', format="x(8)", initial="", max_width=16, label="Character Value", column_label="Char", position=8, order=70, help="Character value")
        t.column('ActionDec', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=19, label="Decimal Value", column_label="Dec", position=9, order=80, help="Decimal value")
        t.column('ActionStatus', 'integer', format=">9", initial="0", max_width=4, label="Status", position=10, order=90, help="Status")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User Code", column_label="User", position=11, order=100, help="User who triggered the action")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=12, order=110, help="From date")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=13, order=120, help="To date")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=14, order=130, help="Customer's number")
        t.index('ActionID', [['Brand'], ['ActionID'], ['ActionTS', 'DESC']], area="Dyn_Index_1", primary=True)
        t.index('CustNum', [['CustNum'], ['ActionID'], ['ToDate', 'DESC']], area="Dyn_Index_1")
        t.index('TableName', [['Brand'], ['TableName'], ['KeyValue'], ['ActionID'], ['ActionPeriod', 'DESC']], area="Dyn_Index_1")
        t.index('UserCode', [['Brand'], ['UserCode'], ['ActionID'], ['ActionTS', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('ActionLog')
