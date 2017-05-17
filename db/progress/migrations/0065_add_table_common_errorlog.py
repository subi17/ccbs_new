from gearbox.migrations import Migration

class AddTableErrorLog(Migration):

    database = "common"

    def up(self):
        t = self.table('ErrorLog', area="Sta_Data_64", label="Error Log", dump_name="errorlog", desc="Error log")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ActionID', 'character', format="x(8)", initial="", max_width=16, label="Action ID", column_label="ID", position=3, order=20, help="Action ID")
        t.column('ActionTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Action Time Stamp", column_label="Time Stamp", position=4, order=60, help="Action time stamp")
        t.column('KeyValue', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Key Value", column_label="Key", position=5, order=40, help="Key value of the record related to the action")
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Table Name", column_label="Table", position=6, order=30, help="Table related to the action")
        t.column('ErrorCode', 'character', format="x(8)", initial="", max_width=16, label="Error Code", column_label="Error", position=7, order=70, help="Error code")
        t.column('ErrorMsg', 'character', format="x(50)", initial="", max_width=100, label="Error Message", column_label="Message", position=8, order=80, help="Error message")
        t.column('ErrorChar', 'character', format="x(8)", initial="", max_width=16, label="Character Value", column_label="Char", position=9, order=90, help="Character value")
        t.column('ErrorDec', 'decimal', format="->>>>>>9.9999", decimals=4, initial="0", max_width=19, label="Decimal Value", column_label="Dec", position=10, order=100, help="Decimal value")
        t.column('ErrorStatus', 'integer', format=">9", initial="0", max_width=4, label="Status", position=11, order=110, help="Status")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User ID", position=12, order=120, help="User ID of log event")
        t.index('ActionID', [['Brand'], ['ActionID'], ['ActionTS', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('ErrorStatus', [['Brand'], ['ErrorStatus'], ['ActionID']], area="Sta_Index_1")
        t.index('TableName', [['Brand'], ['TableName'], ['KeyValue'], ['ActionID'], ['ActionTS', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('ErrorLog')
