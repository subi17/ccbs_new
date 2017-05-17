from gearbox.migrations import Migration

class AddTableTMSQueue(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSQueue', area="Dyn_Data_256", label="Monthly Queue", dump_name="tmsqueue", desc="Monthly queue")
        t.column('Queued', 'decimal', format="->>>9.99", decimals=2, initial="0", max_width=17, label="Value", column_label="Value", position=2, order=130, help="Queued value")
        t.column('CustNum', 'integer', mandatory=True, format="zzzzzz9", initial="0", max_width=4, label=" Cust.nr", column_label="CustNo", position=3, order=10, help="Customer number, 1 ... 999999")
        t.column('Month', 'integer', format="999999", initial="0", help="Year and month (YYYYMM)", max_width=4, label="Month", column_label="Month", position=4, order=20, description="Year and month")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=140, help="Code Of Brand")
        t.index('Brand', [['Brand'], ['Month']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('TMSQueue')
