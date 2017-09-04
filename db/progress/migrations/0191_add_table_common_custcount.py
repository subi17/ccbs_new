from gearbox.migrations import Migration

class AddTableCustCount(Migration):

    database = "common"

    def up(self):
        t = self.table('CustCount', area="Sta_Data_2_256", multitenant="yes", label="Customer counters", dump_name="custcount", desc="Customer counters")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('Unbilled', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Unbilled", position=3, order=20, help="Amount of customer's unbilled events")
        t.index('CustNum', [['CustNum']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('CustCount')
