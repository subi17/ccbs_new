from gearbox.migrations import Migration

class AddTableFMItem(Migration):

    database = "common"

    def up(self):
        t = self.table('FMItem', area="Sta_Data_64", label="Billing Event Items", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-fmitem.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-fmitem.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="bcitem", desc="List of billable items of a single 'billing event'")
        t.column('FeeModel', 'character', format="x(8)", initial="", max_width=16, label="BEvent", column_label="BEvent", position=2, order=10, help="An unique code for a Billing Event")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Price List", column_label="Price List", position=3, order=20, help="Code (identifier) for a Price List")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=4, order=30, help="Product code")
        t.column('BillMethod', 'logical', format="Single/Fixed", initial="no", max_width=1, label="Type", column_label="Type", position=5, order=40, help="Is this a (S)ingle fee or a (F)ixed fee ?")
        t.column('Interval', 'integer', format="z9", initial="0", max_width=4, label="Interval", column_label="Interval", position=6, order=50, help="Billing Interval (1= every month, 12=every year etc)")
        t.column('Amount', 'decimal', format="-z,zz9.99", decimals=2, initial="0", max_width=17, label="Price", column_label="Price", position=7, order=60, help="Billable price of an item")
        t.column('BillType', 'character', format="x(8)", initial="", max_width=16, label="Billing Type", column_label="Billing Type", position=8, order=70, help="Type Of Billing Object")
        t.column('BillCycle', 'integer', valexp="BillCycle > 0 and BillCycle < 4", format="9", initial="1", max_width=4, label="BMeth", column_label="BMeth", position=9, order=80, valmsg="Billing Method code MUST be 1, 2 or 3", help="When this fee is to be billed 1:before 2:during 3:after")
        t.column('FFItemQty', 'integer', format=">>9", initial="0", max_width=4, label="Fixed Fee Qty", column_label="FF Qty", position=10, order=90, help="Quantity of fixed fee items")
        t.column('FFEndDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Fixed Fee End", column_label="FF End", position=11, order=100, help="End date for fixed fee")
        t.column('InclAmt', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Included Amount", column_label="Incl.Amt", position=12, order=110, help="Amount of billable material that is included in this fee")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=14, order=130, help="First effective date")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To Date", column_label="To", position=15, order=140, help="Last effective date")
        t.column('InclBillCode', 'character', format="x(16)", initial="", max_width=32, label="Incl.Billing Item", column_label="Incl.BItem", position=16, order=150, help="Billing item that included amount concerns")
        t.column('InclUnit', 'integer', format=">9", initial="0", max_width=4, label="Included Unit", column_label="Incl.Unit", position=17, order=160, help="Unit of included material")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=18, order=171, help="Code Of Brand")
        t.column('BrokenRental', 'integer', format="9", initial="0", max_width=4, label="Broken Rental", column_label="Broken Rental", position=19, order=180, help="Broken Rental")
        t.column('ServiceLimitGroup', 'character', format="x(16)", initial="", max_width=32, label="ServiceLimitGroup", column_label="ServiceLimitGroup", position=20, order=170, help="Group Code of Service Limit")
        t.column('FirstMonthBR', 'integer', format="9", initial="0", max_width=4, label="First Month Broken Rental", column_label="1.Month BR", position=21, order=190, help="Broken rental for first month")
        t.index('FeeModel', [['Brand'], ['FeeModel'], ['PriceList'], ['BillCode'], ['FromDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BillCode', [['Brand'], ['FeeModel'], ['BillCode'], ['PriceList'], ['FromDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('FMItem')
