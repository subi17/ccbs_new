from gearbox.migrations import Migration

class AddTableFFItem(Migration):

    database = "common"

    def up(self):
        t = self.table('FFItem', area="Sta_Data_64", label="Contract's Billing Periods", dump_name="ffitem", desc="Monthly item (one billable transaction) of a contract fee")
        t.column('FFNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Contract", column_label="Contract", position=2, order=10, help="Consecutive number (sequence) of contract")
        t.column('FFItemNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="ItemNo", column_label="ItemNo", position=3, order=20, help="""Individual """"invisible"""" sequence for this item""")
        t.column('BillPeriod', 'integer', format="999999", initial="0", max_width=4, label="Period", column_label="Period", position=4, order=30, help="Period YYYYMM (month when a service shall be BILLED)")
        t.column('Amt', 'decimal', format="->>>,>>9.99", decimals=2, initial="0", max_width=17, label="Amount", column_label="Amount", position=5, order=40, help="Payable Contract Payment per bill")
        t.column('OwnCost', 'decimal', format="->>>,>>9.99", decimals=2, initial="0", help="Our own costs per bill", max_width=17, label="Our costs", column_label="Our costs", position=6, order=50, description="Our own costs per bill")
        t.column('Billed', 'logical', format="yes/no", initial="no", max_width=1, label="Billed", column_label="Billed", position=7, order=60, help="Is this item billed (y/n)")
        t.column('InvNum', 'integer', format="zzzzzzzzz", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=8, order=70, help="Number of an invoice where this item was billed")
        t.column('BillCode', 'character', format="x(6)", initial="", max_width=12, label="ProdCode", column_label="ProdCode", position=9, order=80, help="Product code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Explanation", column_label="Explanation", extent=5, position=10, order=90, help="Individual Explanation Text for the invoice")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=11, order=100, help="Customer number")
        t.column('Concerns', 'integer', format="999999", initial="0", max_width=28, label="Concerns", column_label="Concerns", extent=2, position=12, order=110, help="Period that this fee concerns")
        t.column('InvRowId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="RowId", column_label="RowId", position=14, order=130, help="Invoice row identification number")
        t.column('VATCode', 'integer', format="z9", initial="1", max_width=4, label="VAT code", column_label="VAT code", position=15, order=140, help="VAT code 1 ... 10")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="A-Subscriber", column_label="A-Sub", position=16, order=150, help="A-Subscriber number")
        t.column('VatIncl', 'logical', format="Included/Excluded", initial="Yes", max_width=1, label="VAT Included", column_label="VAT", position=17, order=160, help="Is VAT included or excluded in prices")
        t.column('AccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Account", column_label="AccNum", position=18, order=170, help="Account number")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=19, order=180, help="Sequential nbr of the subinvoice within the combined invoice")
        t.index('FFNum', [['FFNum'], ['BillPeriod'], ['FFItemNum']], area="Sta_Index_1", primary=True)
        t.index('CustNum', [['CustNum'], ['BillPeriod'], ['Billed']], area="Sta_Index_1")
        t.index('FFItemNum', [['FFItemNum', 'DESC']], area="Sta_Index_1", unique=True)
        t.index('InvNum', [['InvNum']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FFItem')
