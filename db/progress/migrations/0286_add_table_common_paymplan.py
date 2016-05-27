from gearbox.migrations import Migration

class AddTablePaymPlan(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymPlan', area="Sta_Data_128", label="Payment Plan", dump_name="paymplan", desc="Payment plan for unpaid invoices")
        t.column('PPlanID', 'integer', format=">>>>>>>9", initial="0", help="Payment plan ID", max_width=4, label="Payment Plan ID", column_label="PP ID", position=2, order=10, description="sequence pplan")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=3, order=20, help="Customer's number")
        t.column('PPDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Creation Date", column_label="Date", position=4, order=30, help="Date when payment plan was done")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", position=5, order=40, help="Total debt of invoices that belong to payment plan")
        t.column('BankDays', 'integer', format=">>9", initial="0", max_width=4, label="Bank Days", column_label="BankDays", position=6, order=50, help="Bank days, that will be waited after due date for payment")
        t.column('PPStatus', 'integer', format="9", initial="0", help="Status of payment plan", max_width=4, label="Status", position=7, order=60, description="e.g. sent, accepted, cancelled, paid")
        t.column('PPType', 'integer', format="9", initial="0", help="Type of payment plan", max_width=4, label="Type", position=8, order=70, description="e.g. rem(inder), cc (cust.care)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=9, order=80, help="Code Of Brand")
        t.column('Orderer', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Orderer", position=10, order=90, help="Customer who ordered the plan (confirmation letter receiver)")
        t.column('MsRequest', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Request ID", column_label="ID", position=11, order=100, help="Unique ID for request")
        t.index('PPlanID', [['PPlanID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['PPDate', 'DESC']], area="Sta_Index_2")
        t.index('PPDate', [['Brand'], ['PPDate', 'DESC'], ['CustNum']], area="Sta_Index_2")
        t.index('PPStatus', [['Brand'], ['PPStatus'], ['CustNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PaymPlan')
