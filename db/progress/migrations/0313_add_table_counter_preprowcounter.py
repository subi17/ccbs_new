from gearbox.migrations import Migration

class AddTablePrepRowCounter(Migration):

    database = "counter"

    def up(self):
        t = self.table('PrepRowCounter', area="Dyn_Data_64", label="Prepaid Row Counter", dump_name="preprowcounter", desc="Prepaid row counter")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=2, order=10, help="Subscription ID")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", position=3, order=20, help="MSISDN")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="Bill.Item", position=4, order=30, help="Billing item ID")
        t.column('InvCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Inv.Cust", position=5, order=40, help="Invoice customer")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Events From", column_label="From", position=6, order=50, help="Beginning of event period")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Events To", column_label="To", position=7, order=60, help="Events to date")
        t.column('Quantity', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="Quantity", column_label="Qty", position=8, order=70, help="Quantity")
        t.column('Amount', 'decimal', format="->>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Amount", column_label="Amt", position=9, order=80, help="Amount")
        t.index('MsSeq', [['MsSeq'], ['ToDate', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('PrepRowCounter')
