from gearbox.migrations import Migration

class AddTableInvRowCounter(Migration):

    database = "counter"

    def up(self):
        t = self.table('InvRowCounter', area="Sta_Data_64", label="Invoice Row Counter", dump_name="invrowcounter", desc="Invoice row counter")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Number", column_label="Invoice", position=2, order=10, help="Invoice number (internal)")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=3, order=20, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('InvSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="EDR Sequence", column_label="EDR Seq.", position=4, order=30, help="EDR sequence")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", position=5, order=40, help="MSISDN")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="Bill.Item", position=6, order=50, help="Billing item ID")
        t.column('CCN', 'integer', mandatory=True, format=">>>>9", initial="0", max_width=4, label="Report CCN", column_label="CCN", position=7, order=60, help="Reporting CCN")
        t.column('TariffNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Tariff ID", column_label="Tariff", position=8, order=70, help="Tariff ID")
        t.column('InvCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Inv.Cust", position=9, order=80, help="Invoice customer")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=10, order=90, help="Subscription ID")
        t.column('ReportingID', 'character', format="x(20)", initial="", max_width=40, label="Reporting ID", column_label="Report", position=11, order=100, help="Reporting ID (list)")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Events From", column_label="From", position=12, order=110, help="Beginning of event period")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Events To", column_label="To", position=13, order=120, help="Events to date")
        t.column('VATIncl', 'logical', format="Incl/Excl", initial="no", max_width=1, label="Tax Included", column_label="Tax", position=14, order=130, help="Is tax Included/Excluded in amounts")
        t.column('DCEvent', 'character', format="x(16)", initial="", max_width=32, label="Service Package ID", column_label="Service", position=15, order=140, help="Service package ID")
        t.column('Quantity', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="Quantity", column_label="Qty", position=16, order=150, help="Quantity")
        t.column('Duration', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Duration", position=17, order=160, help="Duration")
        t.column('Amount', 'decimal', format="->>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Amount", column_label="Amt", position=18, order=170, help="Amount")
        t.column('DataAmt', 'decimal', format="->>>>>>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Data Amount", column_label="Data", position=19, order=180, help="Data amount")
        t.column('RefPrice', 'decimal', format="->>>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Reference Price", column_label="Ref.Price", position=20, order=190, help="Reference price amount")
        t.column('ExtraAmount', 'decimal', format="->>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Extra Amount", column_label="Extra", position=21, order=200, help="Extra amount")
        t.column('RealQty', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="Real Quantity", column_label="Real Qty", position=22, order=210, help="Real quantity of EDRs")
        t.index('InvCust', [['InvCust'], ['InvSeq'], ['BillCode'], ['CCN'], ['ToDate']], area="Dyn_Index_1", primary=True)
        t.index('InvNum', [['InvNum'], ['SubInvNum']], area="Dyn_Index_1")
        t.index('MsSeq', [['MsSeq'], ['ToDate', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('InvRowCounter')
