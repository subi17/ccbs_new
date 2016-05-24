from gearbox.migrations import Migration

class AddTableCustIntEvent(Migration):

    database = "common"

    def up(self):
        t = self.table('CustIntEvent', area="Sta_Data_64", label="Customer's Interest Events", dump_name="custinte", desc="Customer's interest events")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvoiceNo", column_label="InvoiceNo", position=2, order=8, help="Consecutive Invoicenumber, 1 ... 99999999")
        t.column('InvDate', 'date', format="99-99-99", max_width=4, label="InvoiceDate", column_label="InvoiceDate", position=4, order=30, help="Invoice's date")
        t.column('DueDate', 'date', format="99-99-99", max_width=4, label="InvoiDueDate", column_label="InvoiDueDate", position=5, order=40, help="Invoice's dueday")
        t.column('PaymDate', 'date', format="99-99-99", max_width=4, label="PayDay", column_label="PayDay", position=6, order=50, help="Day when invoice was paid")
        t.column('LateDays', 'integer', format="ZZZ9", initial="0", max_width=4, label="Delay Days", column_label="Delay Days", position=7, order=60, help="How many days the payment was late")
        t.column('Amt', 'decimal', format="ZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Interest", column_label="Interest", position=8, order=70, help="Total interest, calculated due to delayed payment")
        t.column('InvAmt', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="InvoiceTot", column_label="InvTot", position=9, order=80, help="Invoice's total amount")
        t.column('PaidAmt', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Paid Amt", column_label="Paid Amt", position=10, order=90, help="Total payment")
        t.column('Percent', 'decimal', format="Z9.99", decimals=2, initial="0", max_width=17, label="Inter-%", column_label="Inter-%", position=11, order=100, help="Interest % used")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=12, order=110, help="Customer number, 1 ... 999999")
        t.column('BilledInvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Invoice#", column_label="InvoiceNo", position=13, order=120, help="Invoice number which interests have calculated ")
        t.column('Voucher', 'integer', format="ZZZZZZ9", initial="0", max_width=4, label="VoucherNo", column_label="VoucherNo", position=14, order=130, help="Consecutive voucher number of a payment")
        t.column('IntPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=120, label="%%", column_label="%%", extent=10, position=15, order=140, help="Interest %%")
        t.column('IntDays', 'integer', format="zz9", initial="0", max_width=80, label="Days", column_label="Days", extent=10, position=16, order=150, help="Number of days")
        t.column('IntAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=220, label="Interest", column_label="Interest", extent=10, position=17, order=160, help="Amount of interest with a particular %%")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=18, order=170, help="Code Of Brand")
        t.column('BilledSubInv', 'integer', format=">>9", initial="0", max_width=4, label="Billed Subinvoice Nbr", column_label="Billed SubInv", position=19, order=190, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, position=20, order=200)
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=21, order=180, help="Sequential nbr of the subinvoice within the combined invoice")
        t.index('CustNum_s', [['CustNum'], ['PaymDate']], area="Sta_Index_2", primary=True)
        t.index('BilledInvNum', [['BilledInvNum']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['PaymDate', 'DESC']], area="Sta_Index_2")
        t.index('InvNum', [['Brand'], ['InvNum'], ['PaymDate', 'DESC']], area="Sta_Index_2")
        t.index('InvNum_s', [['InvNum'], ['PaymDate']], area="Sta_Index_2")
        t.index('VoucheSta_s', [['Voucher']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustIntEvent')
