from gearbox.migrations import Migration

class AddTablePayment(Migration):

    database = "common"

    def up(self):
        t = self.table('Payment', area="Dyn_Data_32", label="Payments", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-payment.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-payment.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="payment", desc="Payments")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvoiceNo", column_label="InvoiceNo", position=2, order=30, help="Invoice's number")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="CustName", column_label="Customer name", position=3, order=20, help="Customer's name")
        t.column('InvAmt', 'decimal', format=">>>>>>9.99-", decimals=2, initial="0", max_width=17, label="Invoice Amount", column_label="Invoice Amt", position=4, order=40, help="Invoice's amount (payable)")
        t.column('PaymAmt', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Payment", column_label="Payment", position=5, order=50, help="Payment")
        t.column('TotAmt', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Total", column_label="Total", position=6, order=60, help="Total payment ")
        t.column('Discount', 'decimal', format="ZZZ9.99-", decimals=2, initial="0", max_width=17, label="Discount", column_label="Discount", position=7, order=70, help="Total discount with in this payment ")
        t.column('InvDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Invoice date", column_label="Invoice date", position=8, order=80, help="Invoice date")
        t.column('DueDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DueDay", column_label="DueDay", position=9, order=90, help="Invoice's due day")
        t.column('PaymDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Payment Date", column_label="Payment Date", position=10, order=100, help="Date of Invoice Payment")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=140, label="AcctNo", column_label="AcctNo", extent=10, position=11, order=110, help="Booking account's number")
        t.column('Posting', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=240, label="Amt", column_label="Amt", extent=10, position=12, order=120, help="Booked amount")
        t.column('Voucher', 'integer', format="ZZZZZZ9", initial="0", max_width=4, label="VoucherNo", column_label="VoucherNo", position=13, order=130, help="Voucher number of payment")
        t.column('AccDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="BookDate", column_label="BookDate", position=14, order=140, help="Date, when payment has been registered in bank")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=15, order=10, help="Customer number, 1 - 999999")
        t.column('xxMemo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=16, order=330, help="Explanation / memory field for payment")
        t.column('AccType', 'integer', format=">9", initial="0", max_width=60, label="Type", column_label="Type", extent=10, position=17, order=340, help="Account usage type number")
        t.column('PaymFile', 'character', format="x(8)", initial="", max_width=16, label="File", column_label="File", position=18, order=350, help="Name of input file")
        t.column('PaymArc', 'character', format="x(30)", initial="", max_width=60, label="ArchiveId", column_label="ArcId", position=19, order=360, help="Archive id")
        t.column('PaymSrc', 'character', format="x(8)", initial="", help="Source of payment", max_width=16, label="Payment source", column_label="Source", position=20, order=370, description="empty=manual, dd=direct debit, rf=reference file, ca=collection agency")
        t.column('ImportStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Imported", column_label="Imported", position=21, order=380, help="Date and time when payment is imported into TMS")
        t.column('ClaimStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Claimed", column_label="Claimed", position=22, order=390, help="Date and time when payment is claimed or claiming cancelled")
        t.column('PaymType', 'integer', format=">9", initial="0", max_width=4, label="Payment Type", column_label="PaymType", position=23, order=400, help="Payment type")
        t.column('ExchRate', 'decimal', format=">>9.999999", decimals=6, initial="0", max_width=21, label="Exchange Rate", column_label="Rate", position=24, order=410, help="Currency exchange rate")
        t.column('BankAcc', 'character', format="x(20)", initial="", max_width=40, label="Bank Acc.", column_label="Bank Acc.", position=25, order=420, help="Bank account number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=26, order=430, help="Code Of Brand")
        t.column('PPlanID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Payment Plan ID", column_label="PP ID", position=27, order=440, help="Payment plan ID")
        t.column('PPBatch', 'integer', format=">9", initial="0", max_width=4, label="Payment Plan Batch", column_label="PP Batch", position=28, order=450, help="Payment plan batch number")
        t.column('ExpStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Imported", column_label="Imported", position=29, order=460, help="Date and time when payment is imported into TMS")
        t.column('ExpUser', 'character', format="x(8)", initial="", max_width=16, label="Export User", column_label="User", position=30, order=470, help="User who exported payment")
        t.column('RefNum', 'character', format="x(20)", initial="", max_width=40, label="Reference Nbr", column_label="Reference", position=31, order=480, help="Reference number")
        t.column('ExtInvID', 'character', format="x(12)", initial="", help="External invoice ID", max_width=24, label="External Invoice ID", column_label="Ext.ID", position=32, order=490, description='''

''')
        t.column('ExtVoucher', 'character', format="x(12)", initial="", max_width=24, label="External Voucher", column_label="Ext.Voucher", position=33, order=500, help="External voucher")
        t.index('CustNum', [['Brand'], ['CustNum'], ['InvNum'], ['Voucher']], area="Dyn_Index_1", primary=True)
        t.index('AccDate', [['Brand'], ['AccDate', 'DESC'], ['Voucher']], area="Dyn_Index_1")
        t.index('CustNum_s', [['CustNum'], ['InvNum'], ['Voucher']], area="Dyn_Index_1")
        t.index('ExtInvID', [['Brand'], ['ExtInvID']], area="Dyn_Index_1")
        t.index('ExtVoucher', [['Brand'], ['ExtVoucher']], area="Dyn_Index_1")
        t.index('InvNum', [['Brand'], ['InvNum']], area="Dyn_Index_1")
        t.index('InvNum_s', [['InvNum']], area="Dyn_Index_1")
        t.index('PaymArc', [['Brand'], ['PaymArc']], area="Dyn_Index_1")
        t.index('PaymSrc', [['Brand'], ['PaymSrc'], ['PaymAmt']], area="Dyn_Index_1")
        t.index('PaymType', [['Brand'], ['PaymType'], ['AccDate', 'DESC']], area="Dyn_Index_1")
        t.index('PPlanID', [['PPlanID'], ['PPBatch']], area="Dyn_Index_1")
        t.index('RefNum', [['Brand'], ['RefNum']], area="Dyn_Index_1")
        t.index('Voucher', [['Brand'], ['Voucher']], area="Dyn_Index_1", unique=True)
        t.index('Voucher_s', [['Voucher']], area="Dyn_Index_1", unique=True)

    def down(self):
        self.drop_table('Payment')
