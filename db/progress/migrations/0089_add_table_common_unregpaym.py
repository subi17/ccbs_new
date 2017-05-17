from gearbox.migrations import Migration

class AddTableUnregPaym(Migration):

    database = "common"

    def up(self):
        t = self.table('UnregPaym', area="Sta_Data_32", label="Unregistered Payments", dump_name="unregpay", desc="Unregistered payments")
        t.column('AccDate', 'date', format="99.99.9999", initial=self.unknown, max_width=4, label="Book day", column_label="Book day", position=2, order=10, help="Bookkeeping day")
        t.column('PaymDate', 'date', format="99.99.9999", initial=self.unknown, max_width=4, label="Payment day", column_label="Payment day", position=3, order=20, help="Date of invoice payment")
        t.column('RefNum', 'character', format="x(20)", initial="", max_width=40, label="Ref.Num.", column_label="Ref.Num.", position=4, order=30, help="Reference Number")
        t.column('InvNum', 'character', format="x(8)", initial="0", max_width=16, label="Inv.Num.", column_label="Inv.Num.", position=5, order=40, help="Invoice's number")
        t.column('ArchiveId', 'character', format="x(16)", initial="", max_width=32, label="Arch.ID", column_label="Arch.ID", position=6, order=50, help="Archive ID")
        t.column('PaidAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Payment", column_label="Payment", position=7, order=60, help="Payment")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Acc.Num.", column_label="Acc.Num.", position=8, order=70, help="Account number")
        t.column('Interest', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Interest", column_label="Interest", position=9, order=80, help="Interest")
        t.column('CustName', 'character', format="x(12)", initial="", max_width=24, label="Cust.name", column_label="Cust.name", position=10, order=90, help="Customer name")
        t.column('BankAcc', 'character', format="x(20)", initial="", max_width=40, label="Bank Acc.", column_label="Bank Acc.", position=11, order=100, help="Bank account number")
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="Memo", position=12, order=110, help="Info")
        t.column('PaymSrc', 'character', format="x(8)", initial="", help="Source of payment", max_width=16, label="Payment source", column_label="Source", position=13, order=120, description="empty=manual, dd=direct debit, rf=reference file, ca=collection agency")
        t.column('Booked', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Booked sum", column_label="Booked sum", position=14, order=130, help="Booked sum")
        t.column('State', 'integer', format="9", initial="0", max_width=4, label="State", column_label="State", position=15, order=140, help="0 - not processed, 1 - deleted")
        t.column('UrSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=16, order=150, help="Sequence")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=17, order=160, help="Code Of Brand")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=18, order=170, help="Sequential nbr of the subinvoice within the combined invoice")
        t.index('archiveID', [['Brand'], ['ArchiveId']], area="Sta_Index_2", primary=True)
        t.index('bankacc', [['Brand'], ['State'], ['BankAcc'], ['PaymDate', 'DESC']], area="Sta_Index_2")
        t.index('custname', [['Brand'], ['State'], ['CustName']], area="Sta_Index_2")
        t.index('PaidAmt', [['Brand'], ['State'], ['PaidAmt']], area="Sta_Index_2")
        t.index('PaymDate', [['Brand'], ['State'], ['PaymDate']], area="Sta_Index_2")
        t.index('RefNum', [['Brand'], ['State'], ['RefNum']], area="Sta_Index_2")
        t.index('Urseq', [['UrSeq']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('UnregPaym')
