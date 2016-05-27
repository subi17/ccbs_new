from gearbox.migrations import Migration

class AddTableUnregLog(Migration):

    database = "common"

    def up(self):
        t = self.table('UnregLog', area="Sta_Data_128", label="Unreg. payment log", dump_name="unreglog", desc="Log for unregistered payment handling")
        t.column('UrSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=2, order=10, help="Sequence")
        t.column('AccDate', 'date', format="99.99.9999", initial=self.unknown, max_width=4, label="Book day", column_label="Book day", position=3, order=20, help="Bookkeeping day")
        t.column('Voucher', 'integer', format="ZZZZZZ9", initial="0", max_width=4, label="VoucherNo", column_label="VoucherNo", position=4, order=30, help="Consecutive recipt number of payment")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=5, order=40, help="Customer's number")
        t.column('Amount', 'decimal', format="z,zzz,zz9.99-", decimals=2, initial="0", max_width=17, label="Amount", position=7, order=60, help="Amount of payment")
        t.column('CustBal', 'character', format="x(8)", initial="", max_width=16, label="CustBal", column_label="CustBal", position=8, order=50, help="Type of registeration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=9, order=70, help="Code Of Brand")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Number", column_label="Invoice", position=10, order=90, help="Invoice number")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=11, order=100, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, position=12, order=80)
        t.index('Urseq', [['UrSeq']], area="Sta_Index_2", primary=True)
        t.index('AccDate', [['AccDate']], area="Sta_Index_2")
        t.index('CustNum', [['CustNum']], area="Sta_Index_2")
        t.index('Voucher', [['Voucher']], area="Sta_Index_2")

    def down(self):
        self.drop_table('UnregLog')
