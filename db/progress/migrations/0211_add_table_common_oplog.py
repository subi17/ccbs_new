from gearbox.migrations import Migration

class AddTableOPLog(Migration):

    database = "common"

    def up(self):
        t = self.table('OPLog', area="Sta_Data_128", label="Overpayment Transaction", dump_name="oplog", desc="Overpayment Transaction Log")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=2, order=10, help="Customer's number")
        t.column('EventDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=3, order=20, help="Date of transaction/event")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User", column_label="User", position=4, order=30, help="Id of the TMS User")
        t.column('EventType', 'integer', format="9", initial="0", max_width=4, label="Type", column_label="Type", position=5, order=40, help="Transaction Type Code")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=6, order=50, help="Number of Associated Invoice")
        t.column('Voucher', 'integer', format="zzzzzz9", initial="0", max_width=4, label="VoucherNo", column_label="VoucherNo", position=7, order=60, help="Voucher Number of Associated Payment (if any)")
        t.column('Amt', 'decimal', format="-z,zzz,zz9.99", decimals=2, initial="0", max_width=17, label="Sum", column_label="Sum", position=8, order=70, help="Sum (amount of money)")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TStamp", column_label="TStamp", position=9, order=80, help="TimeStamp; when created")
        t.column('Info', 'character', format="x(30)", initial="", max_width=60, label="Info", position=10, order=90, help="Info")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=11, order=100, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="MSISDN", position=12, order=110)
        t.index('CustNum', [['CustNum'], ['CreStamp', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('CLI', [['CLI']], area="Sta_Index_2")
        t.index('CustDate', [['CustNum'], ['EventDate', 'DESC'], ['Voucher']], area="Sta_Index_2")
        t.index('CustInv', [['CustNum'], ['InvNum'], ['EventDate', 'DESC']], area="Sta_Index_2")
        t.index('InvNum', [['InvNum'], ['SubInvNum'], ['EventDate', 'DESC']], area="Sta_Index_2")
        t.index('Voucher', [['Voucher']], area="Sta_Index_2")

    def down(self):
        self.drop_table('OPLog')
