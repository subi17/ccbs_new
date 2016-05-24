from gearbox.migrations import Migration

class AddTableppInvseq(Migration):

    database = "common"

    def up(self):
        t = self.table('ppInvseq', area="Sta_Data_128", label="Invoice Sequence", dump_name="ppInvseq", desc="Customers invoice sequences")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=2, order=10, help="Customer's number")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="DateFrom", column_label="DateFrom", position=3, order=20, help="FROM date for sequence")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="DateTo", column_label="DateTo", position=4, order=30, help="TO date for sequence")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=5, order=610, help="Calls invoice sequence")
        t.column('Billed', 'logical', format="yes/no", initial="no", max_width=1, label="Billed", column_label="Billed", position=6, order=620, help="Is this item billed (y/n)")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=7, order=630, help="Consecutive Invoice Number, 1 ... 99999999")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=8, order=640, help="Mobile subscription ID")
        t.index('CustNum', [['CustNum'], ['ToDate', 'DESC'], ['Billed', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('InvSeq', [['InvSeq']], area="Sta_Index_1")
        t.index('MsSeq', [['MsSeq'], ['CustNum'], ['Billed'], ['ToDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('ppInvseq')
