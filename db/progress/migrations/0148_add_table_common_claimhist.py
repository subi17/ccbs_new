from gearbox.migrations import Migration

class AddTableClaimHist(Migration):

    database = "common"

    def up(self):
        t = self.table('ClaimHist', area="Sta_Data_128", label="Claiming history", dump_name="claimhis", desc="Claiming history of invoice")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=2, order=10, help="Invoice Number")
        t.column('ClaimDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Claiming Date", column_label="Date", position=3, order=20, help="Claiming date")
        t.column('Claim', 'integer', format=">>9", initial="0", help="Claim number", max_width=4, label="Claim nbr", column_label="Claim", position=4, order=30, description="How many times have been claimed")
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="Info", position=5, order=40, help="Info")
        t.column('Handler', 'character', format="x(20)", initial="", max_width=40, label="Handler", position=6, order=50, help="User id of the claim event handler")
        t.column('ClaimAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Claimed Amount", column_label="Amount", position=7, order=60, help="Claimed amount (invoice's open balance)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=9, order=80, help="Customer's number")
        t.column('ClaimState', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Claiming Status", column_label="Claimed", position=10, order=90, help="Claiming status")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=11, order=100, help="Sequential nbr of the subinvoice within the combined invoice")
        t.index('InvNum_s', [['InvNum'], ['Claim']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ClaimDate', [['Brand'], ['ClaimDate', 'DESC'], ['InvNum']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['ClaimDate', 'DESC']], area="Sta_Index_2")
        t.index('InvNum', [['Brand'], ['InvNum'], ['Claim']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ClaimHist')
