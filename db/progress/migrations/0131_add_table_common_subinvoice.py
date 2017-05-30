from gearbox.migrations import Migration

class AddTableSubInvoice(Migration):

    database = "common"

    def up(self):
        t = self.table('SubInvoice', area="Dyn_Data_32", label="Sub Invoice", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-subinvoice.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-subinvoice.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="subinvoice", desc="Subinvoice within a combined invoice")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Invoice Number", column_label="Invoice", position=3, order=20, help="Invoice number")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=4, order=30, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="User Customer", column_label="Cust.", position=5, order=40, help="User customer number")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="MSISDN", position=6, order=50)
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subs.ID", position=7, order=60, help="Subscription ID")
        t.column('AmtExclVAT', 'decimal', format="->>>>>>>9.99", decimals=3, initial="0", max_width=18, label="Total Excl.Tax", column_label="Excl.Tax", position=8, order=70, help="Billed total value excluding tax")
        t.column('VATAmt', 'decimal', format="->>>>>>>9.99", decimals=3, initial="0", max_width=18, label="VAT", column_label="Tax", position=9, order=80, help="Amount of Tax")
        t.column('InterestAmt', 'decimal', format="ZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Interest", column_label="Interest", position=10, order=90, help="Billed interest, from earlier payments")
        t.column('OverPaym', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Overpayment", column_label="OverPaym", position=12, order=110)
        t.column('Rounding', 'decimal', format="->9.99", decimals=3, initial="0", max_width=18, label="Rounding", position=13, order=120)
        t.column('InvAmt', 'decimal', format="->>>>>>>9.99", decimals=3, initial="0", max_width=18, label="Invoice Amount", column_label="Amount", position=14, order=130, help="Invoice amount")
        t.column('AdvPaym', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Advance Payment", column_label="Adv.Payment", position=15, order=100, help="Value of advance payment used in this invoice")
        t.column('VATPercent', 'decimal', format=">9.99", decimals=2, initial="0", max_width=120, label="Tax%", extent=10, position=16, order=150, help="Tax Percentage (%)")
        t.column('VATAmount', 'decimal', format="->>>,>>9.99", decimals=3, initial="0", max_width=240, label="Tax Amount", column_label="Tax", extent=10, position=17, order=160, help="Amount of tax")
        t.column('PaidAmt', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Paid Amount", column_label="Paid", position=19, order=180, help="Paid amount")
        t.column('ClaimState', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Claiming Status", column_label="Claim", position=22, order=190, help="Claiming status")
        t.column('InvSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Invoice Sequence", column_label="InvSeq", position=23, order=200, help="Invoice sequence of cdrs")
        t.column('CrInvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Credit Invoice", column_label="Cr.Inv", position=24, order=210, help="Credit invoice number")
        t.column('PaymState', 'integer', format="9", initial="0", max_width=4, label="Payment Status", column_label="Paym.Stat", position=28, order=170, help="Payment status")
        t.column('VATBasis', 'decimal', format="->>,>>9.99", decimals=3, initial="0", max_width=220, label="Tax Basis", extent=10, position=29, order=140, help="Base sum for tax")
        t.column('VATAccount', 'integer', format=">>>>>>>9", initial="0", max_width=180, label="VAT Account", column_label="VATAcct", extent=10, position=30, order=220, help="Account number of VAT")
        t.column('FixedNumber', 'character', format="x(11)", initial=self.unknown, max_width=22, label="FixedNumber", position=31, order=230)
        t.index('InvNum', [['InvNum'], ['SubInvNum']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CLI', [['Brand'], ['CLI']], area="Dyn_Index_1")
        t.index('CustNum', [['CustNum']], area="Dyn_Index_1")
        t.index('MsSeq', [['MsSeq']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('SubInvoice')
