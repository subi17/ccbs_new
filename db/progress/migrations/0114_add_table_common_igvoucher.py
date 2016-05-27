from gearbox.migrations import Migration

class AddTableIGVoucher(Migration):

    database = "common"

    def up(self):
        t = self.table('IGVoucher', area="Sta_Data_2_256", label="InvGroup Invoice Nbr", dump_name="IGVouche", desc='''Invoice group invoice number sequences
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('InvGroup', 'character', format="x(8)", initial="", help="Invoice group", max_width=16, label="InvGroup", position=3, order=20, description='''

''')
        t.column('PaymType', 'integer', format=">9", initial="0", help="Payment type", max_width=4, label="Payment Type", column_label="Type", position=4, order=30, description='''

''')
        t.column('Voucher', 'integer', format=">>>>>>>9", initial="0", help="Voucher number sequence (last used number)", max_width=4, label="Voucher Number", column_label="Voucher", position=5, order=40, description='''

''')
        t.column('SeqPrefix', 'character', format="x(8)", initial="", max_width=16, label="Sequence Prefix", column_label="Prefix", position=6, order=50, help="Sequence prefix")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=7, order=60, help="Date when sequence becomes effective")
        t.index('InvGroup', [['Brand'], ['InvGroup'], ['PaymType'], ['FromDate', 'DESC']], area="Sta_Index_4", primary=True, unique=True)

    def down(self):
        self.drop_table('IGVoucher')
