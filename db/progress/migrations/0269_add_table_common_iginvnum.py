from gearbox.migrations import Migration

class AddTableIGInvNum(Migration):

    database = "common"

    def up(self):
        t = self.table('IGInvNum', area="Sta_Data_2_256", label="InvGroup Invoice Nbr", dump_name="iginvnum", desc='''Invoice group invoice number sequences
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('InvGroup', 'character', format="x(8)", initial="", help="Invoice group", max_width=16, label="InvGroup", position=3, order=20, description='''

''')
        t.column('InvType', 'integer', format=">9", initial="0", help="Invoice type", max_width=4, label="Invoice Type", column_label="Type", position=4, order=30, description='''

''')
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", help="Invoice number sequence (last used number)", max_width=4, label="Invoice Number", column_label="Inv.Nbr", position=5, order=40, description='''

''')
        t.column('SeqPrefix', 'character', format="x(8)", initial="", max_width=16, label="Sequence Prefix", column_label="Prefix", position=6, order=50, help="Sequence prefix")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=7, order=60, help="Date when sequence becomes effective")
        t.index('InvGroup', [['Brand'], ['InvGroup'], ['InvType'], ['FromDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('IGInvNum')
