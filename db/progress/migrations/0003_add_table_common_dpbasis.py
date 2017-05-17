from gearbox.migrations import Migration

class AddTableDPBasis(Migration):

    database = "common"

    def up(self):
        t = self.table('DPBasis', area="Sta_Data_256", label="Volume Discount Basis", dump_name="dpbasis", desc='''Basis of volume discount (DpConf).
''')
        t.column('DPConfNum', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Sequence", column_label="Seq", position=2, order=10, help="Unique sequence nbr for DpConf")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=5, order=40, help="Product code")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=6, order=50, help="Call Case Number for call's destination")
        t.column('BasisType', 'integer', format=">9", initial="0", help="Type of the Discount Basis", max_width=4, label="Basis Type", column_label="Type", position=10, order=60, description="0=Generic, 1=Product, 2=CCN, 3=bSub")
        t.column('KeyField', 'character', format="x(16)", initial="", max_width=32, label="KeyField", column_label="KeyField", position=12, order=100, help="Keyfield")
        t.index('DpConfNum', [['DPConfNum'], ['BasisType', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('KeyField', [['DPConfNum'], ['KeyField']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DPBasis')
