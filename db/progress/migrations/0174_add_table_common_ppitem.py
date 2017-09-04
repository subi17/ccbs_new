from gearbox.migrations import Migration

class AddTablePPItem(Migration):

    database = "common"

    def up(self):
        t = self.table('PPItem', area="Sta_Data_256", label="Product Package Component", dump_name="ppitem", desc="Product package component")
        t.column('ProdPack', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="PpId", column_label="PpId", position=2, order=10, help="Product Package ID")
        t.column('Product', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="ProdId", column_label="ProdId", position=3, order=20, help="Product Id")
        t.index('ProdPack', [['ProdPack'], ['Product']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BillCode', [['Product'], ['ProdPack']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('PPItem')
