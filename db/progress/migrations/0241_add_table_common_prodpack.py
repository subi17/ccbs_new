from gearbox.migrations import Migration

class AddTableProdPack(Migration):

    database = "common"

    def up(self):
        t = self.table('ProdPack', area="Sta_Data_256", label="Product Package", dump_name="prodpack", desc="Product package")
        t.column('ProdPack', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="PpId", column_label="PpId", position=2, order=10, help="Product Package ID")
        t.column('PPName', 'character', format="x(40)", initial="", max_width=80, label="PpName", column_label="PpName", position=3, order=20, help="Product Package Name")
        t.column('FeeModel', 'character', format="x(8)", initial="", max_width=16, label="BEvent", column_label="BEvent", position=4, order=30, help="Code of Billing Event")
        t.index('ProdPack', [['ProdPack']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PpName', [['PPName'], ['ProdPack']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ProdPack')
