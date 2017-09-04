from gearbox.migrations import Migration

class AddTableProduct(Migration):

    database = "common"

    def up(self):
        t = self.table('Product', area="Sta_Data_256", label="Product", dump_name="product", desc="Product")
        t.column('Product', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="ProdId", column_label="ProdId", position=2, order=10, help="Product Id")
        t.column('ProdName', 'character', format="x(40)", initial="", max_width=80, label="ProductName", column_label="ProductName", position=3, order=20, help="Product Name")
        t.index('Product', [['Product']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ProdName', [['ProdName'], ['Product']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Product')
