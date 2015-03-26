from gearbox.migrations import Migration

class AddProduct(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Product', area='Sta_Data_256',
                       label='Product',
                       dump_name='product',
                       desc='Product')
        t.column('Product', 'character', mandatory=True, initial='',
                 label='ProdId',
                 column_label='ProdId',
                 help='Product Id')
        t.column('ProdName', 'character', format='x(40)', initial='',
                 label='ProductName',
                 column_label='ProductName',
                 help='Product Name')
        t.index('ProdName', ['ProdName', 'Product'], area='Sta_Index_2')
        t.index('Product', ['Product'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Product')

