from gearbox.migrations import Migration

class AddPPItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PPItem', area='Sta_Data_256',
                       label='Product Package Component',
                       dump_name='ppitem',
                       desc='Product package component')
        t.column('ProdPack', 'character', mandatory=True, initial='',
                 label='PpId',
                 column_label='PpId',
                 help='Product Package ID')
        t.column('Product', 'character', mandatory=True, initial='',
                 label='ProdId',
                 column_label='ProdId',
                 help='Product Id')
        t.index('BillCode', ['Product', 'ProdPack'], area='Sta_Index_2',
                unique=True)
        t.index('ProdPack', ['ProdPack', 'Product'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PPItem')

