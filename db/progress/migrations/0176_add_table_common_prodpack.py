from gearbox.migrations import Migration

class AddProdPack(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ProdPack', area='Sta_Data_256',
                       label='Product Package',
                       dump_name='prodpack',
                       desc='Product package')
        t.column('ProdPack', 'character', mandatory=True, initial='',
                 label='PpId',
                 column_label='PpId',
                 help='Product Package ID')
        t.column('PPName', 'character', format='x(40)', initial='',
                 label='PpName',
                 column_label='PpName',
                 help='Product Package Name')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='Code of Billing Event')
        t.index('PpName', ['PPName', 'ProdPack'], area='Sta_Index_2')
        t.index('ProdPack', ['ProdPack'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ProdPack')

