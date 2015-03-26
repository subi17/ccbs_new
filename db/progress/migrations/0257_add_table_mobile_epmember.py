from gearbox.migrations import Migration

class AddEPMember(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('EPMember', area='Sta_Data_256',
                       label='Member of external product group',
                       dump_name='epmemb')
        t.column('EpGroup', 'character', format='x(12)', initial='',
                 label='EPcode',
                 column_label='EPcode',
                 help='Unique code of product group')
        t.column('BillCode', 'character', initial='',
                 label='ProdNo',
                 column_label='ProdNo',
                 help='Product code')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('BillCode', ['Brand', 'BillCode', 'EpGroup'], area='Sta_Index_3',
                unique=True)
        t.index('EpGroup', ['Brand', 'EpGroup', 'BillCode'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('EPMember')

