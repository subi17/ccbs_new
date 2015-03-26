from gearbox.migrations import Migration

class AddBnet(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('Bnet', area='Sta_Data_256',
                       dump_name='bnet')
        t.column('BnetCode', 'character', format='x(5)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Mobile Operator/Service Provider')
        t.column('BnetValue', 'character', initial='',
                 label='Type',
                 column_label='Type',
                 help='Type of BNET')
        t.column('BnetName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of the BNET')
        t.column('Memo', 'character', extent=15, format='x(70)', initial='',
                 column_label='Memo')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('BnetCode', ['Brand', 'BnetCode', 'BnetValue'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('BnetName', ['Brand', 'BnetName', 'BnetValue'], area='Sta_Index_3')
        t.index('BnetValue', ['Brand', 'BnetValue', 'BnetCode'], area='Sta_Index_3')

    def down(self):
        self.drop_table('Bnet')

