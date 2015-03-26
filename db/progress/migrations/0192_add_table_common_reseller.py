from gearbox.migrations import Migration

class AddReseller(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Reseller', area='Sta_Data_128',
                       label='Resellers',
                       dump_name='reseller',
                       desc='Resellers')
        t.column('Reseller', 'character', initial='',
                 label='Code',
                 column_label='Code',
                 help='"An unique code for a reseller; maximum 8 characters"')
        t.column('RsName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Reseller\'s name')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Code of that salesman who is responsible of this reseller')
        t.column('CommPerc', 'decimal', decimals=2, format='z9.9', initial='0',
                 label='Com%',
                 column_label='Com%',
                 help='Amount of Reseller\'s Commission (%)')
        t.column('Address', 'character', extent=3, format='x(30)', initial='',
                 column_label='Address',
                 help='Resellers address')
        t.column('EMail', 'character', format='x(40)', initial='',
                 label='Email',
                 column_label='Email',
                 help='Resellers Email address')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Reseller', ['Brand', 'Reseller'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('RsName', ['Brand', 'RsName', 'Reseller'], area='Sta_Index_2',
                unique=True)
        t.index('Salesman', ['Brand', 'Salesman', 'Reseller'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('Reseller')

