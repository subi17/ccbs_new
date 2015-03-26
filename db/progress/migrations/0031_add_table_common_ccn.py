from gearbox.migrations import Migration

class AddCCN(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CCN', area='Sta_Data_128',
                       label='CCN',
                       dump_name='ccn',
                       desc='CCN')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call Case Number (CCN)')
        t.column('CCNName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of CCN')
        t.column('RepCCN', 'integer', mandatory=True, format='zz9', initial='0',
                 label='Reporting CCN',
                 column_label='RepCCN')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CCN', ['Brand', 'CCN'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CCNName', ['Brand', 'CCNName'], area='Sta_Index_2')
        t.index('RepCCN', ['Brand', 'RepCCN', 'CCN'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CCN')

