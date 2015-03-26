from gearbox.migrations import Migration

class AddServiceLimitGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ServiceLimitGroup', area='Sta_Data_128',
                       dump_name='servlg',
                       desc='\
\
\
')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('GroupCode', 'character', format='x(16)', initial='',
                 column_label='Group Code',
                 help='Group Code of Servicelimit')
        t.column('GroupName', 'character', format='x(30)', initial='',
                 label='Group Name',
                 column_label='Group Name',
                 help='Name of Servicelimit Group')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='Valid From',
                 help='The date FROM which this servicelimit group will be used.')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='Valid To',
                 help='The date TO (until) which this Servicelimit group will be used.')
        t.index('GroupCode', ['Brand', 'GroupCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('GroupName', ['Brand', 'GroupName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('ServiceLimitGroup')

