from gearbox.migrations import Migration

class AddPLMN(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PLMN', area='Sta_Data_128',
                       label='PLMN',
                       dump_name='plmn',
                       desc='PLMN')
        t.column('PLMN', 'character', initial='',
                 column_label='PLMN',
                 help='Code of PLMN')
        t.column('CommName', 'character', format='x(16)', initial='',
                 column_label='CommName',
                 help='Commercial Name')
        t.column('CoName', 'character', format='x(30)', initial='',
                 label='Name of Country',
                 column_label='Name of Country')
        t.column('Country', 'character', format='x(3)', initial='',
                 column_label='CCode',
                 help='Country Code (according to ISO Standard)')
        t.column('PrefToNor', 'character', initial='',
                 label='PrefixToNorway',
                 column_label='PrefixToNorway',
                 help='Prefix to Norway')
        t.column('CountryPrefix', 'character', format='x(6)', initial='',
                 label='Country Prefix',
                 column_label='Prefix',
                 help='Country prefix')
        t.index('Country', ['Country'], area='Sta_Index_2')
        t.index('CountryPrefix', ['CountryPrefix'], area='Sta_Index_2')
        t.index('plmn', ['PLMN', 'Country'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PLMN')

