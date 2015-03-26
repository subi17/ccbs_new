from gearbox.migrations import Migration

class AddCountry(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Country', area='Sta_Data_256',
                       label='Countries',
                       dump_name='country',
                       desc='Country codes (ISO) and names\
')
        t.column('Country', 'character', format='x(3)', initial='',
                 column_label='Country',
                 help='Country Code (according to ISO Standard)')
        t.column('CoName', 'character', format='x(30)', initial='',
                 label='Name of Country',
                 column_label='Name of Country')
        t.index('CoName', ['CoName'], area='Sta_Index_2')
        t.index('Country', ['Country'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Country')

