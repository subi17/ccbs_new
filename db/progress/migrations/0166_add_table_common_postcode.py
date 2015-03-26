from gearbox.migrations import Migration

class AddPostCode(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PostCode', area='Sta_Data_128',
                       label='Zip Codes',
                       dump_name='xPostCod',
                       desc='Zip codes')
        t.column('ZipCode', 'character', format='x(5)', initial='',
                 column_label='ZipCode',
                 help='Postal code')
        t.column('PostOffice', 'character', format='x(40)', initial='',
                 label='Postal Office',
                 column_label='PostOffice',
                 help='Postal office')
        t.column('Country', 'character', format='x(3)', initial='',
                 column_label='CCode',
                 help='Country Code (according to ISO Standard)')
        t.column('Region', 'character', initial='',
                 column_label='Region',
                 help='Region code')
        t.index('PostOffice', ['Country', 'PostOffice'], area='Sta_Index_2')
        t.index('Region', ['Country', 'Region', 'ZipCode'], area='Sta_Index_2',
                primary=True)
        t.index('ZipCode', ['Country', 'ZipCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PostCode')

