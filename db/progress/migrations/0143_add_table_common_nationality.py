from gearbox.migrations import Migration

class AddNationality(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Nationality', area='Sta_Data_128',
                       label='Nationalities',
                       dump_name='national',
                       desc='Nationalities\
\
')
        t.column('Nationality', 'character', initial='',
                 column_label='Nation.')
        t.column('NtName', 'character', format='x(30)', initial='',
                 label='Description')
        t.index('Nationality', ['Nationality'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('NtName', ['NtName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Nationality')

