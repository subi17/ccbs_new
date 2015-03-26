from gearbox.migrations import Migration

class AddConvLang(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ConvLang', area='Sta_Data_256',
                       label='Conversion Language',
                       dump_name='convlang',
                       desc='Conversion languages')
        t.column('CcLang', 'character', initial='',
                 label='Conv. language',
                 column_label='Conv.',
                 help='Conversion language')
        t.column('CcLName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Conversion language name')
        t.index('CcLang', ['CcLang'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CcLName', ['CcLName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('ConvLang')

