from gearbox.migrations import Migration

class AddLanguage(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Language', area='Sta_Data_256',
                       label='Language',
                       dump_name='language',
                       desc='Language')
        t.column('Language', 'integer', format='>9', initial='0',
                 column_label='Language',
                 help='Code of Language')
        t.column('LangName', 'character', format='x(35)', initial='',
                 column_label='LangName',
                 help='Name of Language')
        t.index('LangName', ['LangName', 'Language'], area='Sta_Index_2',
                unique=True)
        t.index('Language', ['Language'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Language')

