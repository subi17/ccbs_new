from gearbox.migrations import Migration

class AddstarLanguage(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('starLanguage', area='Sta_Data_256',
                       dump_name='starlang')
        t.column('translategroup', 'character', format='X(15)', initial='',
                 label='Group',
                 help='Program,Menu,Label,Button,Table')
        t.column('key1', 'character', format='X(50)', initial='',
                 label='Key 1')
        t.column('key2', 'character', format='X(50)', initial='',
                 label='Key 2')
        t.column('key3', 'character', format='X(50)', initial='',
                 label='Key 3')
        t.column('language', 'character', initial='',
                 label='Language',
                 help='eng,fin,swe')
        t.column('name', 'character', format='X(30)', initial='',
                 label='Name',
                 help='Name in current language')
        t.column('helptxt', 'character', format='X(50)', initial='',
                 label='Help',
                 help='Help text in current language')
        t.column('fieldformat', 'character', format='X(20)', initial='',
                 label='Format',
                 help='Field format')
        t.index('language', ['language', 'translategroup', 'key1', 'key2', 'key3'], area='Sta_Index_2',
                unique=True)
        t.index('main', ['translategroup', 'key1', 'key2', 'key3', 'language'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('starLanguage')

