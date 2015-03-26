from gearbox.migrations import Migration

class AddMenuClass(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MenuClass', area='Sta_Data_128',
                       label='Menu Classes',
                       dump_name='menuclas',
                       desc='Menu classes')
        t.column('MenuClass', 'integer', format='zzz9', initial='0',
                 label='No ',
                 column_label='No.',
                 help='Unique number for Program Class')
        t.column('MCName', 'character', format='x(40)', initial='',
                 label='Name of Class',
                 column_label='Name of Class',
                 help='Name of Program Class')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.index('MCName', ['MCName', 'MenuClass'], area='Sta_Index_2')
        t.index('MenuClass', ['MenuClass'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MenuClass')

