from gearbox.migrations import Migration

class AddMenuText(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MenuText', area='Sta_Data_128',
                       label='Menu Texts',
                       dump_name='menutext',
                       desc='Menu texts')
        t.column('MenuNum', 'integer', mandatory=True, format='ZZZ9', initial='0',
                 label='MenuNo',
                 column_label='MenuNo',
                 help='Number of menu')
        t.column('MenuText', 'character', format='x(16)', initial='',
                 label='MenuTxt',
                 column_label='MenuTxt',
                 help='Menu\'s text')
        t.index('MenuNum', ['MenuNum'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MenuText', ['MenuText'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MenuText')

