from gearbox.migrations import Migration

class AddUserSman(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UserSman', area='Sta_Data_256',
                       label='UserSman',
                       dump_name='usersman',
                       desc='User\'s salesman definitions\
')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID, 1 - 8 characters')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('Memo', 'character', format='x(60)', initial='',
                 help='Information')
        t.index('Salesman', ['Salesman'], area='Sta_Index_2')
        t.index('UserCode', ['UserCode', 'Brand'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('UserSman')

