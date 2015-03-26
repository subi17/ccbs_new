from gearbox.migrations import Migration

class AddToken(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Token', area='Sta_Data_128')
        t.column('tokencode', 'character', format='X(12)', initial='',
                 label='Token',
                 help='Token code')
        t.column('tokenname', 'character', format='X(30)', initial='',
                 label='Name',
                 help='Name of token')
        t.column('AdminToken', 'logical', initial='no',
                 label='Admin Token',
                 column_label='Admin',
                 help='Admin level token')
        t.index('token', ['tokencode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Token')

