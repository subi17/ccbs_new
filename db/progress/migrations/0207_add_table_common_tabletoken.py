from gearbox.migrations import Migration

class AddTableToken(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TableToken', area='Sta_Data_256',
                       dump_name='TableTok')
        t.column('tablename', 'character', format='X(15)', initial='',
                 label='Table name',
                 help='Database table name')
        t.column('tokencode', 'character', format='X(50)', initial='',
                 label='Tokens',
                 help='Comma separed list of Token codes')
        t.index('tablename', ['tablename'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TableToken')

