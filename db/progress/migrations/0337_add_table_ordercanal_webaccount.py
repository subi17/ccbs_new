from gearbox.migrations import Migration

class AddWebAccount(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('WebAccount', area='Sta_Data_256',
                       dump_name='webaccou',
                       desc='Web-accounts for salesman and support staff')
        t.column('login', 'character', format='X(12)', initial='',
                 help='Login name')
        t.column('Salesman', 'character', format='X(12)',
                 help='The salesman that can use this account, if any')
        t.column('password', 'character', format='X(20)', initial='',
                 help='Password')
        t.column('groups', 'character', format='x(20)', initial='',
                 help='Comma separated list of access groups')
        t.index('login', ['login'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('WebAccount')

