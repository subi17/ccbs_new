from gearbox.migrations import Migration

class AddAuthUser(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AuthUser', area='Sta_Data_64',
                       label='AuthUser',
                       dump_name='authuser',
                       desc='3rd party user authentication')
        t.column('Username', 'character', format='x(20)', initial='',
                 column_label='Username',
                 description='the login name')
        t.column('PWHash', 'raw', format='x(40)', initial='',
                 column_label='PWHash',
                 description='the hash (SHA-1) of of the user\'s password')
        t.column('PWSalt', 'raw', format='x(40)', initial='',
                 column_label='the hashing salt')
        t.index('Username', ['Username'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('AuthUser')

