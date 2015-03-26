from gearbox.migrations import Migration

class AddUserAccount(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('UserAccount', area='Sta_Data_256',
                       dump_name='useracco',
                       desc='Enduser accounts for logging-in on selfcare pages')
        t.column('login', 'character', format='X(12)', initial='',
                 label='Login',
                 help='Login name')
        t.column('CustNum', 'integer', initial='0',
                 label='Customer',
                 help='The customer that can use this account')
        t.column('password', 'character', format='X(20)', initial='',
                 label='Passw',
                 help='Password')
        t.column('active', 'integer', initial='0',
                 label='status',
                 help='Active status')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2',
                unique=True)
        t.index('login', ['login'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('UserAccount')

