from gearbox.migrations import Migration

class AddUserAccess(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('UserAccess', area='Sta_Data_256',
                       dump_name='UserAcce',
                       desc='Which customer has what role for a subscription')
        t.column('CustNum', 'integer', initial='0',
                 label='Customer')
        t.column('Role', 'character', format='x(6)', initial='',
                 help='The access type of the customer on the subscription')
        t.column('CLI', 'character', format='x(20)', initial='',
                 help='The subscription, to which the user has access')
        t.column('SubType', 'character', format='X(9)', initial='',
                 label='Type',
                 help='Type of the subscription, e.g. mobile or broadband')
        t.index('customer', ['CustNum', 'CLI'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('UserAccess')

