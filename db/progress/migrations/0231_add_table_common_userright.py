from gearbox.migrations import Migration

class AddUserRight(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UserRight', area='Sta_Data_256',
                       label='User Rights',
                       dump_name='userrigh',
                       desc='User rights (user id & Program Class)')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User Id',
                 column_label='User Id')
        t.column('MenuClass', 'integer', format='zzz9', initial='0',
                 label='Class',
                 column_label='Class',
                 help='Unique number for Program Class')
        t.column('UsrRight', 'integer', format='9', initial='0',
                 label='Right',
                 column_label='Right',
                 help='0: Prohibited  1: read only  2: read/write',
                 valexp='input UsrRight < 3',
                 valmsg='Value MUST be 0, 1 or 2 !')
        t.index('katun', ['UserCode', 'MenuClass'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MenuClass', ['MenuClass', 'UserCode'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('UserRight')

