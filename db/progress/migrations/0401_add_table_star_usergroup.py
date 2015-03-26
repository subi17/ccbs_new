from gearbox.migrations import Migration

class Addusergroup(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('usergroup', area='Sta_Data_256',
                       dump_name='usergrrp')
        t.column('usergroup', 'character', format='X(12)', initial='',
                 label='UserGroup')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='User group name')
        t.index('usergroup', ['usergroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('usergroup')

