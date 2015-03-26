from gearbox.migrations import Migration

class Addusername(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('username', area='Sta_Data_256',
                       label='Username')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('name', 'character', mandatory=True, format='X(50)', initial='',
                 label='Name',
                 help='User realname')
        t.column('eMail', 'character', format='X(50)', initial='',
                 help='eMail address')
        t.column('lastname', 'character', format='X(25)', initial='',
                 label='Last Name',
                 help='Person lastname')
        t.column('firstname', 'character', format='X(25)', initial='',
                 label='Frist name',
                 help='Person first name')
        t.column('password', 'character', format='X(20)', initial='',
                 help='Password. It stored ecrypted to database')
        t.column('helpEditor', 'logical', initial='no',
                 label='Help Editor')
        t.column('menulanguage', 'character', initial='eng',
                 label='Menu language',
                 help='User\'s menu language')
        t.column('usergroups', 'character', format='X(50)', initial='',
                 label='UserGroups',
                 help='Comma separed list of user groups')
        t.column('durationbegin', 'date', format='99.99.99',
                 label='Begin',
                 help='Duration begin')
        t.column('durationend', 'date', format='99.99.99',
                 label='End',
                 help='Duration end')
        t.index('usercode', ['usercode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('username', ['name'], area='Sta_Index_2')

    def down(self):
        self.drop_table('username')

