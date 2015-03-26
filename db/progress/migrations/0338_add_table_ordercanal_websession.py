from gearbox.migrations import Migration

class AddWebSession(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('WebSession', area='Sta_Data_256',
                       label='WebSession',
                       dump_name='websessi',
                       desc='Currently logged in users')
        t.column('sessionid', 'character', case_sensitive=True, format='x(32)', initial='',
                 help='Unique identifier')
        t.column('usercode', 'character', format='X(20)', initial='',
                 help='Identifies the user (foreign key to login in access tables)')
        t.column('sessiontype', 'character', initial='',
                 help='Type of the session (e.g. selfcare, portal)')
        t.column('createdate', 'date', format='99.99.99',
                 help='Creation date (see also createtime)')
        t.column('createtime', 'integer', format='>>,>>>', initial='0',
                 help='Creation time (see also createdate)')
        t.column('lastdate', 'date', format='99.99.99',
                 help='Last day user requested a page (see also lasttime)')
        t.column('lasttime', 'integer', format='>>,>>>', initial='0',
                 help='Last time user requested a page (see also lastdate)')
        t.column('data', 'character', format='X(100)', initial='',
                 help='Container for session data that must be saved on server side')
        t.index('LastChange', ['lastdate', 'lasttime'], area='Sta_Index_2')
        t.index('LastDate', ['lastdate'], area='Sta_Index_2')
        t.index('SessionId', ['sessionid'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('UserCode', ['usercode', 'sessiontype'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('WebSession')

