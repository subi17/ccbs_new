from gearbox.migrations import Migration

class AddStarSession(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('StarSession', area='Sta_Data_256',
                       dump_name='starsess')
        t.column('SessionId', 'integer', format='->,>>>,>>>,>>9', initial='0',
                 column_label='Id')
        t.column('clienttype', 'character', format='X(12)', initial='',
                 label='Client Type',
                 help='Client type')
        t.column('SessionConnectionId', 'character', format='X(78)', initial='',
                 label='Connection Id',
                 help='Session Connection Id')
        t.column('usercode', 'character', format='X(12)', initial='',
                 label='Usercode')
        t.column('machinetime', 'integer', format='->,>>>,>>>,>>9', initial='0',
                 label='Machinetime',
                 help='Machinetime since session begin.. ETIME function')
        t.column('CreateDate', 'date', format='99.99.99')
        t.column('CreateTime', 'character', initial='')
        t.column('LastDate', 'date', format='99.99.99')
        t.column('LastTime', 'character', initial='',
                 help='Last time')
        t.column('data', 'character', format='X(78)', initial='',
                 label='Global data')
        t.index('LastChange', ['LastDate', 'machinetime'], area='Sta_Index_2')
        t.index('SessionConnectionId', ['clienttype', 'SessionConnectionId', 'CreateDate', 'CreateTime'], area='Sta_Index_2')
        t.index('SessionId', ['SessionId'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('usercode', ['usercode', 'LastDate', 'LastTime'], area='Sta_Index_2')

    def down(self):
        self.drop_table('StarSession')

