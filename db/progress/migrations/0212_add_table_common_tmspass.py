from gearbox.migrations import Migration

class AddTMSPass(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSPass', area='Sta_Data_32',
                       label='TMS Passwords',
                       dump_name='tmspass',
                       desc='TMS Passwords')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID, 1 - 8 characters',
                 description='Usercode')
        t.column('Password', 'character', mandatory=True, format='x(16)', initial='',
                 column_label='Password',
                 help='Password, 8 - 16 characters',
                 description='Password')
        t.column('CreateTS', 'decimal', decimals=5, mandatory=True, format='99999999.99999', initial='0',
                 column_label='CreateTS',
                 help='Password create timestamp',
                 description='Timestamp of password creation')
        t.column('Creator', 'character', mandatory=True, initial='',
                 label='Created by',
                 column_label='Created by',
                 help='User ID, 1 - 8 characters',
                 description='Creator of the password')
        t.index('UserCode', ['UserCode', ('CreateTS', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSPass')

