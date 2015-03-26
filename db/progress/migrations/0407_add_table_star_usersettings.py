from gearbox.migrations import Migration

class Addusersettings(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('usersettings', area='Sta_Data_256',
                       label='User settings',
                       dump_name='userset')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('settingtype', 'character', format='X(12)', initial='',
                 label='Type',
                 help='Setting type')
        t.column('settingkey', 'character', format='X(20)', initial='',
                 label='Key',
                 help='Key of the setting')
        t.column('settingvalue', 'character', format='X(256)', initial='',
                 label='Value',
                 help='Setting value')
        t.index('setting', ['usercode', 'settingtype', 'settingkey'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('usersettings')

