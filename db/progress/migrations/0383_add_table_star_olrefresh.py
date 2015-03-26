from gearbox.migrations import Migration

class AddOLRefresh(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('OLRefresh', area='Sta_Data_256',
                       label='OLRefresh',
                       dump_name='olrefres',
                       desc='Log for updated files')
        t.column('State', 'logical', initial='Yes',
                 label='St.',
                 column_label='St.',
                 help='Are there unread updates in database ?')

    def down(self):
        self.drop_table('OLRefresh')

