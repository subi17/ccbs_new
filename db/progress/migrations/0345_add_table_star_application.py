from gearbox.migrations import Migration

class Addapplication(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('application', area='Sta_Data_256',
                       label='Application',
                       dump_name='appllica',
                       desc='Applications on the system')
        t.column('applcode', 'character', mandatory=True, initial='',
                 label='Application',
                 help='Application code. Internal use only')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='Application name')
        t.index('application', ['applcode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('application')

