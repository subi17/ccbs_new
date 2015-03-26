from gearbox.migrations import Migration

class AddNatHoliday(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('NatHoliday', area='Sta_Data_128',
                       label='National holidays',
                       dump_name='natholid',
                       desc='National holidays (weekend prices used)')
        t.column('Holiday', 'date', format='99-99-9999',
                 label='Date',
                 column_label='Date',
                 help='Holiday\'s date')
        t.column('HName', 'character', format='x(30)', initial='',
                 label='Explan',
                 column_label='Explan',
                 help='Holiday\'s name')
        t.index('HName', ['HName', 'Holiday'], area='Sta_Index_2')
        t.index('Holiday', ['Holiday'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('NatHoliday')

