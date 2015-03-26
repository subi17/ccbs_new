from gearbox.migrations import Migration

class AddM2MTime(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('M2MTime', area='Sta_Data_256',
                       dump_name='m2mtime')
        t.column('Date', 'date', format='99-99-99',
                 column_label='Date')
        t.column('TimeFrom', 'integer', format='>>>>9', initial='0',
                 column_label='TimeFrom',
                 help='Time from')
        t.column('TimeTo', 'integer', format='>>>>9', initial='0',
                 column_label='TimeTo',
                 help='Time to')
        t.index('Date', ['Date', 'TimeFrom', 'TimeTo'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('M2MTime')

