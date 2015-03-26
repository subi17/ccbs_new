from gearbox.migrations import Migration

class AddCronLog(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CronLog', area='Sta_Data_256',
                       label='CronLog',
                       dump_name='cronlog',
                       desc='Log file for cron based processes\
')
        t.column('Date', 'date', format='99-99-99',
                 help='Date when the run started')
        t.column('LogTime', 'integer', format='>>>>9', initial='0',
                 label='Time',
                 column_label='Time',
                 help='Time of day when run started')
        t.column('DateFrom', 'date', format='99-99-99',
                 label='From',
                 column_label='From',
                 help='From which date erasing started')
        t.column('DateTo', 'date', format='99-99-99',
                 label='To',
                 column_label='To',
                 help='To what date erasing was run')
        t.column('Amt', 'integer', format='>,>>>,>>9', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Amount of erased calls')
        t.column('State', 'character', format='x(12)', initial='',
                 column_label='State',
                 help='Status of the run: RUNNING - ENDED')
        t.index('Date', [('Date', 'DESCENDING'), ('LogTime', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DateFrom', ['DateFrom', 'DateTo'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CronLog')

