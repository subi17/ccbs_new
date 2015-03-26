from gearbox.migrations import Migration

class Addeventlog(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('eventlog', area='Sta_Data_64')
        t.column('eventdate', 'date', format='99.99.99',
                 label='Date',
                 help='Event date')
        t.column('eventtime', 'character', initial='',
                 label='Time',
                 help='Event time')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('tablename', 'character', format='X(15)', initial='',
                 label='Table',
                 help='Source table name')
        t.column('key', 'character', format='X(30)', initial='',
                 label='Key',
                 help='Key to source record')
        t.column('action', 'character', initial='',
                 label='Action',
                 help='Action type.. Create,Modity,Delete')
        t.column('datavalues', 'character', format='X(50)', initial='',
                 label='Values',
                 help='Changed values of record')
        t.column('modifiedfields', 'character', format='X(50)', initial='',
                 label='Modified fields',
                 help='Fieldnames from BUFFER-COMPARE')
        t.column('fieldformats', 'character', format='X(50)', initial='',
                 label='Field formats',
                 help='Field formats of modified fields')
        t.column('EventLogStatus', 'integer', format='9', initial='0',
                 column_label='EventLogStatus',
                 help='Status of eventlog')
        t.column('TimingTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='TimingTS',
                 help='Timing Timestamp')
        t.column('TimingDate', 'date', format='99-99-99',
                 column_label='TimingDate',
                 help='Timing Date (schedule date)')
        t.column('TimingTime', 'decimal', decimals=2, format='99.99', initial='0',
                 column_label='TimingTime',
                 help='Timing Time (Scheluded time)')
        t.index('EventDate', [('eventdate', 'DESCENDING'), ('eventtime', 'DESCENDING'), 'tablename', 'key'], area='Sta_Index_1')
        t.index('EventLogStatus', ['EventLogStatus', ('TimingTS', 'DESCENDING')], area='Sta_Index_1')
        t.index('TableName', ['tablename', 'key', ('eventdate', 'DESCENDING'), ('eventtime', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('UserName', ['usercode', ('eventdate', 'DESCENDING'), ('eventtime', 'DESCENDING')], area='Sta_Index_1')
        t.index('UserTable', ['usercode', 'tablename', 'key', ('eventdate', 'DESCENDING'), ('eventtime', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('eventlog')

