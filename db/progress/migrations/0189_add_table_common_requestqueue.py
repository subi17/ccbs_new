from gearbox.migrations import Migration

class AddRequestQueue(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestQueue', area='Sta_Data_64',
                       label='Request Queue',
                       dump_name='requestqueue',
                       desc='Request queue')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('Queue', 'integer', format='>>>9', initial='0',
                 label='Queue Nbr',
                 column_label='Queue',
                 help='Queue in which this type is handled')
        t.column('QName', 'character', format='x(30)', initial='',
                 label='Description',
                 column_label='Name',
                 help='Queue description')
        t.column('InUse', 'logical', initial='yes',
                 label='Used',
                 help='Request type is handled')
        t.column('Interval', 'integer', format='>>>>>9', initial='0',
                 help='Interval in seconds')
        t.column('Monitor', 'character', format='x(40)', initial='',
                 label='Monitor Command',
                 column_label='Monitor',
                 help='Command for monitoring')
        t.column('LogEntry', 'character', format='x(20)', initial='',
                 label='Log Entry',
                 help='Log entry')
        t.column('LogFile', 'character', format='x(40)', initial='',
                 label='Log File',
                 column_label='File',
                 help='Log file')
        t.column('LogThreshold', 'decimal', decimals=0, format='>>>>>>>>>9', initial='0',
                 label='Log Threshold',
                 column_label='Threshold',
                 help='Log threshold')
        t.column('LogOn', 'logical', initial='no',
                 label='Logging On',
                 column_label='Logging',
                 help='Logging on')
        t.column('MonitorOn', 'logical', initial='no',
                 label='Monitoring On',
                 help='Monitoring on')
        t.index('Queue', ['Brand', 'Queue'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RequestQueue')

