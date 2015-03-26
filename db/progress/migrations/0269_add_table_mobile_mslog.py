from gearbox.migrations import Migration

class AddMSLog(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSLog', area='Sta_Data_128',
                       label='MS Log',
                       dump_name='mslog',
                       desc='Log for mobile subscription events')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='MobSub Sequence',
                 column_label='SubSeq',
                 help='Sequence for a subscription')
        t.column('LogType', 'character', initial='',
                 label='Log Type',
                 column_label='Type',
                 help='Type of log event')
        t.column('LogStatus', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='Stat',
                 help='Status of event')
        t.column('EventValue', 'character', format='x(30)', initial='',
                 label='Event Value',
                 column_label='Value',
                 help='Value of event')
        t.column('UserCode', 'character', initial='',
                 label='User ID',
                 column_label='User',
                 help='TMS user who processed the event')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('EventStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Time Stamp Of Event',
                 column_label='Time',
                 help='Time stamp of event')
        t.index('EventStamp', ['Brand', ('EventStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('LogType', ['Brand', 'LogType', ('EventStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('MsSeq', ['MsSeq', 'LogType', ('EventStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('MSLog')

