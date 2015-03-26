from gearbox.migrations import Migration

class AddRequestStatus(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestStatus', area='Sta_Data_64',
                       label='Status configuration',
                       dump_name='requeststatus',
                       desc='Status configuration for requests')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('ReqType', 'integer', format='>>9', initial='0',
                 label='Request Type',
                 column_label='Type',
                 description='Request type')
        t.column('ReqStatus', 'integer', format='>>9', initial='0',
                 label='Request Status',
                 column_label='Status',
                 help='Request status')
        t.column('Program', 'character', format='x(30)', initial='',
                 help='Program that contains logic for this status')
        t.column('InUse', 'logical', initial='yes',
                 label='Used',
                 help='Request status is handled')
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
        t.column('LogClear', 'logical', initial='no',
                 label='Clear Log',
                 help='Clear log on each round')
        t.index('ReqStatus', ['Brand', 'ReqType', 'ReqStatus'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RequestStatus')

