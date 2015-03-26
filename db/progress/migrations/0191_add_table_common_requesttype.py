from gearbox.migrations import Migration

class AddRequestType(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestType', area='Sta_Data_64',
                       label='Request configuration',
                       dump_name='requesttype',
                       desc='Request type configuration')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('ReqType', 'integer', format='>>9', initial='0',
                 label='Request Type',
                 column_label='Type',
                 description='Request type')
        t.column('ReqName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Description of request type')
        t.column('Program', 'character', format='x(30)', initial='',
                 help='Program that contains logic for this type')
        t.column('UserCode', 'character', mandatory=True, format='x(12)', initial='',
                 label='User ID',
                 help='User ID for logs')
        t.column('InUse', 'logical', initial='yes',
                 label='Used',
                 help='Request type is handled')
        t.column('Queue', 'integer', format='>>>9', initial='0',
                 label='Queue Nbr',
                 column_label='Queue',
                 help='Queue in which this type is handled')
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
        t.index('Queue', ['Brand', 'Queue', 'ReqType'], area='Sta_Index_2')
        t.index('ReqType', ['Brand', 'ReqType'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RequestType')

