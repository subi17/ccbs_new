from gearbox.migrations import Migration

class AddErrorLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ErrorLog', area='Sta_Data_64',
                       label='Error Log',
                       dump_name='errorlog',
                       desc='Error log')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('ActionID', 'character', initial='',
                 label='Action ID',
                 column_label='ID')
        t.column('TableName', 'character', mandatory=True, format='X(15)', initial='',
                 label='Table Name',
                 column_label='Table',
                 help='Table related to the action')
        t.column('KeyValue', 'character', mandatory=True, format='x(16)', initial='',
                 label='Key Value',
                 column_label='Key',
                 help='Key value of the record related to the action')
        t.column('ActionTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Action Time Stamp',
                 column_label='Time Stamp',
                 help='Action time stamp')
        t.column('ErrorCode', 'character', initial='',
                 label='Error Code',
                 column_label='Error',
                 help='Error code')
        t.column('ErrorMsg', 'character', format='x(50)', initial='',
                 label='Error Message',
                 column_label='Message',
                 help='Error message')
        t.column('ErrorChar', 'character', initial='',
                 label='Character Value',
                 column_label='Char',
                 help='Character value')
        t.column('ErrorDec', 'decimal', decimals=4, format='->>>>>>9.9999', initial='0',
                 label='Decimal Value',
                 column_label='Dec',
                 help='Decimal value')
        t.column('ErrorStatus', 'integer', format='>9', initial='0',
                 label='Status')
        t.column('UserCode', 'character', initial='',
                 label='User ID',
                 help='User ID of log event')
        t.index('ActionID', ['Brand', 'ActionID', ('ActionTS', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('ErrorStatus', ['Brand', 'ErrorStatus', 'ActionID'], area='Sta_Index_1')
        t.index('TableName', ['Brand', 'TableName', 'KeyValue', 'ActionID', ('ActionTS', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('ErrorLog')

