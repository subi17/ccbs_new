from gearbox.migrations import Migration

class AddActionLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ActionLog', area='Dyn_Data_64',
                       label='Action Log',
                       dump_name='actionlo',
                       desc='Action log\
')
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
        t.column('ActionPeriod', 'integer', format='999999', initial='0',
                 label='Period')
        t.column('ActionTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Action Time Stamp',
                 column_label='Time Stamp',
                 help='Action time stamp')
        t.column('ActionChar', 'character', initial='',
                 label='Character Value',
                 column_label='Char',
                 help='Character value')
        t.column('ActionDec', 'decimal', decimals=4, format='->>>>>>9.9999', initial='0',
                 label='Decimal Value',
                 column_label='Dec',
                 help='Decimal value')
        t.column('ActionStatus', 'integer', format='>9', initial='0',
                 label='Status')
        t.column('UserCode', 'character', initial='',
                 label='User Code',
                 column_label='User',
                 help='User who triggered the action')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='From date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='To date')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.index('ActionID', ['Brand', 'ActionID', ('ActionTS', 'DESCENDING')], area='Dyn_Index_1',
                primary=True)
        t.index('CustNum', ['CustNum', 'ActionID', ('ToDate', 'DESCENDING')], area='Dyn_Index_1')
        t.index('TableName', ['Brand', 'TableName', 'KeyValue', 'ActionID', ('ActionPeriod', 'DESCENDING')], area='Dyn_Index_1')
        t.index('UserCode', ['Brand', 'UserCode', 'ActionID', ('ActionTS', 'DESCENDING')], area='Dyn_Index_1')

    def down(self):
        self.drop_table('ActionLog')

