from gearbox.migrations import Migration

class AddTMRLimit(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TMRLimit', area='Sta_Data_128',
                       label='TM Rule Limit',
                       dump_name='tmrlimit',
                       desc='TM rule limit')
        t.column('TMRuleSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Rule Sequence',
                 column_label='Seq',
                 help='Rule ID')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when rule expires')
        t.column('LimitID', 'integer', format='>>9', initial='0',
                 label='Limit ID',
                 column_label='ID')
        t.column('ValueType', 'integer', format='>9', initial='0',
                 label='Value Type',
                 column_label='Type',
                 help='Type of limit value')
        t.column('LimitAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Limit Amount',
                 column_label='Amount',
                 help='Limit amount')
        t.column('LimitPerc', 'decimal', decimals=2, format='>>9.99', initial='0',
                 label='Limit Percent',
                 column_label='Percent',
                 help='Limit percent')
        t.column('Action', 'integer', format='>>9', initial='0',
                 help='Action to be taken when limit is exceeded')
        t.column('SMSText', 'character', format='x(12)', initial='',
                 label='SMS Text',
                 column_label='SMS',
                 help='SMS text that is sent when limit is exceeded')
        t.column('MaxValue', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Limit Max',
                 column_label='Max',
                 help='Limit maximum value')
        t.column('MinValue', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Limit Min',
                 column_label='Min',
                 help='Limit minimum value')
        t.column('ActionParam', 'character', format='x(30)', initial='',
                 label='Action Parameters',
                 column_label='Parameters',
                 help='Parameters for the chosen action')
        t.index('LimitID', ['TMRuleSeq', 'LimitID', ('ToDate', 'DESCENDING')], area='Sta_Index_1',
                primary=True, unique=True)
        t.index('ToDate', ['TMRuleSeq', ('ToDate', 'DESCENDING'), 'LimitID'], area='Sta_Index_1')

    def down(self):
        self.drop_table('TMRLimit')

