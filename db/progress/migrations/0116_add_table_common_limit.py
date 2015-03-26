from gearbox.migrations import Migration

class AddLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Limit', area='Sta_Data_256',
                       label='Limit',
                       dump_name='limit',
                       desc='Limits for customer and subscription')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.column('TMRuleSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Rule Sequence',
                 column_label='Seq',
                 help='Rule ID')
        t.column('LimitID', 'integer', format='>>9', initial='0',
                 label='Limit ID',
                 column_label='ID')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when rule expires')
        t.column('LimitAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Limit Amount',
                 column_label='Amount',
                 help='Limit amount')
        t.column('LimitPerc', 'decimal', decimals=2, format='>>9.99', initial='0',
                 label='Limit Percent',
                 column_label='Percent',
                 help='Limit percent')
        t.column('LimitType', 'integer', format='>9', initial='0',
                 label='Limit Type',
                 column_label='Type',
                 help='Type of limit')
        t.column('ValueType', 'integer', format='>9', initial='0',
                 label='Value Type',
                 column_label='Type',
                 help='Type of limit value')
        t.column('DefValue', 'logical', initial='Yes',
                 label='Default Value',
                 column_label='Default',
                 help='Limit has the default value')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['CustNum', 'LimitType', 'TMRuleSeq', 'LimitID', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('LimitType', ['Brand', 'LimitType', 'LimitID', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('MsSeq', ['MsSeq', 'LimitType', 'TMRuleSeq', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('TMRuleSeq', ['TMRuleSeq', 'LimitID', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('Limit')

