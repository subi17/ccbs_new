from gearbox.migrations import Migration

class AddTMCounter(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TMCounter', area='Sta_Data_256',
                       label='TM Counter',
                       dump_name='tmcounter',
                       desc='TM counter')
        t.column('TMRuleSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Rule Sequence',
                 column_label='Seq',
                 help='Rule ID')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.column('LimitID', 'integer', format='>>9', initial='0',
                 label='Limit ID',
                 column_label='ID',
                 help='Limit that has been exceeded')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when rule expires')
        t.column('Amount', 'decimal', decimals=5, format='->>>>>>9.99999', initial='0',
                 column_label='Amt')
        t.index('CustNum', ['CustNum', 'TMRuleSeq', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('MsSeq', ['MsSeq', 'TMRuleSeq', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TMCounter')

