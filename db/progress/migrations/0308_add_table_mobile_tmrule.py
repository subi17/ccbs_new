from gearbox.migrations import Migration

class AddTMRule(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TMRule', area='Sta_Data_128',
                       label='TM Rule',
                       dump_name='tmrule',
                       desc='Ticket management rule')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('TMRuleSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Rule Sequence',
                 column_label='Seq',
                 help='Rule ID')
        t.column('Name', 'character', format='x(40)', initial='')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when rule expires')
        t.column('CounterItems', 'character', format='x(40)', initial='',
                 label='Counter Items',
                 column_label='Items',
                 help='Items that are used in counter collection')
        t.column('CounterAmount', 'character', format='x(15)', initial='',
                 label='Counter Amount',
                 column_label='Amount',
                 help='Amount that is collected to counter')
        t.column('CounterType', 'integer', format='>9', initial='0',
                 label='Counter Type',
                 column_label='Type',
                 help='Type of counter')
        t.column('CounterPeriod', 'integer', format='>9', initial='0',
                 label='Counter Period',
                 column_label='Period',
                 help='Period of counter')
        t.column('NewCustomer', 'logical', initial='yes',
                 label='New Customers',
                 column_label='New Cust',
                 help='Copy rule automatically to new customers')
        t.column('RuleActive', 'integer', format='9', initial='0',
                 label='Active',
                 help='Active status')
        t.column('LimitSource', 'integer', format='>9', initial='1',
                 label='Limit Source',
                 column_label='LimitSrc',
                 help='Which limit is used in analysis')
        t.column('PayType', 'integer', format='9', initial='0',
                 label='Payment Type',
                 column_label='PayType',
                 help='Payment type')
        t.column('LimitCompare', 'integer', format='9', initial='1',
                 label='Limit Comparison',
                 column_label='Compare',
                 help='Limit comparison method')
        t.index('CounterType', ['Brand', 'CounterType', 'TMRuleSeq'], area='Sta_Index_1')
        t.index('Name', ['Brand', 'Name'], area='Sta_Index_1')
        t.index('TMRuleSeq', ['TMRuleSeq'], area='Sta_Index_1',
                primary=True, unique=True)
        t.index('ToDate', ['Brand', ('ToDate', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('TMRule')

