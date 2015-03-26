from gearbox.migrations import Migration

class AddCoBasis(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CoBasis', area='Sta_Data_128',
                       label='CoBasis',
                       dump_name='cobasis',
                       desc='Basis for commission rule\
')
        t.column('CoRuleID', 'integer', format='>>>>>9', initial='0',
                 label='Rule ID',
                 column_label='RuleID',
                 help='Commission rule ID')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing Item code')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call case number for calls')
        t.column('CoAmt', 'decimal', decimals=2, initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Fixed commission amount')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CoPerc', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Commission %',
                 column_label='Comm. %',
                 help='Percentage of billed amount')
        t.column('CommLimit', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Commission Limit',
                 column_label='Limit',
                 help='Commission is paid only for the amount that exceeds this limit')
        t.column('SubsQty', 'integer', format='>>>>>9', initial='0',
                 label='Subscription Qty',
                 column_label='Subs.Qty',
                 help='Subscription qty limit for gaining this percent/fixed amt')
        t.index('BillCode', ['Brand', 'CoRuleID', 'BillCode', 'CCN'], area='Sta_Index_2',
                primary=True)
        t.index('CCN', ['Brand', 'CoRuleID', 'CCN', 'BillCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CoBasis')

