from gearbox.migrations import Migration

class AddPaymPlan(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymPlan', area='Sta_Data_128',
                       label='Payment Plan',
                       dump_name='paymplan',
                       desc='Payment plan for unpaid invoices')
        t.column('PPlanID', 'integer', format='>>>>>>>9', initial='0',
                 label='Payment Plan ID',
                 column_label='PP ID',
                 help='Payment plan ID',
                 description='sequence pplan')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer\'s number')
        t.column('PPDate', 'date', format='99-99-99',
                 label='Creation Date',
                 column_label='Date',
                 help='Date when payment plan was done')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Total debt of invoices that belong to payment plan')
        t.column('BankDays', 'integer', format='>>9', initial='0',
                 label='Bank Days',
                 column_label='BankDays',
                 help='Bank days, that will be waited after due date for payment')
        t.column('PPStatus', 'integer', format='9', initial='0',
                 label='Status',
                 help='Status of payment plan',
                 description='e.g. sent, accepted, cancelled, paid')
        t.column('PPType', 'integer', format='9', initial='0',
                 label='Type',
                 help='Type of payment plan',
                 description='e.g. rem(inder), cc (cust.care)')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('Orderer', 'integer', format='>>>>>>>9', initial='0',
                 help='Customer who ordered the plan (confirmation letter receiver)')
        t.column('MsRequest', 'integer', format='>>>>>>>9', initial='0',
                 label='Request ID',
                 column_label='ID',
                 help='Unique ID for request')
        t.index('CustNum', ['Brand', 'CustNum', ('PPDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('PPDate', ['Brand', ('PPDate', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')
        t.index('PPlanID', ['PPlanID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('PPStatus', ['Brand', 'PPStatus', 'CustNum'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PaymPlan')

