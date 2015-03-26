from gearbox.migrations import Migration

class AddTMQueue(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TMQueue', area='Sta_Data_128',
                       label='TM Queue',
                       dump_name='tmqueue',
                       desc='TM queue')
        t.column('EventID', 'integer', format='>>>>>>>>>>9', initial='0',
                 label='Event ID',
                 column_label='ID',
                 help='Event id')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.column('DateSt', 'date', format='99-99-99',
                 label='Event Date',
                 column_label='Date',
                 help='Event date')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code')
        t.column('RateCCN', 'integer', mandatory=True, format='>>9', initial='0',
                 label='CCN',
                 help='Call case number')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-number',
                 column_label='BDest')
        t.column('SpoCMT', 'integer', format='>>>9', initial='0',
                 label='TCC',
                 help='Technical call case number')
        t.column('BillDur', 'integer', format='>>>>>>>9', initial='0',
                 label='Billing Duration',
                 column_label='Duration',
                 help='Duration')
        t.column('Amount', 'decimal', decimals=5, format='->>>>>>>9.99999', initial='0')
        t.column('Qty', 'integer', format='-9', initial='0',
                 label='Quantity',
                 column_label='Qty')
        t.column('DataIn', 'decimal', decimals=2, format='>>>>>>>9.99', initial='0',
                 label='Data In',
                 help='Data in')
        t.column('DataOut', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Data Out',
                 help='Data out')
        t.column('InvCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Inv.Customer',
                 column_label='InvCust',
                 help='Invoicing customer\'s number')
        t.column('AgrCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Agr.Customer',
                 column_label='AgrCust',
                 help='Agreement customer\'s number')
        t.column('CLIType', 'character', format='x(12)', initial='',
                 label='CLI Type',
                 column_label='CLIType',
                 help='CLI type')
        t.column('PayType', 'integer', format='9', initial='0',
                 label='Payment Type',
                 column_label='PayType',
                 help='Payment type')
        t.column('PPBalance', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Prepaid Balance',
                 column_label='PP Balance',
                 help='Prepaid balance after this ticket')
        t.index('DateSt', ['DateSt'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TMQueue')

