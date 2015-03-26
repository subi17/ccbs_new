from gearbox.migrations import Migration

class AddCustIntEvent(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustIntEvent', area='Sta_Data_64',
                       label='Customer\'s Interest Events',
                       dump_name='custinte',
                       desc='Customer\'s interest events')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvoiceNo',
                 column_label='InvoiceNo',
                 help='Consecutive Invoicenumber, 1 ... 99999999')
        t.column('InvDate', 'date', format='99-99-99',
                 label='InvoiceDate',
                 column_label='InvoiceDate',
                 help='Invoice\'s date')
        t.column('DueDate', 'date', format='99-99-99',
                 label='InvoiDueDate',
                 column_label='InvoiDueDate',
                 help='Invoice\'s dueday')
        t.column('PaymDate', 'date', format='99-99-99',
                 label='PayDay',
                 column_label='PayDay',
                 help='Day when invoice was paid')
        t.column('LateDays', 'integer', format='ZZZ9', initial='0',
                 label='Delay Days',
                 column_label='Delay Days',
                 help='How many days the payment was late')
        t.column('Amt', 'decimal', decimals=2, format='ZZZZZ9.99-', initial='0',
                 label='Interest',
                 column_label='Interest',
                 help='Total interest, calculated due to delayed payment')
        t.column('InvAmt', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='InvoiceTot',
                 column_label='InvTot',
                 help='Invoice\'s total amount')
        t.column('PaidAmt', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='Paid Amt',
                 column_label='Paid Amt',
                 help='Total payment')
        t.column('Percent', 'decimal', decimals=2, format='Z9.99', initial='0',
                 label='Inter-%',
                 column_label='Inter-%',
                 help='Interest % used')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number, 1 ... 999999')
        t.column('BilledInvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='Invoice#',
                 column_label='InvoiceNo',
                 help='Invoice number which interests have calculated ')
        t.column('Voucher', 'integer', format='ZZZZZZ9', initial='0',
                 label='VoucherNo',
                 column_label='VoucherNo',
                 help='Consecutive voucher number of a payment')
        t.column('IntPerc', 'decimal', extent=10, decimals=2, format='z9.99', initial='0',
                 label='%%',
                 column_label='%%',
                 help='Interest %%')
        t.column('IntDays', 'integer', extent=10, format='zz9', initial='0',
                 label='Days',
                 column_label='Days',
                 help='Number of days')
        t.column('IntAmt', 'decimal', extent=10, decimals=2, initial='0',
                 label='Interest',
                 column_label='Interest',
                 help='Amount of interest with a particular %%')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('BilledInvNum', ['BilledInvNum'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('PaymDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'PaymDate'], area='Sta_Index_2',
                primary=True)
        t.index('InvNum', ['Brand', 'InvNum', ('PaymDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('InvNum_s', ['InvNum', 'PaymDate'], area='Sta_Index_2')
        t.index('VoucheSta_s', ['Voucher'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CustIntEvent')

