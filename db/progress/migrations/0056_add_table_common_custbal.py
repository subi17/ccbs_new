from gearbox.migrations import Migration

class AddCustBal(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustBal', area='Sta_Data_128',
                       label='Customer\'s balances',
                       dump_name='custbal',
                       desc='Customer\'s balances\
\
\
')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('Deposit', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Deposit balance')
        t.column('OverPaym', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Overpayment',
                 column_label='OverPaym',
                 help='Overpayment balance')
        t.column('AdvPaym', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Adv.Payment',
                 column_label='Adv.Paym',
                 help='Advance payment balance')
        t.column('Debt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Customer\'s open balance')
        t.column('Interest', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Interest Debt',
                 column_label='Interest',
                 help='Customer\'s interest debt')
        t.column('PaymMethod', 'integer', format='->>>9', initial='0',
                 label='MethOfPaym',
                 column_label='MethOfPaym',
                 help='Customer\'s payment behavior (days +/- dueday)')
        t.column('PaymQty', 'integer', format='>>>>9', initial='0',
                 label='Qty of Payments',
                 column_label='PaymQty',
                 help='Qty of customer\'s payments',
                 description='Used in calculating payment behaviour')
        t.column('LatestPaym', 'date', format='99-99-99',
                 label='Latest Payment',
                 column_label='LastPaymDay',
                 help='Customer\'s latest payment date')
        t.column('LatestInv', 'integer', format='999999', initial='0',
                 label='InvPeriod',
                 column_label='InvPer',
                 help='Period of latest invoice')
        t.column('CLI', 'character', format='x(15)', initial='',
                 label='MSISDN')
        t.column('Refund', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Refund balance')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.column('CreditLoss', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Credit Loss',
                 column_label='Cr.Loss',
                 help='Credit loss balance')
        t.index('CustNum', ['CustNum', 'CLI'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MsSeq', ['CustNum', 'MsSeq'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CustBal')

