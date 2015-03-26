from gearbox.migrations import Migration

class AddVASOper(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VASOper', area='Sta_Data_128',
                       dump_name='vasoper')
        t.column('OperID', 'character', format='x(3)', initial='',
                 label='Operator ID',
                 column_label='OperID',
                 help='OperatorID')
        t.column('VOName', 'character', format='x(16)', initial='',
                 label='Operator Name',
                 column_label='Name',
                 help='VAS Operator Name')
        t.column('OrigPrice', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Originating Price',
                 column_label='OrigPrice')
        t.column('TermPrice', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Terminating Price',
                 column_label='TermPrice')
        t.column('InvFee', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Invoicing Fee',
                 column_label='InvFee',
                 help='Invoicing Fee %')
        t.column('TermPrice2', 'decimal', decimals=3, format='>>,>>9.999', initial='0',
                 label='Terminating Price OtherNet',
                 column_label='TermPriceOther',
                 help='Terminating price not own network')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number (for billing)')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Debt Account',
                 column_label='Account',
                 help='Debt account for clearance invoice')
        t.column('BankAccount', 'character', format='X(20)', initial='',
                 label='Bank Account')
        t.column('MinFee', 'decimal', decimals=2, format='>>>9.99', initial='0',
                 label='Minimum Fee',
                 column_label='MinFee',
                 help='Minimum fee that is billed from operator each month')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 column_label='BillCode',
                 help='Billing item code, max 16 characters')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2')
        t.index('OperID', ['OperID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('VOName', ['VOName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('VASOper')

