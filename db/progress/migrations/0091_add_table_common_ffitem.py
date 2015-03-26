from gearbox.migrations import Migration

class AddFFItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FFItem', area='Sta_Data_64',
                       label='Contract\'s Billing Periods',
                       dump_name='ffitem',
                       desc='Monthly item (one billable transaction) of a contract fee')
        t.column('FFNum', 'integer', format='zzzzzzz9', initial='0',
                 label='Contract',
                 column_label='Contract',
                 help='Consecutive number (sequence) of contract')
        t.column('FFItemNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='ItemNo',
                 column_label='ItemNo',
                 help='"Individual ""invisible"" sequence for this item"')
        t.column('BillPeriod', 'integer', format='999999', initial='0',
                 label='Period',
                 column_label='Period',
                 help='Period YYYYMM (month when a service shall be BILLED)')
        t.column('Amt', 'decimal', decimals=2, format='->>>,>>9.99', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Payable Contract Payment per bill')
        t.column('OwnCost', 'decimal', decimals=2, format='->>>,>>9.99', initial='0',
                 label='Our costs',
                 column_label='Our costs',
                 help='Our own costs per bill',
                 description='Our own costs per bill')
        t.column('Billed', 'logical', initial='no',
                 column_label='Billed',
                 help='Is this item billed (y/n)')
        t.column('InvNum', 'integer', format='zzzzzzzzz', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Number of an invoice where this item was billed')
        t.column('BillCode', 'character', format='x(6)', initial='',
                 label='ProdCode',
                 column_label='ProdCode',
                 help='Product code')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Individual Explanation Text for the invoice')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer number')
        t.column('Concerns', 'integer', extent=2, format='999999', initial='0',
                 column_label='Concerns',
                 help='Period that this fee concerns')
        t.column('InvRowId', 'integer', format='>>>>>>>9', initial='0',
                 label='RowId',
                 column_label='RowId',
                 help='Invoice row identification number')
        t.column('VATCode', 'integer', format='z9', initial='1',
                 label='VAT code',
                 column_label='VAT code',
                 help='VAT code 1 ... 10')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Subscriber',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='Yes',
                 label='VAT Included',
                 column_label='VAT',
                 help='Is VAT included or excluded in prices')
        t.column('AccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Account',
                 column_label='AccNum',
                 help='Account number')
        t.index('CustNum', ['CustNum', 'BillPeriod', 'Billed'], area='Sta_Index_1')
        t.index('FFItemNum', [('FFItemNum', 'DESCENDING')], area='Sta_Index_1',
                unique=True)
        t.index('FFNum', ['FFNum', 'BillPeriod', 'FFItemNum'], area='Sta_Index_1',
                primary=True)
        t.index('InvNum', ['InvNum'], area='Sta_Index_1')

    def down(self):
        self.drop_table('FFItem')

