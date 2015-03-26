from gearbox.migrations import Migration

class AddFixedFee(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FixedFee', area='Sta_Data_32',
                       label='Constant / Contract Fees',
                       dump_name='fixedfee',
                       desc='Constant / contract fees')
        t.column('FFNum', 'integer', format='zzzzzzz9', initial='0',
                 label='Contract',
                 column_label='Contract',
                 help='Consecutive number (sequence) of contract')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number')
        t.column('BillCode', 'character', format='x(6)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 label='MemoExp',
                 column_label='MemoExp',
                 help='Explanation text for the invoice')
        t.column('Amt', 'decimal', decimals=2, format='->>>,>>9.99', initial='0',
                 label='To Pay',
                 column_label='To Pay',
                 help='Contract payment per bill')
        t.column('Interval', 'integer', format='z9', initial='0',
                 column_label='Interval',
                 help='"Invoicing interval; number of months betw. 2 consecutive bills"',
                 valexp='Interval > 0 and Interval < 13',
                 valmsg='Invoicing interval must be between 1 and 12 !')
        t.column('OwnCost', 'decimal', decimals=2, format='->>>,>>9.99', initial='0',
                 label='Our costs',
                 column_label='Our costs',
                 help='Our own costs per bill',
                 description='Our own costs per bill')
        t.column('BegPeriod', 'integer', format='999999', initial='0',
                 label='FromPer',
                 column_label='FromPer',
                 help='1. Period YYYYMM (month when a service was DELIVERED)')
        t.column('EndPeriod', 'integer', format='999999', initial='999999',
                 label='ExpPer',
                 column_label='ExpPer',
                 help='Last Period YYYYMM (month when a service was DELIVERED)')
        t.column('BillMethod', 'integer', format='9', initial='3',
                 label='Type',
                 column_label='Type',
                 help='When this fee is to be billed: 1:before 2:during 3:after MONTH',
                 valexp='BillMethod > 0 and BillMethod < 4',
                 valmsg='Type code MUST be 1, 2 or 3 !')
        t.column('Active', 'logical', initial='no',
                 column_label='Active',
                 help='Is this contract active ?')
        t.column('InUse', 'logical', initial='Yes',
                 label='Used',
                 column_label='Used',
                 help='Is this payment in use ?')
        t.column('xxInclVat', 'logical', initial='y',
                 label='VAT',
                 column_label='VAT',
                 help='Shall VAT be added when billed (Y/N) ?')
        t.column('CalcObj', 'character', format='x(16)', initial='',
                 label='CalcObject',
                 column_label='CalcObject',
                 help='Calculation Object Within Customer/InvTarg/Billing Level')
        t.column('HostTable', 'character', format='x(16)', initial='',
                 column_label='HostTable')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Subscriber',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 column_label='KeyValue')
        t.column('BegDate', 'date', format='99-99-9999',
                 label='ContDate',
                 column_label='ContDate',
                 help='Date of Contract')
        t.column('CustPP', 'integer', format='->>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Internal sequence no of Contract/Product Pack')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='Code of Billing Event')
        t.column('InclAmt', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Included Amount',
                 column_label='Incl.Amt',
                 help='Amount of billable material that is included in this fee')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='Yes',
                 label='VAT Included',
                 column_label='VAT',
                 help='Is VAT included or excluded in prices')
        t.column('InclBillCode', 'character', format='x(16)', initial='',
                 label='Incl.Billing Item',
                 column_label='Incl.BItem',
                 help='Billing item that included amount concerns')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Contract', 'character', initial='',
                 label='Contract ID',
                 column_label='ContrID')
        t.column('ServiceLimitGroup', 'character', format='x(16)', initial='',
                 column_label='ServiceLimitGroup',
                 help='Group Code of Service Limit')
        t.index('BillCode', ['Brand', 'BillCode', 'CustNum'], area='Sta_Index_1')
        t.index('Contract', ['Brand', 'Contract', 'BillCode'], area='Sta_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', 'HostTable', 'KeyValue', ('EndPeriod', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('CustNum_s', ['CustNum', 'BillCode'], area='Sta_Index_1')
        t.index('CustPP', ['Brand', 'CustPP'], area='Sta_Index_1')
        t.index('FeeModel', ['Brand', 'FeeModel'], area='Sta_Index_1')
        t.index('FFNum', ['FFNum'], area='Sta_Index_1',
                unique=True)
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue'], area='Sta_Index_1')
        t.index('used', ['Brand', 'CustNum', 'InUse'], area='Sta_Index_1')

    def down(self):
        self.drop_table('FixedFee')

