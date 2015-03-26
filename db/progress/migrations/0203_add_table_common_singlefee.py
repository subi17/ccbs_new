from gearbox.migrations import Migration

class AddSingleFee(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('SingleFee', area='Sta_Data_32',
                       label='Single Fee',
                       dump_name='singlefe',
                       desc='Single billable fee')
        t.column('FMItemId', 'integer', format='zzzzzzzz9', initial='0',
                 label='ItemNo',
                 column_label='ItemNo',
                 help='"Individual ""invisible"" sequence for this item"')
        t.column('BillPeriod', 'integer', format='999999', initial='0',
                 label='Period',
                 column_label='Period',
                 help='Period YYYYMM (month when this item shall be BILLED)')
        t.column('Amt', 'decimal', decimals=2, format='->,>>>,>>9.99', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Payable Amount ex VAT')
        t.column('OwnCost', 'decimal', decimals=2, format='->,>>>,>>9.99', initial='0',
                 label='Our costs',
                 column_label='Our costs',
                 help='Our own costs per bill')
        t.column('Billed', 'logical', initial='no',
                 column_label='Billed',
                 help='Is this item billed (y/n)')
        t.column('InvNum', 'integer', format='zzzzzzzzz', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Number of an invoice where this item was billed')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Individual Explanation Text for the invoice')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='No',
                 column_label='No',
                 help='Consecutive No. for Customer\'s Invoicing Target')
        t.column('BillLevel', 'character', format='x(10)', initial='',
                 label='Level',
                 column_label='Level',
                 help='Hierarchical Billing Level code')
        t.column('Concerns', 'integer', extent=2, format='999999', initial='0',
                 column_label='Concerns',
                 help='Period that this fee concerns')
        t.column('InvRowId', 'integer', format='>>>>>>>9', initial='0',
                 column_label='InvRowId',
                 help='Invoice row identification number')
        t.column('CalcObj', 'character', format='x(16)', initial='',
                 label='CalcObject',
                 column_label='CalcObject',
                 help='Calculation Object Within Customer/InvTarg/Billing Level')
        t.column('MiscInt', 'integer', extent=10, format='zzzzzzzz9', initial='0',
                 label='Integer',
                 column_label='Integer',
                 help='Miscellaneous integer data')
        t.column('MiscDec', 'decimal', extent=10, decimals=2, format='zzzzz9.99999', initial='0',
                 label='Decimal',
                 column_label='Decimal',
                 help='Miscellaneous decimal data')
        t.column('BillType', 'character', initial='',
                 label='Billing Type',
                 column_label='Billing Type',
                 help='Billing type')
        t.column('HostTable', 'character', format='x(16)', initial='',
                 column_label='HostTable')
        t.column('VATCode', 'integer', format='z9', initial='1',
                 label='VAT code',
                 column_label='VAT code',
                 help='VAT code 1 ... 10')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 column_label='KeyValue')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Subscriber',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='Code of Billing Event')
        t.column('CustPP', 'integer', format='->>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Internal sequence no of Contract/Product Pack')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Contract', 'character', initial='',
                 label='Contract ID',
                 column_label='ContrID')
        t.column('ServiceLimitGroup', 'character', format='x(16)', initial='',
                 column_label='ServiceLimitGroup',
                 help='Group Code of Service Limit')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='Yes',
                 label='VAT Included',
                 column_label='VAT',
                 help='Is VAT included or excluded in prices')
        t.column('Active', 'logical', initial='Yes',
                 help='Is fee active (i.e. billable)')
        t.index('BillCode', ['Brand', 'BillCode', 'CustNum'], area='Sta_Index_2')
        t.index('CalcObj', ['Brand', 'CalcObj'], area='Sta_Index_2')
        t.index('Contract', ['Brand', 'Contract', 'BillCode'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', 'HostTable', 'KeyValue', ('BillPeriod', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'BillPeriod', 'Billed'], area='Sta_Index_2',
                primary=True)
        t.index('FMItemId', ['Brand', 'FMItemId'], area='Sta_Index_2',
                unique=True)
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue'], area='Sta_Index_2')
        t.index('InvNum', ['Brand', 'CustNum', 'InvNum'], area='Sta_Index_2')
        t.index('InvNum_s', ['InvNum'], area='Sta_Index_2')

    def down(self):
        self.drop_table('SingleFee')

