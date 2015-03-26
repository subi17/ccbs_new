from gearbox.migrations import Migration

class AddFATime(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FATime', area='Sta_Data_64',
                       label='Free Air Time',
                       dump_name='fatime',
                       desc='Free air time (or free qty or free sum)')
        t.column('FATNum', 'integer', format='>>>>>>9', initial='0',
                 label='FatId',
                 help='Unique id for fat-definition')
        t.column('Period', 'integer', format='999999', initial='0',
                 help='Period when the free air time is to be used')
        t.column('MsSeq', 'integer', initial='0',
                 label='Mobsub',
                 column_label='Msub',
                 help='Link to mobsub-table')
        t.column('Amt', 'decimal', decimals=2, initial='0',
                 label='Free amount',
                 column_label='FAT',
                 help='Free amount (minutes, qty or sum)')
        t.column('QtyUnit', 'character', initial='',
                 label='Qty Unit',
                 column_label='Unit',
                 help='Unit of the free amount')
        t.column('Transfer', 'logical', initial='no',
                 label='Transferrable',
                 column_label='Transfer',
                 help='Transferrable to next period')
        t.column('PayerType', 'character', initial='',
                 label='Payer Type',
                 column_label='PType',
                 help='Type of the payer (f.ex. reseller)')
        t.column('FTGrp', 'character', initial='',
                 label='FatGroup',
                 column_label='FtGrp',
                 help='Fat Group (for products)')
        t.column('BillPrice', 'decimal', decimals=2, initial='0',
                 label='Billing price',
                 column_label='Price',
                 help='Price for billing the payer')
        t.column('PriceUnit', 'integer', format='9', initial='0',
                 label='Price Unit',
                 column_label='PUnit',
                 help='Is the price per unit or a total price')
        t.column('Used', 'decimal', decimals=2, initial='0',
                 label='Used Amount',
                 column_label='Used',
                 help='Used FAT amount')
        t.column('InvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Invoice number, where FAT was used')
        t.column('InvRowId', 'integer', format='>>>>>>>9', initial='0',
                 label='Invoice Line',
                 column_label='InvLine',
                 help='Invoice line identification number')
        t.column('TransQty', 'decimal', decimals=2, initial='0',
                 label='Transferred',
                 column_label='Tr.Amt',
                 help='Transferred amount')
        t.column('TransPeriod', 'integer', format='999999', initial='0',
                 label='Transf.Period',
                 column_label='Tr.Period',
                 help='Period which the unused qty is transferred to')
        t.column('OrigFat', 'integer', initial='0',
                 label='Original FAT',
                 column_label='Orig.FAT',
                 help='Link to the original FAT (from which transferred)')
        t.column('FFItemNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='Link to BItem',
                 column_label='BItem',
                 help='Link to BITEM which is created to bill the payer')
        t.column('InvAmt', 'decimal', decimals=2, initial='0',
                 label='Invoiced Amount',
                 column_label='Invoiced',
                 help='Invoiced amount (billed from the payer)')
        t.column('PInvRowNum', 'integer', format='>>>>>>>9', initial='0',
                 label='P.Invoice line',
                 column_label='P.InvLine',
                 help='Invoice line where FAT was billed from the payer')
        t.column('Payer', 'integer', format='zzzzzzzz9', initial='0',
                 column_label='Payer',
                 help='Payer\'s code')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation / memory field for Free Air Time')
        t.column('CLI', 'character', format='x(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('FATId', 'integer', initial='0',
                 label='fatimeId',
                 help='Fatime table\'s primary key.')
        t.column('FATClass', 'logical', format='Calls/FixedFees', initial='yes',
                 label='Class',
                 column_label='Class',
                 help='(C)alls / (F)ixed fees')
        t.column('Interval', 'integer', format='z9', initial='1',
                 column_label='Interval',
                 help='"FATime interval; number of months betw. 2 consecutive bills',
                 valexp='Interval > 0 and Interval < 13',
                 valmsg='FATime interval must be between 1 and 12 !')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('FATType', 'integer', format='9', initial='0',
                 label='FATime Type',
                 column_label='Type',
                 help='FATime type',
                 description='0=calls, 1=fixed fees, 2=all')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='Yes',
                 label='VAT Included',
                 column_label='VAT',
                 help='Is VAT included or excluded in prices')
        t.column('FATPerc', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Percentage',
                 column_label='Perc.',
                 help='FATime percentage')
        t.column('HostTable', 'character', format='x(16)', initial='',
                 help='Host table')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 label='Link Key',
                 column_label='Key',
                 help='Link key to hosttable')
        t.column('LastPeriod', 'integer', format='999999', initial='0',
                 label='Last Period',
                 column_label='Last',
                 help='Last period which FAtime can be applied to')
        t.index('CLI', ['Brand', 'CLI'], area='Sta_Index_2')
        t.index('CLI_s', ['CLI'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum'], area='Sta_Index_2')
        t.index('FATId', ['Brand', 'FATId'], area='Sta_Index_2')
        t.index('FATNum', ['Brand', 'FATNum'], area='Sta_Index_2',
                unique=True)
        t.index('FatType', ['CustNum', 'CLI', 'FATType', 'InvNum', 'Period'], area='Sta_Index_2',
                primary=True)
        t.index('ftgrd', ['Brand', 'FTGrp'], area='Sta_Index_2')
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue'], area='Sta_Index_2')
        t.index('InvNum', ['InvNum'], area='Sta_Index_2')
        t.index('Mobsub', ['Brand', 'MsSeq', 'Period'], area='Sta_Index_2')
        t.index('OrigFat', ['Brand', 'OrigFat'], area='Sta_Index_2')

    def down(self):
        self.drop_table('FATime')

