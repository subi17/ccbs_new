from gearbox.migrations import Migration

class AddInvRow(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvRow', area='Dyn_Data_64',
                       label='Invoice Rows',
                       dump_name='invrow',
                       desc='Invoice rows')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvoiceNo',
                 column_label='InvoiceNo',
                 help='Consecutive Invoice Number, 1 ... 99999999')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product number')
        t.column('GrossAmt', 'decimal', decimals=3, format='-zzzzz9.999', initial='0',
                 label='TotBrutto',
                 column_label='TotBrutto',
                 help='Total amount by product exl. discount')
        t.column('Amt', 'decimal', decimals=3, format='-zzzz,zz9.999', initial='0',
                 label='Net',
                 column_label='Net',
                 help='Total value of events')
        t.column('Qty', 'decimal', decimals=2, format='zzz,zz9-', initial='0',
                 label='Calls',
                 column_label='Calls',
                 help='No. of billed calls during this period')
        t.column('FromDate', 'date', format='99-99-99',
                 label='FirstCall',
                 column_label='FirstCall',
                 help='Date of first billed call of this type ')
        t.column('ToDate', 'date', format='99-99-99',
                 label='LastCall',
                 column_label='LastCall',
                 help='Date of last billed call of this type')
        t.column('SlsAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Acct',
                 column_label='Acct',
                 help='Account number')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation / memory field for row')
        t.column('FFRow', 'logical', format='Y/N', initial='no',
                 label='Contract',
                 column_label='Contract',
                 help='Is this a contract invoice row (Yes/No) ?')
        t.column('Minutes', 'integer', format='>>>>>>>9', initial='0',
                 column_label='Minutes',
                 help='Invoice row minutes')
        t.column('FFItemNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='ItemNo',
                 column_label='ItemNo',
                 help='"Individual ""invisible"" sequence for this item"')
        t.column('InvRowNum', 'integer', format='>>>>>>>9', initial='0',
                 label='RowId',
                 column_label='RowId',
                 help='Invoice row identification number')
        t.column('PeakMin', 'integer', format='>>>>>>>9', initial='0',
                 column_label='PeakMin',
                 help='Invoice row peak minutes')
        t.column('OffPeakMin', 'integer', format='>>>>>>>9', initial='0',
                 column_label='OffPeakMin',
                 help='Invoice row offpeak minutes')
        t.column('RowType', 'integer', format='>>>>>>>9', initial='0',
                 label='RowID',
                 column_label='RowID',
                 help='Invoice row identification number(Genesys)')
        t.column('Prefix', 'character', initial='',
                 column_label='Prefix',
                 help='Operator prefix')
        t.column('InvSect', 'character', initial='',
                 label='Section Code',
                 column_label='Section',
                 help='Code of an Invoice Section')
        t.column('Released', 'logical', initial='no',
                 column_label='Rel.',
                 help='Events have been released when row was credited')
        t.column('VATCode', 'integer', format='z9', initial='1',
                 label='VAT code',
                 column_label='VAT code')
        t.column('VATPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='VAT%',
                 column_label='VAT%',
                 help='VAT Percentage (%)')
        t.column('VATAmt', 'decimal', decimals=3, format='->>>>9.999', initial='0',
                 label='VAT',
                 column_label='VAT',
                 help='Amount of V.A.T')
        t.column('Division', 'character', initial='',
                 column_label='Division')
        t.column('Department', 'character', initial='',
                 column_label='Department',
                 help='Department of Division')
        t.column('CostCentre', 'character', initial='',
                 label='Ccentre',
                 column_label='Ccentre',
                 help='Cost Center of Department')
        t.column('CreditInvNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Credit invoice',
                 column_label='Credit',
                 help='Number of credit invoice')
        t.column('DataAmt', 'decimal', decimals=2, format='->>>>>>>>>>>>', initial='0',
                 label='Data Amount',
                 column_label='Data',
                 help='Transferred data amount')
        t.column('VASConAmt', 'decimal', decimals=3, format='->>>>>>9.999', initial='0',
                 label='Consumer Amount',
                 column_label='Cons.Amt',
                 help='Amount that has been billed from consumers for VAS tickets')
        t.column('VASOperAmt', 'decimal', decimals=3, format='->>>>>>9.999', initial='0',
                 label='Operator Amount',
                 column_label='OperAmt',
                 help='Amount for VAS tickets with operator prices')
        t.column('ServiceName', 'character', format='x(15)', initial='',
                 label='Service Name',
                 column_label='Service',
                 help='VAS operatorID + keyword')
        t.column('ServiceAddress', 'character', format='x(9)', initial='',
                 label='Service Address',
                 column_label='Serv.Addr',
                 help='CGW Short Number (VAS)')
        t.column('CLI', 'character', format='x(12)', initial='',
                 column_label='CLI',
                 help='A-Subscriber number')
        t.column('CreditAmt', 'decimal', decimals=3, format='->>>>>>9.999', initial='0',
                 label='Credit Amount',
                 column_label='Credited',
                 help='Credited amount')
        t.index('InvNum', ['InvNum'], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('InvRow')

