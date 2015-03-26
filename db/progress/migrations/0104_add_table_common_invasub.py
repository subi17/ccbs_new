from gearbox.migrations import Migration

class AddInvASub(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvASub', area='Dyn_Data_64',
                       label='Invoiced Calls by CLI',
                       dump_name='invasub',
                       desc='Invoiced calls by CLI')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvoiceNo',
                 column_label='InvoiceNo',
                 help='Consecutive Invoice Number, 1 ... 99999999')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Consecutive country number')
        t.column('Qty', 'integer', format='>>>>>>9', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Amount of call to this CCN')
        t.column('Minutes', 'integer', format='>>>>>>>9', initial='0',
                 label='AmtMin',
                 column_label='AmtMin',
                 help='Amount of minutes to this CCN')
        t.column('Amt', 'decimal', decimals=6, format='>>>>>>9.99', initial='0',
                 label='Value',
                 column_label='Value',
                 help='Value of calls to this CCN')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Sub',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='ProdCd',
                 column_label='ProdCd',
                 help='Product code, max 16 characters')
        t.column('TariffNum', 'integer', format='>>>>>9', initial='0',
                 label='TariffID',
                 column_label='TariffID',
                 help='Tariff ID for rate used')
        t.column('GenPrice', 'decimal', decimals=6, format='zz,zz9.99', initial='0',
                 column_label='GenPrice',
                 help='Call\'s general price')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='Calls invoice sequence')
        t.column('TBDurat', 'integer', extent=6, format='zzzzz9', initial='0',
                 label='TB Dur.',
                 column_label='TB Dur.',
                 help='Duration (sec) within each timeband')
        t.column('FromDate', 'date', format='99-99-99',
                 label='DateFrom',
                 column_label='DateFrom',
                 help='FROM date (oldest call date)')
        t.column('ToDate', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='TO date (latest call date)')
        t.column('DataAmt', 'decimal', decimals=2, format='->>>>>>>>>>>>', initial='0',
                 label='Data Amount',
                 column_label='Data',
                 help='Transferred data amount')
        t.column('MPMAmt', 'decimal', decimals=6, format='>>>>>>9.99', initial='0',
                 label='MPM Amount',
                 column_label='MPM',
                 help='Value of MPM')
        t.column('MpmRid', 'character', initial='',
                 label='Reporting ID',
                 column_label='MpmRid',
                 help='Reporting id')
        t.column('ServRid', 'character', initial='',
                 label='Service ID',
                 column_label='ServRid',
                 help='Service reporting id')
        t.index('InvNum', ['InvNum', 'CLI', 'CCN', 'BillCode'], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('InvASub')

