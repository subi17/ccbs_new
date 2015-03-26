from gearbox.migrations import Migration

class AddInvCCN(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvCCN', area='Dyn_Data_256',
                       label='Invoiced Calls by CCN',
                       dump_name='invccn',
                       desc='Invoiced calls by CCN')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvoiceNo',
                 column_label='InvoiceNo',
                 help='Consecutive Invoicenumber, 1 ... 99999999')
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
        t.column('DataAmt', 'decimal', decimals=2, format='->>>>>>>>>>>>', initial='0',
                 label='Data Amount',
                 column_label='Data',
                 help='Transferred data amount')
        t.index('InvNum', ['InvNum', 'CCN', 'BillCode', 'TariffNum'], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('InvCCN')

