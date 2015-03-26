from gearbox.migrations import Migration

class AddBankAccount(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BankAccount', area='Sta_Data_64',
                       label='Company\'s Bank Accounts',
                       dump_name='bankacco',
                       desc='Company\'s bank accounts')
        t.column('UnitCode', 'integer', format='>>9', initial='0',
                 label='Unit',
                 help='Unit number')
        t.column('BankAccount', 'character', format='X(20)', initial='',
                 label='Bank Account')
        t.column('BankData', 'character', format='X(20)', initial='',
                 label='Bank Data',
                 help='Bank account in standard format for transfer files')
        t.column('PrintCode', 'character', initial='',
                 label='Printing',
                 help='Printing to forms',
                 description='One position per form, e.g. 1 = invoice, 2 = reminder. Empty or zero means that account is not printed, a positive value indicates the printing order if several accounts are printed. ')
        t.column('Currency', 'character', format='x(5)', initial='',
                 column_label='Currency',
                 help='Currency code')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('InvForm', 'character', format='x(60)', initial='',
                 label='Invoice Forms',
                 column_label='Forms',
                 help='Invoice forms, to which this bank account is printed')
        t.column('BarCode', 'character', format='x(60)', initial='',
                 label='BarCode Forms',
                 column_label='BarCode',
                 help='Invoice forms, in which bar code uses this bank account')
        t.column('BankOffice', 'character', mandatory=True, format='x(40)', initial='',
                 label='Office',
                 column_label='Office',
                 help='Bank office\'s name')
        t.index('AccNum', ['Brand', 'BankAccount'], area='Sta_Index_2')
        t.index('UnitBank', ['Brand', 'UnitCode', 'BankAccount'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('BankAccount')

