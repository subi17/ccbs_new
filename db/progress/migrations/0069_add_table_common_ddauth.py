from gearbox.migrations import Migration

class AddDDAuth(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DDAuth', area='Sta_Data_64',
                       label='Direct Debit Authorization',
                       dump_name='ddauth',
                       desc='Direct debit authorization')
        t.column('CustNum', 'integer', format='zzzzzzz', initial='0',
                 label='Customer Nbr',
                 column_label='Cust.Nbr',
                 help='Billable customer number')
        t.column('CustName', 'character', format='x(35)', initial='',
                 label='Billable',
                 column_label='Billable',
                 help='Customer\'s name (in authorization)')
        t.column('DDProcess', 'integer', format='9', initial='0',
                 label='Processing Code',
                 column_label='Processing Code',
                 help='Processing Code: 1=new, 2=change, 3=termination, 4=maintenance')
        t.column('Archive', 'character', format='x(20)', initial='',
                 label='Archive Code',
                 column_label='Archive Code',
                 help='Archive code of authorization')
        t.column('BankAcc', 'character', format='x(30)', initial='',
                 label='Bank Account',
                 column_label='Bank Account',
                 help='Payer\'s current bank account')
        t.column('AuthDate', 'date', format='9999/99/99',
                 label='Event Date',
                 column_label='Event Date',
                 help='Event date of authorization')
        t.column('OldBankAcc', 'character', format='x(30)', initial='',
                 label='Old Bank Account',
                 column_label='Old Bank Account',
                 help='Payer\'s old bank account')
        t.column('Identification', 'character', format='x(30)', initial='',
                 column_label='Identification',
                 help='Identification (cust.nbr, name, telephone etc.)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('AuthID', 'integer', initial='0',
                 label='Authorization ID',
                 column_label='ID',
                 help='Unique ID')
        t.index('AuthDate', ['Brand', ('AuthDate', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('AuthDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'AuthDate'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('DDAuth')

