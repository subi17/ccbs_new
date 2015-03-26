from gearbox.migrations import Migration

class AddAccount(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Account', area='Sta_Data_128',
                       label='Accounts',
                       dump_name='account',
                       desc='Accounts')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Account',
                 column_label='Account',
                 help='Account number')
        t.column('AccName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Account name')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo for account')
        t.column('VATCode', 'integer', format='z9', initial='0',
                 label='VAT Code',
                 column_label='VAT Code',
                 help='VAT code')
        t.column('AccType', 'integer', format='>9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Account usage type number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('AccName', ['Brand', 'AccName'], area='Sta_Index_2')
        t.index('AccNum', ['Brand', 'AccNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Account')

