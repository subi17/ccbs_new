from gearbox.migrations import Migration

class AddCustCat(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustCat', area='Sta_Data_128',
                       label='Customer Category',
                       dump_name='custcat',
                       desc='Customer category')
        t.column('Category', 'character', format='x(4)', initial='',
                 column_label='Category',
                 help='Category code, max 4 characters')
        t.column('CatName', 'character', format='x(30)', initial='',
                 label='CategName',
                 column_label='CategName',
                 help='Name\'s of customer category')
        t.column('MaxCredit', 'integer', format='z,zzz,zz9', initial='0',
                 column_label='MaxCredit',
                 help='Max credit for a customer in this category')
        t.column('ArAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Receivables Account',
                 column_label='Receiv.',
                 help='Account no. for Receivables')
        t.column('PerAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Periodizing Account',
                 column_label='Period.',
                 help='Account for periodizing')
        t.column('UnbillAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Unbilled Account',
                 column_label='Unbilled',
                 help='Account no. for unbilled events (balance sheet)')
        t.column('IntType', 'integer', format='9', initial='1',
                 label='Interest type',
                 column_label='IntType',
                 help='1 = fixed interest, 2 = in addition to euribor')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('PaymTerm', 'integer', format='>9', initial='0',
                 label='Payment Term',
                 column_label='PaymTerm',
                 help='Terms of payment (days)')
        t.column('CustIdType', 'character', initial='',
                 label='Customer ID Type',
                 column_label='ID Type',
                 help='Customer ID type')
        t.column('SelfEmployed', 'logical', initial='no',
                 label='Selfemployed',
                 column_label='Selfempl.')
        t.column('MobSubLimit', 'integer', format='>>>>9', initial='0',
                 label='Subscription Max Limit',
                 column_label='MSLimit')
        t.index('Category', ['Brand', 'Category'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CatName', ['Brand', 'CatName', 'MaxCredit'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CustCat')

