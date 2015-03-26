from gearbox.migrations import Migration

class AddPaymConfig(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymConfig', area='Sta_Data_64',
                       label='Payment Configuration',
                       dump_name='paymconfig',
                       desc='Configuration rules for payments')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('PaymType', 'integer', format='>9', initial='0',
                 label='Payment Type',
                 column_label='PType',
                 help='Payment type')
        t.column('PaymSrc', 'character', initial='',
                 label='Payment Source',
                 column_label='Source',
                 help='Source of payment')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when usage of this rule ends')
        t.column('DebitAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Debit Account',
                 column_label='Debit',
                 help='Account for debit posting')
        t.column('CreditAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Credit Account',
                 column_label='Credit',
                 help='Account for credit posting')
        t.column('Description', 'character', format='x(50)', initial='',
                 help='Description of where to rule is used')
        t.column('PaymConfig', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Configuration ID',
                 column_label='ID',
                 help='Configuration ID, used in links')
        t.column('TaxRules', 'logical', initial='no',
                 label='Tax Rules',
                 column_label='Tax',
                 help='Are there rules for tax accounts')
        t.index('PaymConfig', ['PaymConfig'], area='Sta_Index_2',
                unique=True)
        t.index('PaymSrc', ['Brand', 'PaymSrc', 'PaymType', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('PaymType', ['Brand', 'PaymType', 'PaymSrc', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PaymConfig')

