from gearbox.migrations import Migration

class AddPaymConfTax(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymConfTax', area='Sta_Data_64',
                       dump_name='paymconftax',
                       desc='Configuration rules for tax accounts of payments')
        t.column('PaymConfig', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Configuration ID',
                 column_label='ID',
                 help='Configuration ID, used in links')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.column('TaxAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Tax Account',
                 column_label='Tax Acc',
                 help='Tax account number')
        t.index('PaymConfig', ['PaymConfig', 'TaxZone'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PaymConfTax')

