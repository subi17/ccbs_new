from gearbox.migrations import Migration

class AddVATCode(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VATCode', area='Sta_Data_128',
                       label='VAT Code',
                       dump_name='vatcode',
                       desc='VAT Code')
        t.column('VATCode', 'integer', format='z9', initial='0',
                 label='VAT code',
                 column_label='VAT code',
                 valexp='vc-code < 11',
                 valmsg='VAT code must be between 1 ... 10 !')
        t.column('VCName', 'character', format='x(40)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Explanation for this VAT code')
        t.column('VATPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='%',
                 column_label='%',
                 help='Amount of VAT (%)')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Number',
                 column_label='Number',
                 help='Account number')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.column('TaxClass', 'character', initial='',
                 label='Tax Class',
                 column_label='Class',
                 help='Tax class')
        t.index('TaxClass', ['TaxClass', 'TaxZone'], area='Sta_Index_2')
        t.index('TaxZone', ['TaxZone', 'TaxClass'], area='Sta_Index_2')
        t.index('VatCode', ['VATCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('VatPerc', ['VATPerc', 'VATCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('VATCode')

