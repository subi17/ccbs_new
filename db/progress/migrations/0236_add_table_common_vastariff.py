from gearbox.migrations import Migration

class AddVASTariff(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VASTariff', area='Sta_Data_128',
                       dump_name='vastarif')
        t.column('TariffClass', 'character', format='x(2)', initial='',
                 column_label='TariffClass',
                 help='Tariff Class')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code, max 16 characters')
        t.column('TariffClassName', 'character', format='x(25)', initial='',
                 column_label='Tariff Class Name',
                 help='Name of the Tariff Class')
        t.column('PriceIncVAT', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='PriceIncVat',
                 column_label='PriceIncVat',
                 help='Price incl. VAT')
        t.column('PriceExclVAT', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 column_label='PriceExclVat',
                 help='Price Excl. VAT')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Tariff from',
                 column_label='Tariff from',
                 help='First effective date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To Date',
                 help='Last effective date')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN')
        t.index('TariffClass', ['TariffClass', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('VASTariff')

