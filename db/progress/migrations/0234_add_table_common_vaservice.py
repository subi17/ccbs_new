from gearbox.migrations import Migration

class AddVAService(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VAService', area='Sta_Data_256',
                       dump_name='VAServic')
        t.column('Bdest', 'character', initial='',
                 column_label='Bdest',
                 help='B-Destination')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 column_label='BillCode',
                 help='Billing item code, max 16 characters')
        t.column('ServiceName', 'character', format='x(15)', initial='',
                 column_label='Service Name',
                 help='OperatorID + keyword')
        t.column('ServiceAddress', 'character', format='x(9)', initial='',
                 column_label='ServiceAddress',
                 help='CGW Short Number')
        t.column('PriceIncVAT', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Price Including VAT',
                 column_label='PriceIncVat',
                 help='Price incl. VAT')
        t.column('PriceExclVAT', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Price Excluding VAT',
                 column_label='PriceExclVAt',
                 help='Price Excl. VAT')
        t.column('InvEvent', 'integer', format='9', initial='0',
                 column_label='InvEvent',
                 help='Invoicable Event')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='First effective date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Last effective date')
        t.column('MinFee', 'decimal', decimals=2, format='>>>9.99', initial='0',
                 label='Minimum Fee',
                 column_label='MinFee',
                 help='Minimum fee that is billed from operator each month')
        t.column('ServType', 'character', format='x(2)', initial='',
                 column_label='ServType',
                 help='Service Type')
        t.index('BDest', ['Bdest', 'ServiceName', ('FromDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('ServiceName', ['ServiceName', 'ServiceAddress'], area='Sta_Index_2')

    def down(self):
        self.drop_table('VAService')

