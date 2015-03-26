from gearbox.migrations import Migration

class AddOffer(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('Offer', area='Sta_Data_64',
                       label='Offer',
                       dump_name='offer',
                       desc='Offer header')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('Offer', 'character', format='x(12)', initial='',
                 label='Offer ID',
                 column_label='Offer')
        t.column('Description', 'character', format='x(40)', initial='',
                 help='Description of offer')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when offer becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Date when offer expires')
        t.column('OfferAmount', 'decimal', decimals=2, format='->>>>>>>>9.99', initial='0',
                 label='Offer Amount',
                 column_label='Amount',
                 help='Total price for the offer')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='no',
                 label='Tax Included',
                 column_label='Tax Incl.',
                 help='Tax included in amount')
        t.column('DispItemAmounts', 'integer', format='9', initial='0',
                 label='Disp Item Amounts',
                 column_label='Item Amt',
                 help='Display separate item amounts')
        t.column('Priority', 'integer', format='>>>9', initial='0',
                 help='Priority in contrast to other offers')
        t.column('Active', 'logical', initial='no',
                 help='Is offer active')
        t.index('Active', ['Brand', 'Active', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('Offer', ['Brand', 'Offer'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ToDate', ['Brand', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('Offer')

