from gearbox.migrations import Migration

class AddPriceList(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PriceList', area='Sta_Data_128',
                       label='Price Lists',
                       dump_name='pricelis',
                       desc='Price list header')
        t.column('PriceList', 'character', initial='',
                 label='Plist',
                 column_label='Plist',
                 help='Code (identifier) for a Price List')
        t.column('PLName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of a Price List')
        t.column('Memo', 'character', format='x(60)', initial='',
                 column_label='Memo')
        t.column('Currency', 'character', format='x(3)', initial='',
                 column_label='Currency',
                 help='Price List\'s currency code')
        t.column('Rounding', 'integer', format='9', initial='2',
                 label='Decimals',
                 column_label='Decimals',
                 help='How many decimals will be used for call prices')
        t.column('InclVAT', 'logical', format='Incl/Excl', initial='yes',
                 label='VAT',
                 column_label='VAT',
                 help='Is VAT Included/Excluded in tariffs')
        t.column('CurrUnit', 'logical', format='Full/Sub', initial='yes',
                 column_label='CurrUnit',
                 help='Currency FULL (1) or SUB (1/100)')
        t.column('Prefix', 'character', format='x(5)', initial='',
                 column_label='Prefix',
                 help='Operator prefix where price list is attached to')
        t.column('AutoCreate', 'character', format='x(30)', initial='',
                 label='Autom.create',
                 column_label='AutoCreate',
                 help='Comma separated list of pricelists that follow this pricelist')
        t.column('DedicList', 'logical', format='Dedicated/General', initial='no',
                 label='Dedicated Pricelist',
                 column_label='Dedicated',
                 help='Type of price list; general or dedicated to certain customers')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('DedicList', ['Brand', 'DedicList', 'PriceList'], area='Sta_Index_2')
        t.index('DedicName', ['Brand', 'DedicList', 'PLName'], area='Sta_Index_2')
        t.index('PLName', ['Brand', 'PLName', 'PriceList'], area='Sta_Index_2',
                unique=True)
        t.index('PriceList', ['Brand', 'PriceList'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PriceList')

