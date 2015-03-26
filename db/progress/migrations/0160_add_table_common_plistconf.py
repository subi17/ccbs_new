from gearbox.migrations import Migration

class AddPListConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PListConf', area='Sta_Data_128',
                       dump_name='plistcon',
                       desc='Price configuration\
\
')
        t.column('dFrom', 'date', format='99-99-99',
                 label='DateFrom',
                 column_label='DateFrom',
                 help='FROM date for price list validation')
        t.column('dTo', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='TO date for price list validation')
        t.column('Prior', 'integer', format='>9', initial='0',
                 label='Priority',
                 column_label='Priority',
                 help='Priority for simultaneous price list')
        t.column('PriceList', 'character', initial='',
                 label='Plist',
                 column_label='Plist',
                 help='Code (identifier) for a Price List')
        t.column('RatePlan', 'character', initial='',
                 label='Rating Plan',
                 column_label='RatePlan',
                 help='Rating plan code')
        t.column('StartCharge', 'logical', format='A/P', initial='Yes',
                 label='Start charge',
                 column_label='Start charge',
                 help='Allow / Prohibit starting charges (A/P)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Browse', ['Brand', 'RatePlan', 'dFrom', 'dTo', 'Prior'], area='Sta_Index_2')
        t.index('RatePlan', ['Brand', 'RatePlan', ('dFrom', 'DESCENDING'), ('dTo', 'DESCENDING'), 'Prior'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('PListConf')

