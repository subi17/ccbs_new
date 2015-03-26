from gearbox.migrations import Migration

class AddFMItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FMItem', area='Sta_Data_64',
                       label='Billing Event Items',
                       dump_name='bcitem',
                       desc='List of billable items of a single \'billing event\'')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='An unique code for a Billing Event')
        t.column('PriceList', 'character', initial='',
                 label='Price List',
                 column_label='Price List',
                 help='Code (identifier) for a Price List')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code')
        t.column('BillMethod', 'logical', format='Single/Fixed', initial='no',
                 label='Type',
                 column_label='Type',
                 help='Is this a (S)ingle fee or a (F)ixed fee ?')
        t.column('Interval', 'integer', format='z9', initial='0',
                 column_label='Interval',
                 help='Billing Interval (1= every month, 12=every year etc)')
        t.column('Amount', 'decimal', decimals=2, format='-z,zz9.99', initial='0',
                 label='Price',
                 column_label='Price',
                 help='Billable price of an item')
        t.column('BillType', 'character', initial='',
                 label='Billing Type',
                 column_label='Billing Type',
                 help='Type Of Billing Object')
        t.column('BillCycle', 'integer', format='9', initial='1',
                 label='BMeth',
                 column_label='BMeth',
                 help='When this fee is to be billed 1:before 2:during 3:after',
                 valexp='BillCycle > 0 and BillCycle < 4',
                 valmsg='Billing Method code MUST be 1, 2 or 3')
        t.column('FFItemQty', 'integer', format='>>9', initial='0',
                 label='Fixed Fee Qty',
                 column_label='FF Qty',
                 help='Quantity of fixed fee items')
        t.column('FFEndDate', 'date', format='99-99-99',
                 label='Fixed Fee End',
                 column_label='FF End',
                 help='End date for fixed fee')
        t.column('InclAmt', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Included Amount',
                 column_label='Incl.Amt',
                 help='Amount of billable material that is included in this fee')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='First effective date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Last effective date')
        t.column('InclBillCode', 'character', format='x(16)', initial='',
                 label='Incl.Billing Item',
                 column_label='Incl.BItem',
                 help='Billing item that included amount concerns')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('ServiceLimitGroup', 'character', format='x(16)', initial='',
                 column_label='ServiceLimitGroup',
                 help='Group Code of Service Limit')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('BrokenRental', 'integer', format='9', initial='0',
                 label='Broken Rental',
                 column_label='Broken Rental')
        t.column('FirstMonthBR', 'integer', format='9', initial='0',
                 label='First Month Broken Rental',
                 column_label='1.Month BR',
                 help='Broken rental for first month')
        t.index('BillCode', ['Brand', 'FeeModel', 'BillCode', 'PriceList', ('FromDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('FeeModel', ['Brand', 'FeeModel', 'PriceList', 'BillCode', ('FromDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('FMItem')

