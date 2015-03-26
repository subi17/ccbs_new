from gearbox.migrations import Migration

class AddTopupSchemeRow(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('TopupSchemeRow', area='Sta_Data_128',
                       label='Topup Scheme Row',
                       dump_name='topupschemerow',
                       desc='Topup scheme row')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('TopupScheme', 'character', format='x(12)', initial='',
                 label='Topup Scheme',
                 column_label='Scheme ID',
                 help='Topup scheme ID')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Topup Amount',
                 column_label='Amount',
                 help='Topup amount')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='Bill.Item',
                 help='Billing item')
        t.column('DiscountAmount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Discount Amount',
                 column_label='Discount',
                 help='Discount amount')
        t.column('DiscountBillCode', 'character', format='x(16)', initial='',
                 label='Discount Billing Item',
                 column_label='Discount B.Item',
                 help='Discount billing item')
        t.column('BeginStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('EndStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('TopupSchemeRowID', 'integer', format='>>>>>>>>9', initial='0',
                 label='Row ID',
                 column_label='ID')
        t.index('EndStamp', ['Brand', 'TopupScheme', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('TopupSchemeRow', ['Brand', 'TopupScheme', 'TopupSchemeRowID'], area='Sta_Index_2',
                primary=True)
        t.index('TopupSchemeRowID', ['TopupSchemeRowID'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('TopupSchemeRow')

