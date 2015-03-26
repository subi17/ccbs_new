from gearbox.migrations import Migration

class AddOfferItem(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OfferItem', area='Sta_Data_128',
                       label='Offer Item',
                       dump_name='offeritem',
                       desc='Offer item')
        t.column('Offer', 'character', format='x(12)', initial='',
                 label='Offer ID',
                 column_label='Offer')
        t.column('ItemType', 'character', format='x(16)', initial='',
                 label='Item Type',
                 column_label='Type',
                 help='Item type')
        t.column('ItemKey', 'character', format='x(20)', initial='',
                 label='Item Key',
                 column_label='Key',
                 help='Item key')
        t.column('BeginStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('EndStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Offer Amount',
                 column_label='Amount',
                 help='Offer amount')
        t.column('DispInUI', 'logical', initial='yes',
                 label='Display In UI',
                 column_label='Display',
                 help='Display item in UI')
        t.column('DispOnInvoice', 'logical', initial='yes',
                 label='Display On Invoice',
                 column_label='Disp.Inv',
                 help='Display item on invoice')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='no',
                 label='Tax Included',
                 column_label='Tax',
                 help='Tax included in amount')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('OfferItemID', 'integer', format='>>>>>>>>9', initial='0',
                 label='Item ID',
                 column_label='ID',
                 help='Offer item ID')
        t.index('EndStamp', ['Brand', 'Offer', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('Item', ['Brand', 'Offer', 'OfferItemID'], area='Sta_Index_2',
                primary=True)
        t.index('ItemKey', ['Brand', 'ItemType', 'ItemKey'], area='Sta_Index_2')
        t.index('ItemType', ['Brand', 'Offer', 'ItemType', 'ItemKey', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('OfferItemID', ['OfferItemID'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('OfferItem')

