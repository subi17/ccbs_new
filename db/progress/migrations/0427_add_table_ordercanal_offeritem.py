from gearbox.migrations import Migration

class AddTableOfferItem(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OfferItem', area="Sta_Data_128", label="Offer Item", dump_name="offeritem", desc="Offer item")
        t.column('ItemType', 'character', format="x(16)", initial="", max_width=32, label="Item Type", column_label="Type", position=3, order=20, help="Item type")
        t.column('ItemKey', 'character', format="x(20)", initial="", max_width=40, label="Item Key", column_label="Key", position=4, order=30, help="Item key")
        t.column('BeginStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid From", column_label="From", position=5, order=40, help="Valid from")
        t.column('EndStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid To", column_label="To", position=6, order=50, help="Valid to")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Offer Amount", column_label="Amount", position=7, order=60, help="Offer amount")
        t.column('DispInUI', 'logical', format="Yes/No", initial="yes", max_width=1, label="Display In UI", column_label="Display", position=8, order=70, help="Display item in UI")
        t.column('DispOnInvoice', 'logical', format="Yes/No", initial="yes", max_width=1, label="Display On Invoice", column_label="Disp.Inv", position=9, order=80, help="Display item on invoice")
        t.column('VatIncl', 'logical', format="Included/Excluded", initial="no", max_width=1, label="Tax Included", column_label="Tax", position=10, order=90, help="Tax included in amount")
        t.column('Offer', 'character', format="x(12)", initial="", max_width=24, label="Offer ID", column_label="Offer", position=11, order=10, help="Offer ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=12, order=100, help="Code of brand")
        t.column('OfferItemID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Item ID", column_label="ID", position=13, order=110, help="Offer item ID")
        t.column('Periods', 'integer', format=">>9", initial="0", max_width=4, label="Periods", column_label="Periods", position=14, order=120)
        t.index('Item', [['Brand'], ['Offer'], ['OfferItemID']], area="Sta_Index_2", primary=True)
        t.index('EndStamp', [['Brand'], ['Offer'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('ItemKey', [['Brand'], ['ItemType'], ['ItemKey']], area="Sta_Index_2")
        t.index('ItemType', [['Brand'], ['Offer'], ['ItemType'], ['ItemKey'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('OfferItemID', [['OfferItemID']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('OfferItem')
