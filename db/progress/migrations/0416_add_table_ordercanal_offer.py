from gearbox.migrations import Migration

class AddTableOffer(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('Offer', area="Sta_Data_64", label="Offer", dump_name="offer", desc="Offer header")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('Description', 'character', format="x(40)", initial="", max_width=80, label="Description", position=5, order=40, help="Description of offer")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=6, order=50, help="Date when offer becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=7, order=60, help="Date when offer expires")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=11, order=160, help="Is offer active")
        t.column('Offer', 'character', format="x(12)", initial="", max_width=24, label="Offer ID", column_label="Offer", position=12, order=20, help="Offer ID")
        t.column('OfferAmount', 'decimal', format="->>>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Offer Amount", column_label="Amount", position=13, order=120, help="Total price for the offer")
        t.column('VatIncl', 'logical', format="Included/Excluded", initial="no", max_width=1, label="Tax Included", column_label="Tax Incl.", position=14, order=130, help="Tax included in amount")
        t.column('DispItemAmounts', 'integer', format="9", initial="0", max_width=4, label="Disp Item Amounts", column_label="Item Amt", position=15, order=140, help="Display separate item amounts")
        t.column('Priority', 'integer', format=">>>9", initial="0", max_width=4, label="Priority", position=16, order=150, help="Priority in contrast to other offers")
        t.index('Offer', [['Brand'], ['Offer']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Active', [['Brand'], ['Active'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Offer')
