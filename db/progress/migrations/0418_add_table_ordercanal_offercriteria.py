from gearbox.migrations import Migration

class AddTableOfferCriteria(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OfferCriteria', area="Sta_Data_128", label="Offer Criteria", dump_name="offercriteria", desc="Offer criteria")
        t.column('CriteriaType', 'character', format="x(20)", initial="", max_width=40, label="Criteria Type", column_label="Type", position=3, order=20, help="Criteria type")
        t.column('IncludedValue', 'character', format="x(30)", initial="", max_width=60, label="Included Value", column_label="Included", position=4, order=30, help="Values that are included in this offer")
        t.column('BeginStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid From", column_label="From", position=5, order=40, help="Valid from")
        t.column('EndStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid To", column_label="To", position=6, order=50, help="Valid to")
        t.column('ExcludedValue', 'character', format="x(30)", initial="", max_width=60, label="Excluded Value", column_label="Excluded", position=7, order=60, help="Values that are excluded from this offer")
        t.column('Offer', 'character', format="x(12)", initial="", max_width=24, label="Offer ID", column_label="Offer", position=9, order=10, help="Offer ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=10, order=70, help="Code of brand")
        t.column('OfferCriteriaID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Criteria ID", column_label="ID", position=11, order=80, help="Offer criteria ID")
        t.index('Criteria', [['Brand'], ['Offer'], ['OfferCriteriaID']], area="Sta_Index_2", primary=True)
        t.index('CriteriaType', [['Brand'], ['CriteriaType'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('EndStamp', [['Brand'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('Offer', [['Brand'], ['Offer'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('OfferCriteriaID', [['OfferCriteriaID']], area="Sta_Index_2", unique=True)
        t.index('OfferCriteriaType', [['Brand'], ['Offer'], ['CriteriaType'], ['BeginStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('OfferCriteria')
