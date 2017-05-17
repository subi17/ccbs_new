from gearbox.migrations import Migration

class AddTableTopupScheme(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TopupScheme', area="Sta_Data_64", label="Topup Scheme", dump_name="topupscheme", desc="Topup scheme")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('Description', 'character', format="x(40)", initial="", max_width=80, label="Description", position=4, order=30, help="Description of topup scheme")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Date when scheme becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=6, order=50, help="Date when scheme expires")
        t.column('TopupSource', 'character', format="x(12)", initial="", max_width=24, label="Topup Source", column_label="Source", position=7, order=60, help="Source of topup")
        t.column('PPReqPrefix', 'character', format="x(8)", initial="", max_width=16, label="Topup Prefix", column_label="Prefix", position=8, order=70, help="Topup prefix")
        t.column('VatIncl', 'logical', format="Included/Excluded", initial="no", max_width=1, label="Tax Included", column_label="Tax Incl.", position=10, order=90, help="Tax included in amount")
        t.column('TopupScheme', 'character', format="x(12)", initial="", max_width=24, label="Topup Scheme", column_label="Scheme ID", position=14, order=20, help="Topup scheme ID")
        t.index('ToupScheme', [['Brand'], ['TopupScheme']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Source', [['Brand'], ['TopupSource'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TopupScheme')
