from gearbox.migrations import Migration

class AddTablePListConf(Migration):

    database = "common"

    def up(self):
        t = self.table('PListConf', area="Sta_Data_128", dump_name="plistcon", desc='''Price configuration

''')
        t.column('dFrom', 'date', format="99-99-99", max_width=4, label="DateFrom", column_label="DateFrom", position=3, order=20, help="FROM date for price list validation")
        t.column('dTo', 'date', format="99-99-99", max_width=4, label="DateTo", column_label="DateTo", position=4, order=30, help="TO date for price list validation")
        t.column('Prior', 'integer', format=">9", initial="0", max_width=4, label="Priority", column_label="Priority", position=5, order=40, help="Priority for simultaneous price list")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Plist", column_label="Plist", position=6, order=50, help="Code (identifier) for a Price List")
        t.column('RatePlan', 'character', format="x(8)", initial="", max_width=16, label="Rating Plan", column_label="RatePlan", position=8, order=70, help="Rating plan code")
        t.column('StartCharge', 'logical', format="A/P", initial="Yes", max_width=1, label="Start charge", column_label="Start charge", position=9, order=80, help="Allow / Prohibit starting charges (A/P)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.index('RatePlan', [['Brand'], ['RatePlan'], ['dFrom', 'DESC'], ['dTo', 'DESC'], ['Prior']], area="Sta_Index_2", primary=True)
        t.index('Browse', [['Brand'], ['RatePlan'], ['dFrom'], ['dTo'], ['Prior']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PListConf')
