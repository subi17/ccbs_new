from gearbox.migrations import Migration

class AddTableCoBasis(Migration):

    database = "common"

    def up(self):
        t = self.table('CoBasis', area="Sta_Data_128", label="CoBasis", dump_name="cobasis", desc='''Basis for commission rule
''')
        t.column('CoRuleID', 'integer', format=">>>>>9", initial="0", max_width=4, label="Rule ID", column_label="RuleID", position=2, order=10, help="Commission rule ID")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=3, order=20, help="Billing Item code")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=4, order=30, help="Call case number for calls")
        t.column('CoAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Amount", column_label="Amount", position=5, order=40, help="Fixed commission amount")
        t.column('CommLimit', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Commission Limit", column_label="Limit", position=6, order=90, help="Commission is paid only for the amount that exceeds this limit")
        t.column('CoPerc', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Commission %", column_label="Comm. %", position=7, order=50, help="Percentage of billed amount")
        t.column('SubsQty', 'integer', format=">>>>>9", initial="0", max_width=4, label="Subscription Qty", column_label="Subs.Qty", position=8, order=100, help="Subscription qty limit for gaining this percent/fixed amt")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=14, order=41, help="Code Of Brand")
        t.index('BillCode', [['Brand'], ['CoRuleID'], ['BillCode'], ['CCN']], area="Sta_Index_2", primary=True)
        t.index('CCN', [['Brand'], ['CoRuleID'], ['CCN'], ['BillCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CoBasis')
