from gearbox.migrations import Migration

class AddTableTMRule(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TMRule', area="Sta_Data_128", label="TM Rule", dump_name="tmrule", desc="Ticket management rule")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=3, order=40, help="Date when rule becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=4, order=50, help="Date when rule expires")
        t.column('TMRuleSeq', 'integer', format=">>>>>>>>9", initial="0", help="Rule ID", max_width=4, label="Rule Sequence", column_label="Seq", position=5, order=20, description='''
''')
        t.column('CounterItems', 'character', format="x(40)", initial="", max_width=80, label="Counter Items", column_label="Items", position=6, order=60, help="Items that are used in counter collection")
        t.column('CounterAmount', 'character', format="x(15)", initial="", max_width=30, label="Counter Amount", column_label="Amount", position=7, order=70, help="Amount that is collected to counter")
        t.column('CounterType', 'integer', format=">9", initial="0", max_width=4, label="Counter Type", column_label="Type", position=8, order=80, help="Type of counter")
        t.column('CounterPeriod', 'integer', format=">9", initial="0", max_width=4, label="Counter Period", column_label="Period", position=9, order=90, help="Period of counter")
        t.column('Name', 'character', format="x(40)", initial="", max_width=80, label="Name", position=10, order=30, help="Name")
        t.column('NewCustomer', 'logical', format="yes/no", initial="yes", max_width=1, label="New Customers", column_label="New Cust", position=11, order=100, help="Copy rule automatically to new customers")
        t.column('RuleActive', 'integer', format="9", initial="0", max_width=4, label="Active", position=12, order=110, help="Active status")
        t.column('LimitSource', 'integer', format=">9", initial="1", max_width=4, label="Limit Source", column_label="LimitSrc", position=13, order=120, help="Which limit is used in analysis")
        t.column('PayType', 'integer', format="9", initial="0", max_width=4, label="Payment Type", column_label="PayType", position=14, order=130, help="Payment type")
        t.column('LimitCompare', 'integer', format="9", initial="1", max_width=4, label="Limit Comparison", column_label="Compare", position=15, order=140, help="Limit comparison method")
        t.column('TicketType', 'integer', format=">9", initial="0", max_width=4, label="Ticket Type", column_label="TicketType", position=16, order=150, help="Type of tickets/cdrs")
        t.index('TMRuleSeq', [['TMRuleSeq']], area="Sta_Index_1", primary=True, unique=True)
        t.index('CounterType', [['Brand'], ['CounterType'], ['TMRuleSeq']], area="Sta_Index_1")
        t.index('Name', [['Brand'], ['Name']], area="Sta_Index_1")
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('TMRule')
