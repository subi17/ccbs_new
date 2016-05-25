from gearbox.migrations import Migration

class AddTableTMRItemValue(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TMRItemValue', area="Sta_Data_256", label="TM Rule Item Values", dump_name="tmritemvalue", desc='''TM rule item values
''')
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=2, order=40, help="Date when rule becomes effective")
        t.column('TMRuleSeq', 'integer', format=">>>>>>>>9", initial="0", help="Rule ID", max_width=4, label="Rule Sequence", column_label="Seq", position=3, order=20, description='''
''')
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=4, order=50, help="Date when rule expires")
        t.column('CounterItemValues', 'character', format="x(40)", initial="", max_width=80, label="Item Values", column_label="Values", position=5, order=60, help="List of item values used to collect the counter")
        t.index('CounterItemValues', [['TMRuleSeq'], ['CounterItemValues'], ['ToDate', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('ToDate', [['TMRuleSeq'], ['ToDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('TMRItemValue')
