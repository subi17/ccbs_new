from gearbox.migrations import Migration

class AddTableTMRLimit(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TMRLimit', area="Sta_Data_128", label="TM Rule Limit", dump_name="tmrlimit", desc="TM rule limit")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=2, order=40, help="Date when rule becomes effective")
        t.column('TMRuleSeq', 'integer', format=">>>>>>>>9", initial="0", help="Rule ID", max_width=4, label="Rule Sequence", column_label="Seq", position=3, order=20, description='''
''')
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=4, order=50, help="Date when rule expires")
        t.column('LimitID', 'integer', format=">>9", initial="0", max_width=4, label="Limit ID", column_label="ID", position=5, order=60, help="Limit ID")
        t.column('ValueType', 'integer', format=">9", initial="0", max_width=4, label="Value Type", column_label="Type", position=6, order=70, help="Type of limit value")
        t.column('LimitAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Limit Amount", column_label="Amount", position=7, order=80, help="Limit amount")
        t.column('LimitPerc', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="Limit Percent", column_label="Percent", position=8, order=90, help="Limit percent")
        t.column('Action', 'integer', format=">>9", initial="0", max_width=4, label="Action", position=9, order=100, help="Action to be taken when limit is exceeded")
        t.column('SMSText', 'character', format="x(12)", initial="", max_width=24, label="SMS Text", column_label="SMS", position=10, order=110, help="SMS text that is sent when limit is exceeded")
        t.column('MaxValue', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Limit Max", column_label="Max", position=11, order=120, help="Limit maximum value")
        t.column('MinValue', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Limit Min", column_label="Min", position=12, order=130, help="Limit minimum value")
        t.column('ActionParam', 'character', format="x(30)", initial="", max_width=60, label="Action Parameters", column_label="Parameters", position=13, order=140, help="Parameters for the chosen action")
        t.index('LimitID', [['TMRuleSeq'], ['LimitID'], ['ToDate', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('ToDate', [['TMRuleSeq'], ['ToDate', 'DESC'], ['LimitID']], area="Sta_Index_1")

    def down(self):
        self.drop_table('TMRLimit')
