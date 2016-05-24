from gearbox.migrations import Migration

class AddTableLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('Limit', area="Sta_Data_256", label="Limit", table_trigger=[{'crc': '?', 'procedure': 'rd-limit.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-limit.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="limit", desc="Limits for customer and subscription")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=2, order=10, help="Customer number")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=3, order=20, help="Mobile subscription ID")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=4, order=50, help="Date when rule becomes effective")
        t.column('LimitAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Limit Amount", column_label="Amount", position=5, order=70, help="Limit amount")
        t.column('LimitID', 'integer', format=">>9", initial="0", max_width=4, label="Limit ID", column_label="ID", position=6, order=40, help="Limit ID")
        t.column('LimitPerc', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="Limit Percent", column_label="Percent", position=7, order=80, help="Limit percent")
        t.column('TMRuleSeq', 'integer', format=">>>>>>>>9", initial="0", help="Rule ID", max_width=4, label="Rule Sequence", column_label="Seq", position=8, order=30, description='''
''')
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=9, order=60, help="Date when rule expires")
        t.column('LimitType', 'integer', format=">9", initial="0", max_width=4, label="Limit Type", column_label="Type", position=10, order=90, help="Type of limit")
        t.column('ValueType', 'integer', format=">9", initial="0", max_width=4, label="Value Type", column_label="Type", position=11, order=100, help="Type of limit value")
        t.column('DefValue', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Default Value", column_label="Default", position=12, order=110, help="Limit has the default value")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=13, order=120, help="Code Of Brand")
        t.index('CustNum', [['CustNum'], ['LimitType'], ['TMRuleSeq'], ['LimitID'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('LimitType', [['Brand'], ['LimitType'], ['LimitID'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('MsSeq', [['MsSeq'], ['LimitType'], ['TMRuleSeq'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('TMRuleSeq', [['TMRuleSeq'], ['LimitID'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Limit')
