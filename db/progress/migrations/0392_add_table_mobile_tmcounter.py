from gearbox.migrations import Migration

class AddTableTMCounter(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TMCounter', area="Sta_Data_256", label="TM Counter", dump_name="tmcounter", desc="TM counter")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=2, order=20, help="Customer number")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=3, order=50, help="Date when rule becomes effective")
        t.column('LimitID', 'integer', format=">>9", initial="0", max_width=4, label="Limit ID", column_label="ID", position=4, order=40, help="Limit that has been exceeded")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=5, order=30, help="Mobile subscription ID")
        t.column('TMRuleSeq', 'integer', format=">>>>>>>>9", initial="0", help="Rule ID", max_width=4, label="Rule Sequence", column_label="Seq", position=6, order=10, description='''
''')
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=7, order=60, help="Date when rule expires")
        t.column('Amount', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", help="Amount", max_width=20, label="Amount", column_label="Amt", position=8, order=70, description='''

''')
        t.column('LimitAmt', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Amount", column_label="LimitAmt", position=9, order=80, help="Limit Amount")
        t.column('IntValue', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="IntValue", column_label="IntValue", position=10, order=90, help="General integer value field")
        t.column('DecValue', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=20, label="DecValue", column_label="DecValue", position=11, order=100, help="General decimal value field")
        t.index('MsSeq', [['MsSeq'], ['TMRuleSeq'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('CustNum', [['CustNum'], ['TMRuleSeq'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TMCounter')
