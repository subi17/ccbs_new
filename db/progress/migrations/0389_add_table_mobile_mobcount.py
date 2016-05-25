from gearbox.migrations import Migration

class AddTableMobCount(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MobCount', area="Sta_Data_128", dump_name="mobcount")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('CountType', 'character', format="x(8)", initial="", max_width=16, label="MC-Type", column_label="MC-Type", position=3, order=20, help="Mobile Counter Type")
        t.column('Balance', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Balance", column_label="Balance", position=4, order=30, help="Current unbilled balance")
        t.column('QtyLimit', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Limit", column_label="Limit", position=5, order=40, help="Mobile Counter alarm limit")
        t.column('CallQty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Count", column_label="Count", position=6, order=50, help="Amount of unbilled calls")
        t.column('Duration', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Sec.", column_label="Sec.", position=7, order=60, help="Seconds of unbilled calls")
        t.column('CreditLimit', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Call Limit", column_label="Call Limit", position=8, order=70, help="Mobile Counter alarm limit")
        t.index('MsSeq', [['MsSeq'], ['CountType']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('MobCount')
