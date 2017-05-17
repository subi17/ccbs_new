from gearbox.migrations import Migration

class AddTableSimDeliveryHist(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SimDeliveryHist', area="Sta_Index_1", dump_name="simdeliv")
        t.column('OrderID', 'integer', format=">>>>>>>>9", initial="0", help="Order ID", max_width=4, label="OrderID", column_label="OrderID", position=2, order=10, description='''

''')
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", help="Sequence for a subscription", max_width=4, label="MobSub Sequence", column_label="SubSeq", position=3, order=20, description='''

''')
        t.column('StatusCode', 'integer', format="z9", initial="0", help="Status Code", max_width=4, label="Status", column_label="St", position=4, order=30, description='''

''')
        t.column('TimeStamp', 'decimal', format="99999999.99999", decimals=2, initial="0", help="Activation Timestamp", max_width=17, label="TimeStamp", column_label="TimeStamp", position=5, order=40, description='''

''')
        t.column('CancelCode', 'integer', format="z9", initial="0", help="Status Code", max_width=4, label="Status", column_label="St", position=6, order=50, description='''

''')
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="Memo", column_label="Memo", position=7, order=60, help="Memo")
        t.index('MSSeq', [['MsSeq'], ['TimeStamp', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('OrderID', [['OrderID'], ['TimeStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('SimDeliveryHist')
