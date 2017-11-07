from gearbox.migrations import Migration

class AddTableMsALog(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MsALog', area="Sta_Data_256", dump_name="msalog", desc='''


''')
        t.column('MsALog', 'integer', mandatory=True, format=">>>>>>9", initial="0", max_width=4, label="Sa-seq", column_label="Sa-seq", position=2, order=10, help="Sequence of Send Activation LOG")
        t.column('FirstName', 'character', format="x(20)", initial="", max_width=40, label="Filename", column_label="Filename", position=3, order=20, help="Filename")
        t.column('SaAts', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Activated", column_label="Activated", position=4, order=30, help="Activate time")
        t.column('SaSts', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Sent", column_label="Sent", position=5, order=40, help="Sending time")
        t.column('SaRts', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Received", column_label="Received", position=6, order=50, help="When File received from Operator")
        t.column('Memo', 'character', format="x(70)", initial="", max_width=2130, label="Memo", column_label="Memo", extent=15, position=7, order=60, help="Memo text")
        t.column('SaStatus', 'integer', format="9", initial="0", max_width=4, label="Status", column_label="Status", position=8, order=70, help="Status of File")
        t.column('SaQty', 'integer', format=">>>>9", initial="0", max_width=48, label="Amount", column_label="Amount", extent=4, position=9, order=80, help="Amount of Mobile Subscription")
        t.index('MsALog', [['MsALog']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SaStatus', [['SaStatus'], ['SaAts'], ['MsALog']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('MsALog')
