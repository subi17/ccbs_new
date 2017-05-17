from gearbox.migrations import Migration

class AddTableSOLog(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SOLog', area="Sta_Data_32", label="Service Order Log", dump_name="solog", desc='''
''')
        t.column('SoLog', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Order Seq", column_label="Order Seq", position=2, order=10, help="Sequence for a Service Order")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=3, order=20, help="Sequence for a Subscription")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Created", column_label="Created", position=4, order=30, help="Time Stamp: When Created")
        t.column('ActivationTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Activated", column_label="Activated", position=5, order=40, help="Time Stamp: Intended Activation")
        t.column('CompletedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Completed", column_label="Completed", position=6, order=50, help="Time Stamp: Service order Succesfully Completed")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=7, order=60, help="MSISDN Subscriber No")
        t.column('Stat', 'integer', format="9", initial="0", max_width=4, label="Status", column_label="St", position=8, order=70, help="Status Code 0 ... 9")
        t.column('CommLine', 'character', format="x(255)", initial="", max_width=510, label="Command Line", column_label="Command Line", position=9, order=80, help="Service Order Command Line as sent via SOG GWY")
        t.column('Response', 'character', format="x(70)", initial="", max_width=140, label="Response", column_label="Response", position=10, order=90, help="Response/error message")
        t.column('MsALog', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Activation Seq", column_label="Act. Seq", position=11, order=101, help="Sequence of Send Activation LOG")
        t.column('TimeSlotTMS', 'decimal', format="99999999.99999", decimals=6, initial="0", max_width=21, label="ActTime", column_label="ActTime", position=12, order=100, help="Time Slot for activation")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=13, order=111, help="Code Of Brand")
        t.column('users', 'character', format="x(12)", initial="", max_width=24, label="Users", column_label="Users", position=14, order=121, help="Who made this Service order log req")
        t.column('MsRequest', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Request ID", column_label="Request", position=15, order=131, help="Unique ID for request")
        t.index('MsSeq', [['MsSeq'], ['Stat'], ['ActivationTS']], area="Sta_Index_2", primary=True)
        t.index('CLI', [['Brand'], ['CLI'], ['Stat'], ['ActivationTS']], area="Sta_Index_2")
        t.index('CLI_s', [['CLI'], ['Stat'], ['ActivationTS']], area="Sta_Index_2")
        t.index('SoLog', [['Brand'], ['SoLog', 'DESC']], area="Sta_Index_2")
        t.index('Solog_s', [['SoLog']], area="Sta_Index_2")
        t.index('Stat', [['Brand'], ['Stat'], ['ActivationTS'], ['SoLog']], area="Sta_Index_2")
        t.index('TimeSlotTMS', [['Brand'], ['Stat'], ['TimeSlotTMS'], ['SoLog']], area="Sta_Index_2")

    def down(self):
        self.drop_table('SOLog')
