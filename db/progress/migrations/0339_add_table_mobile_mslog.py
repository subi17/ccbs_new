from gearbox.migrations import Migration

class AddTableMSLog(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSLog', area="Sta_Data_128_2", label="MS Log", dump_name="mslog", desc="Log for mobile subscription events")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MobSub Sequence", column_label="SubSeq", position=2, order=10, help="Sequence for a subscription")
        t.column('LogType', 'character', format="x(8)", initial="", help="Type of log event", max_width=16, label="Log Type", column_label="Type", position=3, order=20, description='''


''')
        t.column('LogStatus', 'integer', format="9", initial="0", help="Status of event", max_width=4, label="Status", column_label="Stat", position=5, order=40, description='''

''')
        t.column('EventValue', 'character', format="x(30)", initial="", help="Value of event", max_width=60, label="Event Value", column_label="Value", position=6, order=50, description='''

''')
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User ID", column_label="User", position=7, order=60, help="TMS user who processed the event")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('EventStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Time stamp of event", max_width=17, label="Time Stamp Of Event", column_label="Time", position=9, order=80, description='''

''')
        t.index('MsSeq', [['MsSeq'], ['LogType'], ['EventStamp', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('EventStamp', [['Brand'], ['EventStamp', 'DESC']], area="Sta_Index_2")
        t.index('LogType', [['Brand'], ['LogType'], ['EventStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MSLog')
