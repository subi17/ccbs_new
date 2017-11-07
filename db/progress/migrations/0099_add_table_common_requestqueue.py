from gearbox.migrations import Migration

class AddTableRequestQueue(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestQueue', area="Sta_Data_64", label="Request Queue", dump_name="requestqueue", desc="Request queue")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('InUse', 'logical', format="Yes/No", initial="yes", max_width=1, label="Used", position=3, order=40, help="Request type is handled")
        t.column('Queue', 'integer', format=">>>9", initial="0", max_width=4, label="Queue Nbr", column_label="Queue", position=4, order=20, help="Queue in which this type is handled")
        t.column('QName', 'character', format="x(30)", initial="", max_width=60, label="Description", column_label="Name", position=5, order=30, help="Queue description")
        t.column('Interval', 'integer', format=">>>>>9", initial="0", max_width=4, label="Interval", position=6, order=50, help="Interval in seconds")
        t.column('Monitor', 'character', format="x(40)", initial="", max_width=80, label="Monitor Command", column_label="Monitor", position=7, order=60, help="Command for monitoring")
        t.column('LogEntry', 'character', format="x(20)", initial="", max_width=40, label="Log Entry", position=8, order=70, help="Log entry")
        t.column('LogFile', 'character', format="x(40)", initial="", max_width=80, label="Log File", column_label="File", position=9, order=80, help="Log file")
        t.column('LogThreshold', 'decimal', format=">>>>>>>>>9", decimals=0, initial="0", max_width=15, label="Log Threshold", column_label="Threshold", position=10, order=90, help="Log threshold")
        t.column('LogOn', 'logical', format="Yes/No", initial="no", max_width=1, label="Logging On", column_label="Logging", position=11, order=100, help="Logging on")
        t.column('MonitorOn', 'logical', format="Yes/No", initial="no", max_width=1, label="Monitoring On", position=12, order=110, help="Monitoring on")
        t.index('Queue', [['Brand'], ['Queue']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('RequestQueue')
