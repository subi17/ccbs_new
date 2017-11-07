from gearbox.migrations import Migration

class AddTableRequestStatus(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestStatus', area="Sta_Data_64", label="Status configuration", dump_name="requeststatus", desc="Status configuration for requests")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('InUse', 'logical', format="Yes/No", initial="yes", max_width=1, label="Used", position=3, order=60, help="Request status is handled")
        t.column('Program', 'character', format="x(30)", initial="", max_width=60, label="Program", position=4, order=40, help="Program that contains logic for this status")
        t.column('ReqType', 'integer', format=">>9", initial="0", max_width=4, label="Request Type", column_label="Type", position=5, order=20, description="Request type")
        t.column('ReqStatus', 'integer', format=">>9", initial="0", max_width=4, label="Request Status", column_label="Status", position=6, order=30, help="Request status")
        t.column('LogClear', 'logical', format="Yes/No", initial="no", max_width=1, label="Clear Log", position=7, order=120, help="Clear log on each round")
        t.column('LogEntry', 'character', format="x(20)", initial="", max_width=40, label="Log Entry", position=8, order=80, help="Log entry")
        t.column('LogFile', 'character', format="x(40)", initial="", max_width=80, label="Log File", column_label="File", position=9, order=90, help="Log file")
        t.column('LogOn', 'logical', format="Yes/No", initial="no", max_width=1, label="Logging On", column_label="Logging", position=10, order=110, help="Logging on")
        t.column('LogThreshold', 'decimal', format=">>>>>>>>>9", decimals=0, initial="0", max_width=15, label="Log Threshold", column_label="Threshold", position=11, order=100, help="Log threshold")
        t.index('ReqStatus', [['Brand'], ['ReqType'], ['ReqStatus']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('RequestStatus')
