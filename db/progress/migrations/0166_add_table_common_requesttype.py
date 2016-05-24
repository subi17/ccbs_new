from gearbox.migrations import Migration

class AddTableRequestType(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestType', area="Sta_Data_64", label="Request configuration", table_trigger=[{'crc': '?', 'procedure': 'rd-requesttype.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-requesttype.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="requesttype", desc="Request type configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('ReqType', 'integer', format=">>9", initial="0", max_width=4, label="Request Type", column_label="Type", position=3, order=20, description="Request type")
        t.column('ReqName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=4, order=30, help="Description of request type")
        t.column('Program', 'character', format="x(30)", initial="", max_width=60, label="Program", position=5, order=40, help="Program that contains logic for this type")
        t.column('UserCode', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="User ID", position=6, order=50, help="User ID for logs")
        t.column('InUse', 'logical', format="Yes/No", initial="yes", max_width=1, label="Used", position=7, order=60, help="Request type is handled")
        t.column('Queue', 'integer', format=">>>9", initial="0", max_width=4, label="Queue Nbr", column_label="Queue", position=8, order=70, help="Queue in which this type is handled")
        t.column('LogEntry', 'character', format="x(20)", initial="", max_width=40, label="Log Entry", position=9, order=80, help="Log entry")
        t.column('LogFile', 'character', format="x(40)", initial="", max_width=80, label="Log File", column_label="File", position=10, order=90, help="Log file")
        t.column('LogThreshold', 'decimal', format=">>>>>>>>>9", decimals=0, initial="0", max_width=15, label="Log Threshold", column_label="Threshold", position=11, order=100, help="Log threshold")
        t.column('LogOn', 'logical', format="Yes/No", initial="no", max_width=1, label="Logging On", column_label="Logging", position=12, order=110, help="Logging on")
        t.column('LogClear', 'logical', format="Yes/No", initial="no", max_width=1, label="Clear Log", position=13, order=120, help="Clear log on each round")
        t.column('Mode', 'character', format="x(8)", initial="", max_width=16, label="Mode", column_label="Mode", position=14, order=130, help="Mode for executing requests")
        t.index('ReqType', [['Brand'], ['ReqType']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Queue', [['Brand'], ['Queue'], ['ReqType']], area="Sta_Index_2")

    def down(self):
        self.drop_table('RequestType')
