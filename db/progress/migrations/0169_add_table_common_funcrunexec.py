from gearbox.migrations import Migration

class AddTableFuncRunExec(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunExec', area="Dyn_Data_128", label="Function execution", dump_name="FuncRunExec", desc="Execution of function")
        t.column('FRConfigID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="ID", position=2, order=20, help="ID of the configuration")
        t.column('EndTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="End Time", column_label="End", position=3, order=90, help="Time when run ended")
        t.column('StartTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Start Time", column_label="Start", position=4, order=80, help="Time when run started")
        t.column('RunState', 'character', format="x(16)", initial="", max_width=32, label="Status Of the Run", column_label="Status", position=5, order=100, help="Status of the run")
        t.column('FRExecID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Execution ID", column_label="Exec.", position=6, order=110, help="Unique ID for the execution")
        t.column('FRQScheduleID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Queue Timing ID", column_label="Timing", position=7, order=120, help="Unique ID for this timing row")
        t.column('FRQRowSeq', 'integer', format=">>9", initial="0", max_width=4, label="Queue Order", column_label="Order", position=8, order=130, help="Execution order when belongs to a queue")
        t.column('FRExecSeq', 'integer', format=">>9", initial="0", help="Execution sequence", max_width=4, label="Execution Sequence", column_label="Exec.Seq", position=9, order=140, description="within FuncRunConfig")
        t.column('FRQueueID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Queue ID", column_label="Queue", position=10, order=150, help="Unique ID for the queue")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=11, order=160, help="Code of brand")
        t.column('WaitForExecSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Wait For Execution", column_label="Wait Exec.", position=12, order=170, help="Execution that must be completed before this one can be started")
        t.column('MinStartTime', 'character', format="x(8)", initial="", max_width=16, label="Earliest Start Time", column_label="Min.Start", position=13, order=40, help="Earliest possible start time")
        t.column('FeedFromExecSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Feeds From", column_label="Feeds", position=14, order=180, help="Execution from which feeds are taken from")
        t.column('RunMode', 'character', format="x(12)", initial="", max_width=24, label="Run Mode", column_label="Mode", position=15, order=190, help="Run mode, e.g. production or test")
        t.index('FRExecSeq', [['FRConfigID'], ['FRExecSeq', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('FRExecID', [['FRExecID']], area="Dyn_Index_1", unique=True)
        t.index('FRQScheduleID', [['FRQScheduleID'], ['FRQRowSeq']], area="Dyn_Index_1")
        t.index('RunState', [['Brand'], ['RunState']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('FuncRunExec')
