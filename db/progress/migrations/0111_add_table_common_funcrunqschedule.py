from gearbox.migrations import Migration

class AddTableFuncRunQSchedule(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunQSchedule', area="Sta_Data_128", label="Function Queue Timing", dump_name="FuncRunQSchedule", desc="Timetable for function queue")
        t.column('FRQueueID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Queue ID", column_label="Queue", position=2, order=10, help="Unique ID for the queue")
        t.column('StartTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Start Time", column_label="Start", position=3, order=20, help="Time when run will be started")
        t.column('DoneTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Start Time", column_label="Start", position=4, order=30, help="Time when run will be started")
        t.column('FRQScheduleID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Queue Timing ID", column_label="Timing", position=6, order=50, help="Unique ID for this timing row")
        t.column('RunMode', 'character', format="x(12)", initial="", max_width=24, label="Run Mode", column_label="Mode", position=7, order=60, help="Run mode, e.g. production or test")
        t.column('RunState', 'character', format="x(16)", initial="", max_width=32, label="Run Status", column_label="Status", position=8, order=40, help="Status of the scheduled run")
        t.index('FRQScheduleID', [['FRQScheduleID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('FRQueueID', [['FRQueueID'], ['StartTS', 'DESC']], area="Sta_Index_1")
        t.index('RunState', [['FRQueueID'], ['RunState'], ['StartTS', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FuncRunQSchedule')
