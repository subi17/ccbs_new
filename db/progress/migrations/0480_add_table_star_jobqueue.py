from gearbox.migrations import Migration

class AddTableJobQueue(Migration):

    database = "star"

    def up(self):
        t = self.table('JobQueue', area="Sta_Data_256", label="Job Queue", dump_name="jobqueue", desc="Job Queue")
        t.column('JobNum', 'integer', format=">>>,>>>,>>9", initial="0", max_width=4, column_label="Job Id", position=2, order=10, help="Job Id. Number which identify job")
        t.column('QueueDate', 'date', format="99.99.99", max_width=4, label="Queued Date", position=3, order=60, help="Date when put to queue")
        t.column('QueueTime', 'character', format="X(8)", initial="", max_width=16, label="Queued Time", position=4, order=70, help="Time when put to queue")
        t.column('RunDate', 'date', format="99.99.99", max_width=4, label="Run Date", position=5, order=40, help="Date when launch")
        t.column('RunTime', 'character', format="X(8)", initial="", max_width=16, label="Run Time", position=6, order=50, help="Time when launch")
        t.column('Priority', 'integer', format=">9", initial="90", max_width=4, label="Priority", position=7, order=30, help="Queue priority 0 = faster.. 99=slower.. 90=default")
        t.column('JobState', 'integer', format="9", initial="0", help="Job status", max_width=4, label="Status", position=8, order=20, description='''0 = queue
5 = running
6 = aborted
7 = error
9 = done''')
        t.column('ProgramCode', 'character', format="X(12)", initial="", max_width=24, label="ProgramCode", position=9, order=80, help="ProgramCode")
        t.column('UserCode', 'character', format="X(8)", initial="", max_width=16, label="UserCode", position=10, order=90, help="Job owner")
        t.column('UserName', 'character', format="X(50)", initial="", max_width=100, label="User name", position=11, order=100, help="Job owner real name")
        t.column('InputData', 'character', format="X(256)", initial="", max_width=512, label="Input Data", position=15, order=110, help="starEntry list")
        t.column('CommitDate', 'date', format="99.99.99", max_width=4, label="Commit Date", position=16, order=120, help="Date when job is commited")
        t.column('CommitTime', 'character', format="X(8)", initial="", max_width=16, label="Commit time", position=17, order=130, help="Time when job is commited")
        t.column('TotalTime', 'character', format="X(10)", initial="", max_width=20, label="Total time", position=18, order=140, help="Total time. Format ""HHHH:MM:SS""")
        t.column('queueNumber', 'integer', format=">>9", initial="0", max_width=4, label="Queue number", position=25, order=150, help="0=workstation,1...999 queues")
        t.column('jobgroup', 'character', format="X(10)", initial="", max_width=20, label="Job group", position=26, order=160, help="Job group code")
        t.column('number', 'integer', format=">9", initial="0", max_width=4, label="Number", position=27, order=170, help="Job groups sequence number")
        t.column('archive', 'character', format="X(10)", initial="Default", help="Archive time", max_width=20, label="Archive", position=28, order=180, description='''Default
Year
Month
Week
Permanent''')
        t.column('logfile', 'character', format="X(78)", initial="", help="Log File information", max_width=156, label="Log File", position=29, order=190, description="max 16384 characters")
        t.index('JobNum', [['JobNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('archive', [['archive'], ['CommitDate']], area="Sta_Index_2")
        t.index('JobGroup', [['jobgroup'], ['number'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('JobState', [['JobState'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('Priority', [['JobState'], ['Priority'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('programCode', [['ProgramCode'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('Queue', [['queueNumber'], ['JobState'], ['Priority'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('UserCode', [['UserCode'], ['JobState'], ['QueueDate'], ['QueueTime']], area="Sta_Index_2")
        t.index('UserCode_ProgramCode', [['UserCode'], ['ProgramCode'], ['QueueDate', 'DESC'], ['QueueTime', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('JobQueue')
