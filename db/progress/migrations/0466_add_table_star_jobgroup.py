from gearbox.migrations import Migration

class AddTablejobgroup(Migration):

    database = "star"

    def up(self):
        t = self.table('jobgroup', area="Sta_Data_256", dump_name="jobgroup")
        t.column('jobgroup', 'character', format="X(10)", initial="", max_width=20, label="Job group", position=2, order=10, help="Job group code")
        t.column('description', 'character', format="X(50)", initial="", max_width=100, label="Description", position=3, order=20, help="Description of group")
        t.column('schedulegroup', 'character', format="x(10)", initial="", help="Schedule group code", max_width=20, label="Schedule group", position=4, order=30, description="manually,daily,weekly,monthly")
        t.column('scheduleDay', 'character', format="X(8)", initial="", max_width=16, label="Schedule day", position=5, order=40, help="Schedule day code")
        t.column('launchTime', 'character', format="X(5)", initial="", max_width=10, label="Launch", position=6, order=50, help="Time to launch")
        t.column('lastTime', 'character', format="X(5)", initial="", max_width=10, label="Last", position=7, order=60, help="Last time to lauch")
        t.column('interval', 'integer', format=">>>9", initial="0", max_width=4, label="Interval", position=8, order=70, help="Interval in minutes")
        t.column('queueNumber', 'integer', format=">>9", initial="0", max_width=4, label="Queue number", position=9, order=80, help="0=any queue,1...999 queues")
        t.column('onlyoneQueued', 'logical', format="yes/no", initial="no", max_width=1, label="Only one queued", position=10, order=90, help="Only one queued job")
        t.column('runType', 'character', format="X(8)", initial="", help="Run type", max_width=16, label="Run type", position=11, order=100, description="Periphal / sequental run")
        t.index('jobgroup', [['jobgroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('schedulegroup', [['schedulegroup'], ['scheduleDay']], area="Sta_Index_2")

    def down(self):
        self.drop_table('jobgroup')
