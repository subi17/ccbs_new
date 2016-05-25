from gearbox.migrations import Migration

class AddTableCronLog(Migration):

    database = "star"

    def up(self):
        t = self.table('CronLog', area="Sta_Data_256", label="CronLog", dump_name="cronlog", desc='''Log file for cron based processes
''')
        t.column('Date', 'date', format="99-99-99", max_width=4, label="Date", position=2, order=10, help="Date when the run started")
        t.column('LogTime', 'integer', format=">>>>9", initial="0", max_width=4, label="Time", column_label="Time", position=3, order=20, help="Time of day when run started")
        t.column('DateFrom', 'date', format="99-99-99", max_width=4, label="From", column_label="From", position=4, order=30, help="From which date erasing started")
        t.column('DateTo', 'date', format="99-99-99", max_width=4, label="To", column_label="To", position=5, order=40, help="To what date erasing was run")
        t.column('Amt', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="Amount", column_label="Amount", position=6, order=50, help="Amount of erased calls")
        t.column('State', 'character', format="x(12)", initial="", max_width=24, label="State", column_label="State", position=7, order=60, help="Status of the run: RUNNING - ENDED")
        t.index('Date', [['Date', 'DESC'], ['LogTime', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DateFrom', [['DateFrom'], ['DateTo']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CronLog')
