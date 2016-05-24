from gearbox.migrations import Migration

class AddTableprogjob(Migration):

    database = "star"

    def up(self):
        t = self.table('progjob', area="Sta_Data_256", label="Program jobs", dump_name="progjob")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=2, order=30, help="Programcode")
        t.column('job', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Job", position=3, order=10, help="Number of job")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=4, order=40, help="User code")
        t.column('action', 'character', format="X(20)", initial="", max_width=40, label="Action", position=5, order=60, help="Action: Planing,Develoment,Testing")
        t.column('description', 'character', format="X(78)", initial="", max_width=156, label="Description", position=6, order=70)
        t.column('startdate', 'date', format="99.99.99", max_width=4, label="Start", position=7, order=80, help="Start date")
        t.column('enddate', 'date', format="99.99.99", max_width=4, label="End", position=8, order=90, help="End date")
        t.column('statuscode', 'integer', format=">9", initial="0", max_width=4, label="Statuscode", position=9, order=50, help="0=wait,10=work on,20=waiting infromation,99=complete")
        t.column('lastchanged', 'date', format="99.99.99", max_width=4, label="Last changed", position=10, order=100)
        t.column('lastuser', 'character', format="X(8)", initial="", max_width=16, label="Last user", position=11, order=110)
        t.column('applcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Application", position=12, order=20, help="Application code. Internal use only")
        t.index('job', [['job']], area="Sta_Index_2", primary=True, unique=True)
        t.index('application', [['applcode'], ['job']], area="Sta_Index_2", unique=True)
        t.index('programcode', [['programcode'], ['job']], area="Sta_Index_2", unique=True)
        t.index('statuscode', [['statuscode'], ['programcode']], area="Sta_Index_2")
        t.index('userstatus', [['usercode'], ['statuscode'], ['programcode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('progjob')
