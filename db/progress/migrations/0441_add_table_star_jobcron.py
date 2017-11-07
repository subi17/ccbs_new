from gearbox.migrations import Migration

class AddTablejobcron(Migration):

    database = "star"

    def up(self):
        t = self.table('jobcron', area="Sta_Data_256", dump_name="jobcron")
        t.column('jobgroup', 'character', format="X(10)", initial="", max_width=20, label="Job group", position=2, order=10, help="Job group code")
        t.column('number', 'integer', format=">9", initial="0", max_width=4, label="Number", position=3, order=20, help="Job groups sequence number")
        t.column('InputData', 'character', format="X(256)", initial="", max_width=512, label="Input Data", position=4, order=30, help="starEntry list")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=5, order=25, help="Programcode")
        t.index('number', [['jobgroup'], ['number']], area="Sta_Index_2", primary=True, unique=True)
        t.index('programCode', [['jobgroup']], area="Sta_Index_2")

    def down(self):
        self.drop_table('jobcron')
