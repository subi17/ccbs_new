from gearbox.migrations import Migration

class AddTableDumpHPD(Migration):

    database = "common"

    def up(self):
        t = self.table('DumpHPD', area="Sta_Data_128", multitenant="yes", label="Dump Range", dump_name="DumpHPD", desc="Dump HPD")
        t.column('DumpID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump ID", column_label="ID", position=2, order=10, help="Dump ID")
        t.column('Active', 'logical', format="Yes/No", initial="yes", max_width=1, label="Active", position=3, order=20, help="Is dump range active")
        t.column('Continuous', 'logical', format="Yes/No", initial="yes", max_width=1, label="Continuous", position=4, order=30, help="Is the dump range continuous")
        t.column('StartTime', 'character', format="x(19)", initial="", max_width=38, label="Start Time", column_label="StartTime", position=5, order=40, help="Start time of a dump DD.MM.YYYY HH:MM:SS")
        t.column('FinalTime', 'character', format="x(19)", initial="", max_width=38, label="Final Time", column_label="FinalTime", position=6, order=50, help="Final time of a dump DD.MM.YYYY HH:MM:SS")
        t.column('UnitsToDump', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Units to dump", column_label="UnitsToDump", position=7, order=60, help="How many unit to dump. Zero value means no restriction.")
        t.column('UnitType', 'character', format="x(12)", initial="days", max_width=24, label="Unit type", column_label="UnitType", position=8, order=70, help="The type of the unit (years, months, weeks, days, hours, minutes, seconds)")
        t.index('DumpID', [['DumpID']], area="Sta_Index_4", primary=True, unique=True)

    def down(self):
        self.drop_table('DumpHPD')
