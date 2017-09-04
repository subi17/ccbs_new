from gearbox.migrations import Migration

class AddTableapplication(Migration):

    database = "star"

    def up(self):
        t = self.table('application', area="Sta_Data_256", label="Application", dump_name="appllica", desc="Applications on the system")
        t.column('applcode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Application", position=2, order=10, help="Application code. Internal use only")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=3, order=20, help="Application name")
        t.index('application', [['applcode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('application')
