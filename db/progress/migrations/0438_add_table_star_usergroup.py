from gearbox.migrations import Migration

class AddTableusergroup(Migration):

    database = "star"

    def up(self):
        t = self.table('usergroup', area="Sta_Data_256", dump_name="usergrrp")
        t.column('usergroup', 'character', format="X(12)", initial="", max_width=24, label="UserGroup", position=2, order=10)
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=3, order=20, help="User group name")
        t.index('usergroup', [['usergroup']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('usergroup')
