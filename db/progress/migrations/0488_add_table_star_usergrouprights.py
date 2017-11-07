from gearbox.migrations import Migration

class AddTableusergrouprights(Migration):

    database = "star"

    def up(self):
        t = self.table('usergrouprights', area="Sta_Data_256", dump_name="ugrprigt")
        t.column('usergroup', 'character', format="X(12)", initial="", max_width=24, label="UserGroup", position=2, order=10)
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=3, order=20, help="Right group code")
        t.column('canCreate', 'logical', format="yes/no", initial="no", max_width=1, label="Create", position=4, order=30, help="Can create")
        t.column('canModified', 'logical', format="yes/no", initial="no", max_width=1, label="Modified", position=5, order=40, help="Can modified")
        t.column('canCopy', 'logical', format="yes/no", initial="no", max_width=1, label="Copy", position=6, order=50, help="Can copy")
        t.column('canDelete', 'logical', format="yes/no", initial="no", max_width=1, label="Delete", position=7, order=60, help="Can Delete")
        t.column('canDisplay', 'logical', format="yes/no", initial="no", max_width=1, label="Display", position=8, order=25, help="Can Display")
        t.index('usergroup', [['usergroup'], ['rightgroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('rightgroup', [['rightgroup'], ['usergroup']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('usergrouprights')
