from gearbox.migrations import Migration

class AddTablestarParamGroup(Migration):

    database = "star"

    def up(self):
        t = self.table('starParamGroup', area="Sta_Data_256", dump_name="starprgr")
        t.column('groupCode', 'character', format="X(20)", initial="", max_width=40, label="Group", position=2, order=10, help="Group code")
        t.column('groupName', 'character', format="X(40)", initial="", max_width=80, label="Name", position=3, order=20, help="Name")
        t.index('GroupCode', [['groupCode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('starParamGroup')
