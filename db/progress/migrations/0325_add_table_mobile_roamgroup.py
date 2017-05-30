from gearbox.migrations import Migration

class AddTableRoamGroup(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RoamGroup', area="Sta_Data_256", dump_name="roamgrou")
        t.column('RoamGroup', 'character', format="x(8)", initial="", max_width=16, label="RoamGroup", column_label="RoamGroup", position=2, order=10)
        t.column('Name', 'character', format="x(20)", initial="", max_width=40, label="Name", column_label="Name", position=3, order=20)
        t.index('RoamGroup', [['RoamGroup']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('RoamGroup')
