from gearbox.migrations import Migration

class AddTableRoamZone(Migration):

    database = "common"

    def up(self):
        t = self.table('RoamZone', area="Sta_Data_128", dump_name="RoamZone", desc="Roaming Zone ")
        t.column('RoamZone', 'character', format="x(12)", initial="", max_width=24, label="RoamZone", column_label="RoamZone", position=2, order=10, help="RoamZone")
        t.column('RZName', 'character', format="x(8)", initial="", max_width=16, label="RoamZone Name", column_label="RoamZone Name", position=3, order=20, help="Name of RoamZone")
        t.index('RoamZone', [['RoamZone']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('RoamZone')
