from gearbox.migrations import Migration

class AddTableFColor(Migration):

    database = "common"

    def up(self):
        t = self.table('FColor', area="Sta_Data_256", label="Colours", dump_name="fcolor", desc="Colours")
        t.column('FrameName', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="FrmName", column_label="FrmName", position=2, order=5, help="Frame's name in a code")
        t.column('TitleColor', 'character', format="x(24)", initial="", max_width=48, label="Header's colour", column_label="Header's colour", position=3, order=10, help="Pair of colors for frame's title bar, fg/bg")
        t.column('FrameColor', 'character', format="x(24)", initial="", max_width=48, label="FrColour", column_label="FrColour", position=4, order=20, help="\"Colors of a frame, separated with slash \"\"/\"\"\"")
        t.index('FrameName', [['FrameName', 'ABBREVIATED']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('FColor')
