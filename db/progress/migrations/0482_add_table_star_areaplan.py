from gearbox.migrations import Migration

class AddTableAreaPlan(Migration):

    database = "star"

    def up(self):
        t = self.table('AreaPlan', area="Sta_Data_256", label="AreaPlan", dump_name="areaplan", desc="Traffic area plan")
        t.column('TrafficArea', 'integer', format="z9", initial="0", max_width=4, label="No.", column_label="No.", position=2, order=10, help="Number of a Traffic Area")
        t.column('AreaName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Name of a Traffic Area")
        t.column('GroupCode', 'integer', format=">9", initial="0", max_width=4, label="Group", column_label="Group", position=4, order=30, help="Group number, calls within one group are local")
        t.index('TrafficArea', [['TrafficArea']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AreaName', [['AreaName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('AreaPlan')
