from gearbox.migrations import Migration

class AddTableAreaCode(Migration):

    database = "star"

    def up(self):
        t = self.table('AreaCode', area="Sta_Data_256", label="AreaCode", dump_name="areacode", desc="Numbering areas")
        t.column('TrafficArea', 'integer', format="z9", initial="0", max_width=4, label="No ", column_label="Nro", position=2, order=10, help="Consecutive Number of a Traffic Area")
        t.column('AreaCode', 'character', format="x(4)", initial="", max_width=8, label="AreaCode", column_label="AreaCode", position=3, order=20, help="Area Code")
        t.column('POI', 'logical', format="Yes/No", initial="no", max_width=1, label="AP", column_label="AP", position=4, order=30, help="Is there an point of interconnection in this area")
        t.column('AreaName', 'character', format="x(30)", initial="", max_width=60, label="Place/City", column_label="Place/City", position=5, order=40, help="Name of place/city")
        t.column('Local', 'logical', format="Yes/No", initial="no", max_width=1, label="Local Segment", column_label="Local Segment", position=6, order=50, help="\"Shall calls from/to this area treated as \"\"local segment\"\" Y/N\"")
        t.column('TrunkCode', 'character', format="x(8)", initial="", max_width=16, label="CGR", column_label="CGR", position=7, order=60, help="Circuit Group Code for this ICP (point of interconnection)")
        t.index('AreaCode', [['AreaCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AreaName', [['AreaName'], ['AreaCode']], area="Sta_Index_2")
        t.index('POI', [['TrafficArea'], ['POI']], area="Sta_Index_2")
        t.index('TrafficArea', [['TrafficArea'], ['AreaCode']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('AreaCode')
