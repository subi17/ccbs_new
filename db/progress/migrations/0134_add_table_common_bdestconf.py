from gearbox.migrations import Migration

class AddTableBDestConf(Migration):

    database = "common"

    def up(self):
        t = self.table('BDestConf', area="Sta_Data_128", label="BDest Configuration", dump_name="bdestconf", desc="BDest configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('RateBDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="Rating Destination", column_label="Rate BDest", position=3, order=40, help="B-number destination used in rating")
        t.column('RateCCN', 'integer', format=">>9", initial="0", max_width=4, label="Rating CCN", column_label="RateCCN", position=4, order=50, help="Rating CCN")
        t.column('ConfigValue1', 'decimal', format="->>>>>>9.99", decimals=5, initial="0", max_width=20, label="Config Value 1", column_label="Value1", position=5, order=60, help="Configuration value 1")
        t.column('ConfigValue2', 'decimal', format="->>>>>>9.99", decimals=5, initial="0", max_width=20, label="Config Value 2", column_label="Value2", position=6, order=70, help="Configuration value 2")
        t.column('ConfigValue3', 'decimal', format="->>>>>>9.99", decimals=5, initial="0", max_width=20, label="Config Value 3", column_label="Value3", position=7, order=80, help="Configuration value 3")
        t.column('ConfigChar', 'character', format="x(12)", initial="", max_width=24, label="Config Value", column_label="CharValue", position=9, order=100, help="Configuration value")
        t.column('BDCGroup', 'character', format="x(12)", initial="", max_width=24, label="Config Group", column_label="Group", position=10, order=20, help="Configuration group ID")
        t.column('BDCName', 'character', format="x(30)", initial="", max_width=60, label="Config Description", column_label="Name", position=11, order=30, help="Configuration description")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=12, order=120, help="Last effective day")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=13, order=110, help="Valid from")
        t.column('GroupType', 'integer', format=">9", initial="0", max_width=4, label="Group Type", column_label="Type", position=14, order=130, help="Configuration group type")
        t.index('BDCGroup', [['Brand'], ['BDCGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('GroupType', [['Brand'], ['GroupType'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BDestConf')
