from gearbox.migrations import Migration

class AddTableRegion(Migration):

    database = "common"

    def up(self):
        t = self.table('Region', area="Sta_Data_128", label="Region", dump_name="region", desc="Customer region")
        t.column('Region', 'character', format="x(8)", initial="", help="Region code", max_width=16, label="Region", position=2, order=10, description='''

''')
        t.column('RgName', 'character', format="x(30)", initial="", help="Region name", max_width=60, label="Name", position=3, order=20, description='''

''')
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=6, order=50, help="Tax Zone")
        t.index('Region', [['Region']], area="Sta_Index_2", primary=True, unique=True)
        t.index('RgName', [['RgName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Region')
