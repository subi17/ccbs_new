from gearbox.migrations import Migration

class AddTableCountry(Migration):

    database = "common"

    def up(self):
        t = self.table('Country', area="Sta_Data_2_256", label="Countries", dump_name="country", desc='''Country codes (ISO) and names
''')
        t.column('Country', 'character', format="xxx", initial="", max_width=6, label="Country", column_label="Country", position=2, order=10, help="Country Code (according to ISO Standard)")
        t.column('CoName', 'character', format="x(30)", initial="", max_width=60, label="Name of Country", column_label="Name of Country", position=3, order=20, help="Name of Country")
        t.column('FraudGroup', 'character', format="x(8)", initial="", max_width=16, label="Fraud Group", column_label="FraudGrp", position=4, order=30, help="Roaming Fraud Group")
        t.index('Country', [['Country']], area="Sta_Index_3", primary=True, unique=True)
        t.index('CoName', [['CoName']], area="Sta_Index_3")

    def down(self):
        self.drop_table('Country')
