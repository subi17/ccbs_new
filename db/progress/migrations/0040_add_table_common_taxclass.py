from gearbox.migrations import Migration

class AddTableTaxClass(Migration):

    database = "common"

    def up(self):
        t = self.table('TaxClass', area="Sta_Data_128", label="Tax Class", dump_name="taxclass", desc="Tax classes")
        t.column('TaxClass', 'character', format="x(8)", initial="", help="Tax class", max_width=16, label="Tax Class", column_label="Class", position=2, order=10, description='''

''')
        t.column('TCName', 'character', format="x(30)", initial="", max_width=60, label="Class Name", column_label="Name", position=3, order=20, help="Tax class name")
        t.index('TaxClass', [['TaxClass']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TaxClass')
