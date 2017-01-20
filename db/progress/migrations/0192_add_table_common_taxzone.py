from gearbox.migrations import Migration

class AddTableTaxZone(Migration):

    database = "common"

    def up(self):
        t = self.table('TaxZone', area="Sta_Data_128", label="Tax Zone", dump_name="taxzone", desc="Tax zones")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=2, order=10, help="Tax Zone")
        t.column('TZName', 'character', format="x(30)", initial="", max_width=60, label="Zone Name", column_label="Name", position=3, order=20, help="Tax zone name")
        t.index('TaxZone', [['TaxZone']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TaxZone')
