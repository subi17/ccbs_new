from gearbox.migrations import Migration

class AddTableCurrency(Migration):

    database = "common"

    def up(self):
        t = self.table('Currency', area="Sta_Data_128", label="Currency", dump_name="currenc", desc="Currency file")
        t.column('Currency', 'character', format="x(3)", initial="", max_width=6, label="Currency", column_label="Currency", position=2, order=10, help="Currency code")
        t.column('CurrName', 'character', format="x(20)", initial="", max_width=40, label="Currency Name", column_label="Currency Name", position=3, order=20, help="Name of the currency")
        t.column('SubUnit', 'character', format="x(8)", initial="", max_width=16, label="SubUnit", column_label="SubUnit", position=4, order=30, help="Subunit of currency (1/100)")
        t.column('SubName', 'character', format="x(12)", initial="", max_width=24, label="SubName", column_label="SubName", position=5, order=40, help="Name of subunit")
        t.index('Currency', [['Currency']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CurrName', [['CurrName'], ['Currency']], area="Sta_Index_2", unique=True)
        t.index('subunit', [['SubUnit'], ['Currency']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('Currency')
