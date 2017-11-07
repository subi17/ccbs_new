from gearbox.migrations import Migration

class AddTablePnpQty(Migration):

    database = "common"

    def up(self):
        t = self.table('PnpQty', area="Sta_Data_256", dump_name="pnpqty")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="CLI", column_label="CLI", position=2, order=10, help="CLI number")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=3, order=20, help="PNP Period")
        t.column('Qty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Amount", column_label="Amount", position=4, order=30, help="Amount of call to this PNP CLI/Period")
        t.column('TotQty', 'integer', format="zzzzz9", initial="0", max_width=4, label="TotQty", column_label="TotQty", position=5, order=40, help="Total Quantity of PNP ticket")
        t.index('Cli', [['CLI'], ['Period']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Period', [['Period', 'DESC'], ['CLI']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PnpQty')
