from gearbox.migrations import Migration

class AddTableCampPrice(Migration):

    database = "common"

    def up(self):
        t = self.table('CampPrice', area="Sta_Data_256", label="CampPrice", dump_name="camppric", desc='''Campaign prices

''')
        t.column('Campaign', 'character', format="x(8)", initial="", max_width=16, label="Campaign ID", column_label="ID", position=2, order=10, help="Campaign ID")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="PriceList", column_label="Plist", position=3, order=20, help="Code (identifier) for a Price List")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="CLI", position=4, order=30, help="CLI")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=5, order=40, help="Beginning of campaign")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=6, order=50, help="End of campaign")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=7, order=60, help="Code Of Brand")
        t.index('CLI', [['Brand'], ['CLI'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('Campaign', [['Brand'], ['Campaign'], ['CLI']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CampPrice')
