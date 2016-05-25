from gearbox.migrations import Migration

class AddTableStoBal(Migration):

    database = "mobile"

    def up(self):
        t = self.table('StoBal', area="Sta_Data_128", dump_name="stobal", desc='''Article's Balance in certain stock
''')
        t.column('Stobal', 'character', format="x(8)", initial="", max_width=16, label="Stock", column_label="Stock", position=2, order=10, help="Stock Code")
        t.column('SimArt', 'character', format="x(12)", initial="", max_width=24, label="Sim Article", column_label="Sim Article", position=3, order=20, help="Article Code for a SIM type")
        t.column('DetBal', 'integer', format=">,>>>,>>9", initial="0", max_width=200, label="Balance", column_label="Balance", extent=10, position=4, order=30, help="Balance")
        t.column('Balance', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="TotalBal", column_label="Total Bal", position=5, order=40, help="Total Balance of article in this stock")
        t.column('OrdPoint', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="OrderPoint", column_label="OrderPoint", position=6, order=50, help="Ordering Point")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=7, order=60, help="Code Of Brand")
        t.index('Stobal', [['Brand'], ['Stobal'], ['SimArt']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SimArt', [['Brand'], ['SimArt'], ['Stobal']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('StoBal')
