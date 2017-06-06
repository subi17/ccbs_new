from gearbox.migrations import Migration

class AddTableSimArt(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SimArt', area="Sta_Data_128_2", dump_name="simart", desc='''SIM Article
''')
        t.column('SimArt', 'character', format="x(12)", initial="", max_width=24, label="Sim Article", column_label="Sim Article", position=2, order=10, help="Article Code for a SIM type")
        t.column('ManCode', 'character', format="x(8)", initial="", max_width=16, label="Manufacturer", column_label="Manufacturer", position=3, order=20, help="Code Of SIM Manufacturer")
        t.column('SAName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=4, order=30, help="Name of a SIM Card Type")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=5, order=40, help="Memo")
        t.column('Balance', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Total Balance", column_label="Total Bal", position=6, order=50, help="Toltal Balance (No. of individual cards in all stock locs)")
        t.column('OrdPoint', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="OrderPoint", column_label="OrderPoint", position=7, order=60, help="Ordering Point (total)")
        t.column('DetBal', 'integer', format=">,>>>,>>9", initial="0", max_width=200, label="Balance", column_label="Balance", extent=10, position=8, order=70, help="Number of cards")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=9, order=80, help="Code Of Brand")
        t.index('SimArt', [['Brand'], ['SimArt']], area="Sta_Index_3", primary=True, unique=True)
        t.index('mancode', [['Brand'], ['ManCode'], ['SimArt']], area="Sta_Index_3", unique=True)
        t.index('SAName', [['Brand'], ['SAName'], ['SimArt']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('SimArt')
