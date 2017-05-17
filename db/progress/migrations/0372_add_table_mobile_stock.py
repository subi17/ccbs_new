from gearbox.migrations import Migration

class AddTableStock(Migration):

    database = "mobile"

    def up(self):
        t = self.table('Stock', area="Sta_Data_64", dump_name="stock", desc='''Stock Location
''')
        t.column('Stock', 'character', format="x(8)", initial="", max_width=16, label="Stock", column_label="Stock", position=2, order=10, help="Stock Code")
        t.column('StoName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Name Of Stock")
        t.column('StoStreet', 'character', format="x(30)", initial="", max_width=60, label="Street", column_label="Street", position=4, order=30, help="Street Address")
        t.column('StoCity', 'character', format="x(30)", initial="", max_width=60, label="City", column_label="City", position=5, order=40, help="City (Postal no + Post Office)")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Retailer", column_label="Retailer", position=6, order=50, help="An unique code for a retailer; maximum 8 characters")
        t.column('StoFile1', 'character', format="x(60)", initial="", max_width=120, label="ICCFileName", column_label="ICCFileName", position=7, order=60, help="Directory and Name of 'ICC information' output file")
        t.column('StoFile2', 'character', format="x(60)", initial="", max_width=120, label="SubsDataFile", column_label="SubsDataFile", position=8, order=70, help="Directory and name of 'Subscriber Data' output file")
        t.column('StoFile3', 'character', format="x(60)", initial="", max_width=120, label="SIMDistFile", column_label="SIMDistFile", position=9, order=80, help="Directory and name of 'SIM Distribution' output file")
        t.column('SimDel', 'integer', valexp="SimDel < 3", format="9", initial="0", max_width=4, label="SimDel", column_label="SimDel", position=10, order=90, valmsg="0:External delivery file used 1:Low Prioirity 2:High Priority", help="Default SIM Delivery Method")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=11, order=100, help="Code Of Brand")
        t.column('StoType', 'character', format="x(8)", initial="", max_width=16, label="Type", column_label="Type", position=12, order=110, help="Stock Type")
        t.column('ZipCodeExp', 'character', format="x(8)", initial="", max_width=16, label="ZipCodeExp", column_label="ZipCodeExp", position=13, order=120, help="Zip code regular expression")
        t.index('Reseller', [['Brand'], ['Reseller'], ['Stock']], area="Sta_Index_3", primary=True, unique=True)
        t.index('Stock', [['Brand'], ['Stock']], area="Sta_Index_3", unique=True)
        t.index('StoName', [['Brand'], ['StoName'], ['Stock']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('Stock')
