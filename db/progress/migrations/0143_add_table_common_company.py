from gearbox.migrations import Migration

class AddTableCompany(Migration):

    database = "common"

    def up(self):
        t = self.table('Company', area="Sta_Data_32", label="Company", dump_name="company", desc="Company data")
        t.column('CompName', 'character', format="x(30)", initial="", max_width=60, label="CompName", column_label="Comp.Name", position=2, order=10, help="Company's name")
        t.column('Address', 'character', format="x(30)", initial="", max_width=60, label="Address", column_label="Address", position=3, order=20, help="Company's address")
        t.column('PostOffice', 'character', format="x(30)", initial="", max_width=60, label="Postcode", column_label="Postcode", position=4, order=30, help="Company's postal code + city")
        t.column('Phone', 'character', format="x(30)", initial="", max_width=60, label="Phone", column_label="Phone", position=5, order=35, help="Company's telephone number")
        t.column('Fax', 'character', format="x(20)", initial="", max_width=40, label="Fax", column_label="Fax", position=6, order=40, help="Company's fax number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=440, help="Code Of Brand")
        t.column('Phone2', 'character', format="x(30)", initial="", max_width=60, label="Phone 2", position=9, order=450, help="Phone")
        t.column('Phone3', 'character', format="x(30)", initial="", max_width=60, label="Phone 3", position=10, order=460, help="Phone")
        t.column('CompanyID', 'character', format="x(20)", initial="", max_width=40, label="Company ID", position=11, order=470, help="Company ID")
        t.column('UnitCode', 'integer', format=">>9", initial="0", max_width=4, label="Unit", position=12, order=430, help="Unit number")
        t.column('HomeLocation', 'character', format="x(30)", initial="", max_width=60, label="Home Location", column_label="Home", position=13, order=480, help="Home location that is printed to invoice")
        t.column('Address3', 'character', format="x(30)", initial="", max_width=60, label="Address 3", column_label="Addr3", position=14, order=500, help="Additional address")
        t.column('Address4', 'character', format="x(30)", initial="", max_width=60, label="Address 4", column_label="Addr4", position=15, order=510, help="Additional address")
        t.column('Address2', 'character', format="x(30)", initial="", max_width=60, label="Address 2", column_label="Addr2", position=16, order=490, help="Additional address")
        t.index('unitcode', [['Brand'], ['UnitCode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('Company')
