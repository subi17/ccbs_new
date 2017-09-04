from gearbox.migrations import Migration

class AddTableSalesoffice(Migration):

    database = "common"

    def up(self):
        t = self.table('Salesoffice', area="Sta_Data_256", label="Salesoffices", dump_name="salesoff", desc="Salesoffices for salesmen")
        t.column('SalesOffice', 'character', format="x(8)", initial="", max_width=16, label="SalesOffice", column_label="SalesOffice", position=2, order=10, help="Sales office code")
        t.column('SOName', 'character', format="x(30)", initial="", max_width=60, label="Office Name", column_label="SalesOffice Name", position=3, order=20, help="Sales office name")
        t.column('CostCentre', 'integer', format="999", initial="0", max_width=4, label="Cost Center", column_label="Cost Center", position=4, order=30, help="Cost Center")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('SalesOffice', [['Brand'], ['SalesOffice']], area="Sta_Index_2", primary=True, unique=True)
        t.index('SOName', [['Brand'], ['SOName'], ['SalesOffice']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('Salesoffice')
