from gearbox.migrations import Migration

class AddTableBRAND(Migration):

    database = "common"

    def up(self):
        t = self.table('BRAND', area="Sta_Data_256", dump_name="brand", desc='''Brand info
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('BRName', 'character', format="x(20)", initial="", max_width=40, label="BrName", column_label="BrName", position=3, order=20, help="Name of Brand")
        t.column('directory', 'character', format="x(50)", initial="", max_width=100, label="Dir", column_label="Dir", position=4, order=30, help="Directory Of Brand")
        t.column('Address', 'character', format="x(25)", initial="", max_width=50, label="Address", column_label="Addr.", position=5, order=40, help="Address of Brand")
        t.column('Phone', 'character', format="x(18)", initial="", max_width=36, label="Tel", column_label="Tel", position=6, order=50, help="Telefon Number")
        t.column('ContactName', 'character', format="x(20)", initial="", max_width=40, label="ContactName", column_label="ContactName", position=7, order=60, help="Contact Name")
        t.column('email', 'character', format="x(50)", initial="", max_width=100, label="Email", column_label="Email", position=8, order=70, help="Email")
        t.column('ProgPath', 'character', format="x(40)", initial="", max_width=80, label="Program Path", column_label="Path", position=9, order=80, help="Brand specific program path (propath)")
        t.column('PostOffice', 'character', format="x(24)", initial="", max_width=48, label="Postal Address", column_label="Post Addr.", position=10, order=90, help="Postal address")
        t.index('Brand', [['Brand']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BRName', [['BRName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BRAND')
