from gearbox.migrations import Migration

class AddTableEPMember(Migration):

    database = "mobile"

    def up(self):
        t = self.table('EPMember', area="Sta_Data_256", label="Member of external product group", dump_name="epmemb")
        t.column('EpGroup', 'character', format="x(12)", initial="", max_width=24, label="EPcode", column_label="EPcode", position=2, order=10, help="Unique code of product group")
        t.column('BillCode', 'character', format="x(8)", initial="", max_width=16, label="ProdNo", column_label="ProdNo", position=3, order=20, help="Product code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=4, order=30, help="Code Of Brand")
        t.index('EpGroup', [['Brand'], ['EpGroup'], ['BillCode']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BillCode', [['Brand'], ['BillCode'], ['EpGroup']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('EPMember')
