from gearbox.migrations import Migration

class AddTableBnet(Migration):

    database = "mobile"

    def up(self):
        t = self.table('Bnet', area="Sta_Data_256", dump_name="bnet")
        t.column('BnetCode', 'character', format="x(5)", initial="", max_width=10, label="Code", column_label="Code", position=2, order=10, help="Mobile Operator/Service Provider")
        t.column('BnetValue', 'character', format="x(8)", initial="", max_width=16, label="Type", column_label="Type", position=3, order=20, help="Type of BNET")
        t.column('BnetName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=4, order=30, help="Name of the BNET")
        t.column('Memo', 'character', format="x(70)", initial="", max_width=2130, label="Memo", column_label="Memo", extent=15, position=5, order=40, help="Memo")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=6, order=50, help="Code Of Brand")
        t.index('BnetCode', [['Brand'], ['BnetCode'], ['BnetValue']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BnetName', [['Brand'], ['BnetName'], ['BnetValue']], area="Sta_Index_3")
        t.index('BnetValue', [['Brand'], ['BnetValue'], ['BnetCode']], area="Sta_Index_3")

    def down(self):
        self.drop_table('Bnet')
