from gearbox.migrations import Migration

class AddTableBillType(Migration):

    database = "mobile"

    def up(self):
        t = self.table('BillType', area="Sta_Data_128_2", label="ObiType", dump_name="obitype", desc="Object Of Billing Type")
        t.column('BillType', 'character', format="x(8)", initial="", max_width=16, label="Billing Object", column_label="Billing Object", position=2, order=10, help="Type Of Billing Object")
        t.column('BTName', 'character', format="x(30)", initial="", max_width=60, label="Billing Object Name", column_label="Billing Object Name", position=3, order=20, help="Name Of The Billing Object")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo of Billing Object")
        t.index('BillType', [['BillType', 'ABBREVIATED']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BTName', [['BTName'], ['BillType', 'ABBREVIATED']], area="Sta_Index_3")

    def down(self):
        self.drop_table('BillType')
