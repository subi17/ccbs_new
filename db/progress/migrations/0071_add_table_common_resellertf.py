from gearbox.migrations import Migration

class AddTableResellerTF(Migration):

    database = "common"

    def up(self):
        t = self.table('ResellerTF', area="Sta_Data_128", label="ResellerTF", dump_name="resellertf", desc="ResellerTF")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Reseller", column_label="Reseller", position=3, order=20, help="Reseller Code")
        t.column('TFBank', 'character', format="x(8)", initial="", max_width=16, label="TFBank", column_label="TFBank", position=4, order=30)
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="ValidFrom", position=5, order=40)
        t.index('ResellerTF', [['Brand'], ['Reseller'], ['ValidFrom', 'DESC']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('ResellerTF')
