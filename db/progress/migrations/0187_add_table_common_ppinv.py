from gearbox.migrations import Migration

class AddTablePPInv(Migration):

    database = "common"

    def up(self):
        t = self.table('PPInv', area="Sta_Data_256", label="PP Invoices", dump_name="ppinv", desc="Payment plan's invoices")
        t.column('PPlanID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Payment Plan ID", column_label="PP ID", position=2, order=10, help="Payment plan ID")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice", column_label="InvNum", position=3, order=20, help="Invoice number")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", position=4, order=30, help="Invoice's debt when it was added to payment plan")
        t.index('PPlanID', [['PPlanID'], ['InvNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('InvNum', [['InvNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PPInv')
