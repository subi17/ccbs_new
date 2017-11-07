from gearbox.migrations import Migration

class AddTablePaymConfTax(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymConfTax', area="Sta_Data_64", dump_name="paymconftax", desc="Configuration rules for tax accounts of payments")
        t.column('PaymConfig', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="ID", position=2, order=90, help="Configuration ID, used in links")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=3, order=270, help="Tax Zone")
        t.column('TaxAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Tax Account", column_label="Tax Acc", position=4, order=280, help="Tax account number")
        t.index('PaymConfig', [['PaymConfig'], ['TaxZone']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('PaymConfTax')
