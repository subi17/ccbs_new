from gearbox.migrations import Migration

class AddTableHiUsageLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('HiUsageLimit', area="Sta_Data_2_256", dump_name="hiusagel")
        t.column('Category', 'character', format="x(4)", initial="", max_width=8, label="Cat", column_label="Cat", position=2, order=10, help="Category code")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="ProdCd", column_label="ProdCd", position=3, order=20, help="Product code, max 16 characters")
        t.column('Limit', 'integer', format=">,>>>,>>9", initial="0", help="Highusage limit", max_width=4, label="Limit", column_label="Limit", position=4, order=30, description="Highusage limit")
        t.index('Category', [['Category'], ['BillCode']], area="Sta_Index_4", primary=True, unique=True)
        t.index('BillCode', [['BillCode'], ['Limit', 'DESC']], area="Sta_Index_4")

    def down(self):
        self.drop_table('HiUsageLimit')
