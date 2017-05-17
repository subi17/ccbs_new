from gearbox.migrations import Migration

class AddTableHiUsageKat(Migration):

    database = "common"

    def up(self):
        t = self.table('HiUsageKat', area="Sta_Data_2_256", dump_name="hiusagek")
        t.column('Category', 'character', format="x(4)", initial="", max_width=8, label="Cat", column_label="Cat", position=2, order=10, help="Category code")
        t.column('AgeFrom', 'integer', format=">>9", initial="0", max_width=4, label="AgeFrom", column_label="Age From", position=3, order=20, help="From Age")
        t.column('AgeTo', 'integer', format=">>9", initial="999", max_width=4, label="AgeTo", column_label="AgeTo", position=4, order=30, help="To Age")
        t.column('CLIType', 'character', format="x(8)", initial="", help="CLI Type", max_width=16, label="CLIType", column_label="CLIType", position=5, order=40, description="CLI Type")
        t.column('ActInDays', 'integer', format=">>>9", initial="0", max_width=4, label="ActiveInDays", column_label="ActiveInDays", position=6, order=50, help="Active in days")
        t.column('CustClass', 'integer', format="9", initial="0", max_width=4, label="Class", column_label="Class", position=7, order=60, help="Customer Class (depend on avg. amount of invoices)")
        t.index('Category', [['CustClass']], area="Sta_Index_4", primary=True, unique=True)

    def down(self):
        self.drop_table('HiUsageKat')
