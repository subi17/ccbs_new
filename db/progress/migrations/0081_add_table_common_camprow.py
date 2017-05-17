from gearbox.migrations import Migration

class AddTableCampRow(Migration):

    database = "common"

    def up(self):
        t = self.table('CampRow', area="Sta_Data_128", label="CampRow", dump_name="camprow", desc='''Campaign rows
''')
        t.column('Campaign', 'character', format="x(8)", initial="", max_width=16, label="Campaign ID", column_label="ID", position=2, order=10, help="Campaign ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=3, order=20, help="Code Of Brand")
        t.column('CLIType', 'character', format="x(8)", initial="", max_width=16, label="CLI Type", column_label="CLIType", position=4, order=30, help="CLI Type")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=5, order=40, help="Billing item code")
        t.column('CRowType', 'integer', format="9", initial="0", help="Row type", max_width=4, label="Row Type", column_label="Type", position=6, order=50, description="1=pricelist,2=discplan etc.")
        t.column('CRowItem', 'character', format="x(16)", initial="", help="Campaign row item", max_width=32, label="Row Item", column_label="Item", position=7, order=60, description="pricelist code, discplan code etc.")
        t.index('Campaign', [['Brand'], ['Campaign'], ['CRowType'], ['CRowItem']], area="Sta_Index_2", primary=True)
        t.index('BillCode', [['Brand'], ['BillCode']], area="Sta_Index_2")
        t.index('CLIType', [['Brand'], ['CLIType']], area="Sta_Index_2")
        t.index('CRowType', [['Brand'], ['CRowType'], ['CLIType']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CampRow')
