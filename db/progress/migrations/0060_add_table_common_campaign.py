from gearbox.migrations import Migration

class AddTableCampaign(Migration):

    database = "common"

    def up(self):
        t = self.table('Campaign', area="Sta_Data_128", label="Campaign", dump_name="campaign", desc='''Campaign header, rows in CampRow

''')
        t.column('Campaign', 'character', format="x(8)", initial="", max_width=16, label="Campaign ID", column_label="ID", position=2, order=10, help="Campaign ID")
        t.column('CaName', 'character', format="x(30)", initial="", max_width=60, label="Campaign Name", column_label="Name", position=3, order=20, help="Campaign name")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=4, order=30, help="Beginning of campaign")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=5, order=40, help="End of campaign")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=6, order=50, help="Code Of Brand")
        t.column('CampType', 'integer', format="9", initial="0", max_width=4, label="Campaign Type", column_label="Type", position=7, order=60, help="Campaign type")
        t.column('AccessSchemaSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="AccessSchemaSeq", column_label="AccessSchemaSeq", position=8, order=70, description="Link to the Access Schema for this campaign")
        t.index('Campaign', [['Brand'], ['Campaign']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CaName', [['Brand'], ['CaName']], area="Sta_Index_2")
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Campaign')
