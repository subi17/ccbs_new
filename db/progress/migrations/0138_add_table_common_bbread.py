from gearbox.migrations import Migration

class AddTableBBRead(Migration):

    database = "common"

    def up(self):
        t = self.table('BBRead', area="Sta_Data_256", label="Bulletin Board Readers Table", dump_name="bbread", desc="Bulletin Board Readers Details")
        t.column('BBNumber', 'integer', format=">>>>>>>9", initial="0", help="Reference Number for Bulletin Board", max_width=4, label="BBNumber", column_label="BBNumber", position=2, order=10, description="Auto-generated sequence number for Bulletin Board Message")
        t.column('UserCode', 'character', format="X(8)", initial="", max_width=16, label="Read By", position=3, order=20, help="Bulletin Board User")
        t.column('ReadStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of Bulletin Board Message read Time", max_width=20, label="Read On", column_label="Read On", position=4, order=30, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('bbnumber', [['Brand'], ['BBNumber'], ['UserCode']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('BBRead')
