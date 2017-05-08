from gearbox.migrations import Migration

class AddTableBBoard(Migration):

    database = "common"

    def up(self):
        t = self.table('BBoard', area="Sta_Data_256", label="Bulletin Board Table", dump_name="bboard", desc="Bulletin Board Details")
        t.column('BBNumber', 'integer', format=">>>>>>>9", initial="0", help="Reference Number for Bulletin Board", max_width=4, label="BBNumber", column_label="BBNumber", position=2, order=10, description="Auto-generated sequence number for Bulletin Board Message")
        t.column('TroTicket', 'integer', format=">,>>>,>>9", initial="0", help="Reference no. for Customer", max_width=4, label="Reference", column_label="Reference", position=3, order=20, description="Auto-generated sequence number for Trouble Ticket")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of Bulletin Board Message creation Time", max_width=20, label="Created", column_label="Created", position=4, order=30, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Importance', 'character', format="X(8)", initial="", max_width=16, label="Importance", column_label="Importance", position=5, order=40, help="Importance of Bulletin Board Message")
        t.column('Topic', 'character', format="x(60)", initial="", max_width=120, label="Topic", column_label="Topic", position=6, order=50, help="Bulletin Board Topic")
        t.column('BBText', 'character', format="x(60)", initial="", max_width=120, label="Text", column_label="Text", position=7, order=60, help="Bulletin Board Message Text")
        t.column('ExpStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of Bulletin Board Message expiration", max_width=20, label="Expires", column_label="Expires", position=8, order=70, description="Time Stamp yyyymmdd.time (sec)")
        t.column('UserCode', 'character', format="X(8)", initial="", max_width=16, label="User", position=9, order=80, help="Bulletin Board Message created By")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.index('bbnumber', [['Brand'], ['BBNumber']], area="Sta_Index_2", primary=True, unique=True)
        t.index('importance', [['Brand'], ['Importance', 'ABBREVIATED']], area="Sta_Index_2")
        t.index('TroTicket', [['Brand'], ['TroTicket'], ['BBNumber']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('BBoard')
