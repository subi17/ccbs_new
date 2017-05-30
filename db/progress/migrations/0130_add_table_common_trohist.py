from gearbox.migrations import Migration

class AddTableTroHist(Migration):

    database = "common"

    def up(self):
        t = self.table('TroHist', area="Sta_Data_256", label="Trouble Ticket History", dump_name="trohist", desc="Trouble Ticket history")
        t.column('TTNum', 'integer', format=">,>>>,>>9", initial="0", help="Reference no. for Customer", max_width=4, label="Reference", column_label="Reference", position=2, order=10, description="Auto-generated sequence number for Trouble Ticket")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of trohist creation", max_width=20, label="Created", column_label="Created", position=3, order=20, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Event', 'character', format="x(60)", initial="", max_width=120, label="Event", column_label="Event", position=4, order=30, help="History Event")
        t.column('CreUser', 'character', format="X(8)", initial="", max_width=16, label="Created By", position=5, order=40, help="User code")
        t.index('TTNum', [['TTNum'], ['CreStamp']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('TroHist')
