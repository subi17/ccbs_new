from gearbox.migrations import Migration

class AddTableProCommand(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ProCommand', area="Sta_Data_32", multitenant="yes", label="BUS/BPM commands to SAPC", dump_name="procommand")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('CreatedTS', 'datetime-tz', format="99/99/9999 HH:MM:SS.SSS+HH:MM", initial=self.unknown, max_width=12, label="Creation", position=3, order=20, help="Date and time when record is created")
        t.column('ResponseTS', 'datetime-tz', format="99/99/9999 HH:MM:SS.SSS+HH:MM", initial=self.unknown, max_width=12, label="Response", position=5, order=40, help="Date and time when BUS/BPM response is received")
        t.column('ProCommandId', 'int64', format=">>>>>>>>>9", initial="0", max_width=8, label="ProCommand Id", position=6, order=50, help="Record Unique identifier")
        t.column('MsRequest', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Request ID", column_label="ID", position=7, order=60, help="Unique ID for request")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MobSub Sequence", column_label="SubSeq", position=8, order=70, help="Sequence for a subscription")
        t.column('ProCommandStatus', 'integer', format=">>9", initial="0", max_width=4, label="Status", position=9, order=80, help="Status: 0 - New, 10 - Waiting accept notify, 20 - Accepted notify, 30 - Done, 80... - Errors")
        t.column('ProCommandTarget', 'character', format="x(8)", initial="", help="NB_CH, NB_AS for Northbound Change and After Sales, BPM for BPM", max_width=16, label="Target", position=10, order=90, description="NB for Northbound, BPM for BPM")
        t.column('ProCommandTargetURL', 'character', format="x(70)", initial="", max_width=140, label="Target URL", position=11, order=100, help="Target URL")
        t.column('ProCommandType', 'character', format="x(30)", initial="", help="Command type", max_width=60, label="Command Type", position=12, order=110, description="Command Type: add_upsell, change_imsi, ...")
        t.column('Creator', 'character', format="x(12)", initial="", max_width=24, label="Creator", position=13, order=120, help="Who made this procommand request")
        t.column('CommandLine', 'clob', clob_collation="BASIC", format="x(8)", initial=self.unknown, lob_size="10M", lob_bytes=10485760, lob_area="Y_Lob", position=14, clob_type=2, order=130, clob_codepage="UTF-8")
        t.column('Response', 'clob', clob_collation="BASIC", format="x(8)", initial=self.unknown, lob_size="10M", lob_bytes=10485760, lob_area="Y_Lob", position=15, clob_type=2, order=140, clob_codepage="UTF-8")
        t.column('SendTS', 'datetime-tz', format="99/99/9999 HH:MM:SS.SSS+HH:MM", initial=self.unknown, max_width=12, label="Send", position=16, order=150, help="Date and time when command is sent to BUS/BPM")
        t.column('ProCommandVerb', 'character', format="x(8)", initial="", help="HTTP method (verb) for the command", max_width=16, label="Verb", position=17, order=160, description="POST, GET, ...")
        t.index('Ix_ProCommandId', [['ProCommandId']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Ix_MsSeq', [['MsSeq'], ['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('Ix_OrderId', [['OrderId'], ['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('Ix_MsRequest', [['MsRequestId'], ['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('Ix_Target', [['ProCommandTarget'], ['ProCommandStatus'], ['CreatedTS']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ProCommand')
