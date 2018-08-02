from gearbox.migrations import Migration

class AddTableProCommand(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ProCommandResp', area="Sta_Data_32", multitenant="yes", label="BUS/BPM commands to SAPC response history", dump_name="procommandresp")
        t.column('ProCommandId', 'int64', format=">>>>>>>>>9", initial="0", max_width=8, label="ProCommand Id", position=2, order=10, help="ProCommandId")
        t.column('ProCommandStatus', 'integer', format=">>9", initial="0", max_width=4, label="Status", position=3, order=20, help="Status: 0 - New, 10 - Waiting accept notify, 20 - Accepted notify, 30 - Done, 80... - Errors")
        t.column('ResponseTS', 'datetime-tz', format="99/99/9999 HH:MM:SS.SSS+HH:MM", initial=self.unknown, max_width=12, label="Response", position=4, order=30, help="Date and time when BUS/BPM response is received")
        t.column('Response', 'clob', clob_collation="BASIC", format="x(8)", initial=self.unknown, lob_size="10M", lob_bytes=10485760, lob_area="Y_Lob", position=15, clob_type=2, order=140, clob_codepage="UTF-8")
        t.index('Ix_ProCommandId', [['ProCommandId'], ['ResponseTS', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('Ix_ProCommandStatus', [['ProCommandStatus'], ['ProCommandId']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ProCommandResp')
