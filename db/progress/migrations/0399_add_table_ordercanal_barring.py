from gearbox.migrations import Migration

class AddTableBarring(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('Barring', area="Sta_Data_128", label="Barring", table_trigger=[{'crc': '?', 'procedure': 'rd-barring.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-barring.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="barring", desc="Barring information")
        t.column('MsSeq', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="SubSeq", column_label="MsSeq", position=2, order=10, help="Subscription ID")
        t.column('BarringCode', 'character', format="x(25)", initial="", max_width=50, label="Barring", column_label="BarringCode", position=3, order=20, help="Barring name")
        t.column('BarringStatus', 'character', format="x(8)", initial="", max_width=16, label="Barring Status", column_label="BarringStatus", position=4, order=30, help="Activity for status")
        t.column('EventTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", column_label="EventTS", position=5, order=40, help="Event Time Stamp")
        t.column('UserCode', 'character', format="x(12)", initial="", max_width=24, label="User ID", column_label="UserCode", position=6, order=50, help="User ID")
        t.column('MsRequest', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MsRequest", column_label="MsRequest", position=7, order=60, help="MsRequest ID")
        t.index('MsSeq', [['MsSeq'], ['BarringCode'], ['EventTS', 'DESC']], area="Dyn_Index_1", primary=True)
        t.index('EventTS', [['EventTS', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('Barring')
