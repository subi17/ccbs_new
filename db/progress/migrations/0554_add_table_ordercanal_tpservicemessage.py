from gearbox.migrations import Migration

class AddTableTPServiceMessage(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TPServiceMessage', area="Sta_Data_64", label="Third Party Message", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-tpservicemessage.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-tpservicemessage.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="tpservicemessage", desc="Third Party Message")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=2, order=20, help="Sequence for a Subscription")
        t.column('ServSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="ServSeq", column_label="ServSeq", position=3, order=30, help="Service Sequence Number")
        t.column('MessageSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MessageSeq", column_label="MessageSeq", position=4, order=40, help="Message Sequence Number")
        t.column('Source', 'character', format="x(8)", initial="", max_width=16, label="Message Source", column_label="Source", position=6, order=60, description="Messsage source (TMS/Masmovil)")
        t.column('MessageStatus', 'character', format="x(15)", initial="", max_width=30, label="Message Status", column_label="MessageStatus", position=7, order=70)
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=8, order=80)
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdateTS", column_label="UpdateTS", position=9, order=90)
        t.index('MsSeq', [['MsSeq'], ['ServSeq'], ['MessageSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CreatedTS', [['CreatedTS']], area="Dyn_Index_1")
        t.index('MessageSeq', [['MessageSeq']], area="Dyn_Index_1", unique=True)
        t.index('MessageStatus', [['MessageStatus'], ['Source']], area="Dyn_Index_1")
        t.index('UpdateTS', [['UpdateTS']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('TPServiceMessage')
