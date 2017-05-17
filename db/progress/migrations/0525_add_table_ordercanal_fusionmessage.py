from gearbox.migrations import Migration

class AddTableFusionMessage(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('FusionMessage', area="Sta_Data_64", label="Fusion Message", dump_name="fusionmessage", desc="Contains messages between TMS and MasMovil")
        t.column('MessageSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MessageSeq", column_label="MessageSeq", position=2, order=10)
        t.column('MessageID', 'character', format="x(37)", initial="", max_width=74, label="MessageId", column_label="MessageID", position=3, order=20, description="Auto-generated UUID")
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=4, order=30, description="Order sequence number")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subs.ID", position=5, order=40, help="Subscription ID")
        t.column('MessageType', 'character', format="x(16)", initial="", max_width=32, label="Message Type", column_label="MessageType", position=6, order=50)
        t.column('Source', 'character', format="x(8)", initial="", max_width=16, label="Message Source", column_label="Source", position=7, order=60, description="Messsage source (TMS/Masmovil)")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=8, order=70)
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdateTS", column_label="UpdateTS", position=9, order=80)
        t.column('MessageStatus', 'character', format="x(8)", initial="", max_width=16, label="Message Status", column_label="MessageStatus", position=10, order=90)
        t.column('OrderType', 'character', format="x(30)", initial="", max_width=60, label="Order Type", column_label="OrderType", position=11, order=100)
        t.column('FixedStatus', 'character', format="x(15)", initial="", max_width=30, label="Fixed Status", column_label="FixedStatus", position=12, order=110)
        t.column('FixedStatusTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Fixed Status Timestamp", column_label="FixedStatusTS", position=13, order=120)
        t.column('FixedStatusDesc', 'character', format="x(45)", initial="", max_width=90, label="Fixed Status Desc", column_label="FixedStatusDesc", position=14, order=130)
        t.column('AdditionalInfo', 'character', format="x(40)", initial="", max_width=80, label="Additional Info", column_label="AdditionalInfo", position=15, order=140, description="Additional status info")
        t.column('ResponseCode', 'character', format="x(10)", initial="", max_width=20, label="Response Code", column_label="ResponseCode", position=16, order=150, description="Response code from Adapter/Masmovil")
        t.index('MessageSeq', [['MessageSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('MessageStatus', [['Source'], ['MessageStatus']], area="Dyn_Index_1")
        t.index('OrderId', [['OrderId'], ['CreatedTS', 'DESC']], area="Dyn_Index_1")
        t.index('UpdateTS', [['UpdateTS', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('FusionMessage')
