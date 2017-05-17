from gearbox.migrations import Migration

class AddTableSMSMessage(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('SMSMessage', area="Sta_Data_64", dump_name="smsmessage")
        t.column('SMSSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="SMSSeq", column_label="SMSSeq", position=2, order=10, help="SMS Sequence")
        t.column('Custnum', 'integer', format="zzzzzzzzz", initial="0", max_width=4, label="Custnum", column_label="Custnum", position=3, order=20, help="Customer Number")
        t.column('MSISDN', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", column_label="MSISDN", position=4, order=30, help="Target MSISDN")
        t.column('DeliStatus', 'integer', format="9", initial="1", max_width=4, label="DeliStatus", column_label="DeliStatus", position=5, order=40, help="Delivery Status")
        t.column('Delitype', 'integer', format=">9", initial="0", max_width=4, label="DeliType", column_label="DeliType", position=6, order=50, help="Delivery Type")
        t.column('DeliMsg', 'character', format="x(65)", initial="", max_width=510, label="DeliMsg", column_label="DeliMsg", position=7, order=60, help="Delivery message")
        t.column('ActStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ActStamp", column_label="ActStamp", position=8, order=70, help="Activation timestamp")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreStamp", column_label="CreStamp", position=9, order=80, help="Creation timestamp")
        t.column('SMSType', 'integer', format=">9", initial="0", max_width=4, label="SMSType", column_label="SMSType", position=10, order=90, help="Credit Type")
        t.column('OrigAddress', 'character', format="x(12)", initial="", max_width=24, label="OrigAddress", column_label="OrigAddress", position=11, order=100, help="Originating number")
        t.column('DeliStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Delivered", column_label="Delivered", position=12, order=110, help="Delivery timestamp")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=13, order=120, help="Subscription ID")
        t.column('OrderId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=14, order=130, help="Order ID")
        t.index('SMSSeq', [['SMSSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('ActStamp', [['ActStamp', 'DESC']], area="Dyn_Index_1")
        t.index('Custnum', [['Custnum']], area="Dyn_Index_1")
        t.index('DeliType', [['Delitype'], ['DeliStatus'], ['ActStamp']], area="Dyn_Index_1")
        t.index('MSISDN', [['MSISDN']], area="Dyn_Index_1")
        t.index('MsSeq', [['MsSeq']], area="Dyn_Index_1")
        t.index('OrderId', [['OrderId']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('SMSMessage')
