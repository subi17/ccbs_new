from gearbox.migrations import Migration

class AddTableTPService(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TPService', area="Sta_Data_32", label="Third Party Service", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-tpservice.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-tpservice.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="tpservice", desc="Third Party Service")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=2, order=20, help="Sequence for a Subscription")
        t.column('ServSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="ServSeq", column_label="ServSeq", position=3, order=30, help="Service Sequence")
        t.column('ServType', 'character', format="x(15)", initial="", help="Service Type", max_width=30, label="Type", column_label="Type", position=4, order=40, description="Service Type")
        t.column('Operation', 'character', format="x(15)", initial="", help="Operation", max_width=30, label="Operation", column_label="Operation", position=5, order=50, description="Operation")
        t.column('Product', 'character', format="x(25)", initial="", max_width=50, label="Product", position=6, order=60, help="Product")
        t.column('Provider', 'character', format="x(20)", initial="", max_width=40, label="Provider", column_label="Provider", position=7, order=70, help="Service Provider")
        t.column('SerialNbr', 'character', format="x(25)", initial="", max_width=50, label="Serial Nbr", column_label="Serial Nbr", position=8, order=80, help="Serial Number")
        t.column('ServStatus', 'character', format="x(15)", initial="", max_width=30, label="Status", column_label="Status", position=9, order=90, help="Service Status")
        t.column('Offer', 'character', format="x(12)", initial="", max_width=24, label="Offer", column_label="Offer", position=10, order=100, help="Offer")
        t.column('UserCode', 'character', format="x(10)", initial="", max_width=20, label="User", column_label="User", position=11, order=110, help="User")
        t.column('TermReason', 'character', format="x(35)", initial="", max_width=70, label="TermReason", column_label="TermReason", position=12, order=120, help="Termination Reason")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=13, order=130, help="CreatedTS")
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdatedTS", column_label="UpdateTS", position=14, order=140, help="UpdateTS")
        t.column('MessageID', 'character', format="x(35)", initial="", max_width=70, label="MessageId", column_label="MessageID", position=15, order=150, description="Auto-generated")
        t.column('ResponseCode', 'character', format="x(10)", initial="", max_width=20, label="Response Code", column_label="ResponseCode", position=16, order=160, description="Response code from Third party")
        t.column('AdditionalInfo', 'character', format="x(40)", initial="", max_width=80, label="Additional Info", column_label="AdditionalInfo", position=17, order=170, description="Additional status info")
        t.index('MsSeq', [['MsSeq'], ['ServSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CreatedTS', [['CreatedTS'], ['ServStatus']], area="Dyn_Index_1")
        t.index('MessageId', [['MessageID']], area="Dyn_Index_1")
        t.index('MsSeqTypeStatus', [['MsSeq'], ['Operation'], ['ServType'], ['ServStatus']], area="Dyn_Index_1")
        t.index('SerialNbr', [['SerialNbr']], area="Dyn_Index_1")
        t.index('ServSeq', [['ServSeq']], area="Dyn_Index_1", unique=True)
        t.index('UpdateTS', [['UpdateTS']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('TPService')
