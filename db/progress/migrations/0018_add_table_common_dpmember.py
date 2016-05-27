from gearbox.migrations import Migration

class AddTableDPMember(Migration):

    database = "common"

    def up(self):
        t = self.table('DPMember', area="Sta_Data_2_256", label="Discount Plan Member", table_trigger=[{'crc': '?', 'procedure': 'rd-dpmember.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-dpmember.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="dpmember")
        t.column('DPId', 'integer', mandatory=True, format="zzzzzzz9", initial="0", max_width=4, label="Discount Plan Id", column_label="PlanId", position=2, order=10, help="Discount Plan Id")
        t.column('HostTable', 'character', format="x(12)", initial="", max_width=24, label="Host Table", column_label="Host Table", position=3, order=20, help="Name of the host table")
        t.column('KeyValue', 'character', format="x(12)", initial="", max_width=24, label="Key Value", column_label="Key Value", position=4, order=30, help="Key value")
        t.column('DiscValue', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Discount Value", column_label="Discount Value", position=5, order=40, help="Discount amount")
        t.column('ValidFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=6, order=50, help="Effective from date")
        t.column('ValidTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=7, order=60, help="Effective to date")
        t.column('OrderId', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=8, order=70, description="Order sequence number")
        t.index('DPId', [['DPId'], ['HostTable'], ['KeyValue']], area="Sta_Index_4", primary=True)
        t.index('DPHostKey', [['HostTable'], ['KeyValue']], area="Sta_Index_4")

    def down(self):
        self.drop_table('DPMember')
