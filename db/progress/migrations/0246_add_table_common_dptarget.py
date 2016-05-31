from gearbox.migrations import Migration

class AddTableDPTarget(Migration):

    database = "common"

    def up(self):
        t = self.table('DPTarget', area="Sta_Data_128", label="Discount Plan Target", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-dptarget.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-dptarget.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="dptarget")
        t.column('DPId', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Discount Plan Id", column_label="PlanId", position=2, order=10, help="Discount Plan Id")
        t.column('ValidFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Effective from date")
        t.column('ValidTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=5, order=40, help="Effective to date")
        t.column('TargetTable', 'character', format="x(20)", initial="", max_width=40, label="Target Table", column_label="Table", position=6, order=50, help="Target table")
        t.column('TargetKey', 'character', format="x(16)", initial="", max_width=32, label="Target Key", column_label="Key", position=7, order=60, help="Target key value")
        t.column('Included', 'logical', format="Yes/No", initial="yes", max_width=1, label="Included", position=8, order=70, help="Included in targets")
        t.index('DPTarget', [['DPId'], ['TargetTable'], ['TargetKey'], ['ValidTo', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('DPId', [['DPId'], ['ValidTo', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('DPTarget')
