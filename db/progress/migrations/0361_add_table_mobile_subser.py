from gearbox.migrations import Migration

class AddTableSubSer(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SubSer', area="SubSer_Data", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-subser.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-subser.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="subser-------1", desc="Services of mobile subscribers")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('ServPac', 'character', format="x(8)", initial="", max_width=16, label="ServPackage", column_label="ServPack", position=3, order=20, help="Code of ServPack")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Service Component", position=5, order=40, help="Code of Service Component")
        t.column('SSAData', 'character', format="x(40)", initial="", max_width=410, label="Add'l Data", column_label="Additional Data", extent=5, position=6, order=50, help="Additional Data")
        t.column('SSDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=7, order=60, help="Date When Activated")
        t.column('SSStat', 'integer', format=">>>9", initial="0", max_width=4, label="Status", column_label="Status", position=8, order=90, help="Service Status")
        t.column('SSParam', 'character', format="x(24)", initial="", max_width=48, label="Parameter", column_label="Parameter", position=9, order=80, help="Service-oriented, subscriber-specific parameter")
        t.column('SologStat', 'integer', format="9", initial="0", help="Solog status of service (sent to HLR)", max_width=4, label="Solog Status", column_label="HLR", position=10, order=100, description="0=no need to send, 1=should be sent, 2=sent (solog created)")
        t.index('ServCom', [['MsSeq'], ['ServCom'], ['SSDate', 'DESC']], area="Sta_Index_4", primary=True, unique=True)
        t.index('ServPac', [['MsSeq'], ['ServPac'], ['ServCom'], ['SSDate', 'DESC']], area="Sta_Index_5")

    def down(self):
        self.drop_table('SubSer')
