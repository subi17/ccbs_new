from gearbox.migrations import Migration

class AddTableDMS(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('DMS', area="Sta_Data_128", label="Document mgmt case", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-dms.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-dms.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="dms", desc="Document management case")
        t.column('HostID', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Host ID", position=2, order=10, help="TMS source ID")
        t.column('HostTable', 'character', format="X(12)", initial="", max_width=24, label="Host Table", position=3, order=20, help="TMS Table Information")
        t.column('DMSID', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="DMSID", position=4, order=30, help="DMS ID")
        t.column('DmsExternalID', 'character', format="X(12)", initial="", max_width=24, label="ID in DMS", position=5, order=40, help="ID from external DMS system")
        t.column('StatusCode', 'character', format="X(12)", initial="", max_width=24, label="Status Code", position=6, order=50, help="Status Code")
        t.column('StatusDesc', 'character', format="X(20)", initial="", max_width=40, label="Status Description", position=7, order=60, help="Status Description")
        t.column('StatusTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", column_label="StatusTS", position=8, order=70, help="Status Time Stamp")
        t.column('CaseTypeID', 'character', format="X(8)", initial="", max_width=16, label="Case Type", position=9, order=80, help="Case Type ID")
        t.column('ContractID', 'character', format="X(8)", initial="", max_width=16, label="ContractID", position=10, order=90, help="ContractID")
        t.column('OrderStatus', 'character', format="X(8)", initial="", max_width=16, label="OrderStatus", position=11, order=100, help="Order Status in TMS")
        t.column('DMSStatusTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="DMS Status TS", column_label="DMSStatusTS", position=12, order=110, help="DMS Time Stamp")
        t.index('DMSID', [['DMSID']], area="Sta_Index_1", primary=True)
        t.index('ContractID', [['ContractID']], area="Sta_Index_1")
        t.index('Host', [['HostTable'], ['HostID'], ['StatusTS', 'DESC']], area="Sta_Index_1")
        t.index('StatusTS', [['StatusTS', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('DMS')
