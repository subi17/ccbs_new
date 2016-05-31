from gearbox.migrations import Migration

class AddTableDMSDOC(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('DMSDOC', area="Sta_Data_128", label="Document", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-dmsdoc.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-dmsdoc.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="dmsdoc", desc="DMS Document")
        t.column('DMSID', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="DMSID", position=2, order=10, help="DMS ID")
        t.column('DocTypeID', 'character', format="X(2)", initial="", max_width=4, label="Document Type", position=3, order=20, help="Document Type ID")
        t.column('DocStatusCode', 'character', format="X(8)", initial="", max_width=16, label="Document Status", position=4, order=30, help="Document Status Code")
        t.column('DocTypeDesc', 'character', format="X(20)", initial="", max_width=40, label="Doc Type Desc", position=5, order=35, help="Document Type Description")
        t.column('DocRevComment', 'character', format="X(30)", initial="", max_width=60, label="Doc Revision Comment", position=6, order=45, help="Document Revision Comment")
        t.column('DocStatusTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", column_label="DocStatusTS", position=7, order=50, help="Document Status Time Stamp")
        t.column('DMSStatusTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="DMS Time Stamp", column_label="DMSStatusTS", position=8, order=60, help="DMS Time Stamp")
        t.index('DMSID', [['DMSID']], area="Sta_Index_1", primary=True)
        t.index('StatusTS', [['DocStatusTS', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('DMSDOC')
