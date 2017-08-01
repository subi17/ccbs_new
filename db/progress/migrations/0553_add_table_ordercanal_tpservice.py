from gearbox.migrations import Migration

class AddTableTPService(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TPService', area="Sta_Data_32", label="Third Party Service", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-tpservice.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-tpservice.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="tpservice", desc="Third Party Service")
        
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=1, order=10, description="contract sequence number")
        t.column('ServSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="ServSeq", column_label="ServSeq", position=2, order=20, description="service sequence number")
        t.column('ServType', 'character', format="X(15)", initial="", max_width=30, label="ServType", column_label="ServType", position=3, order=30, help="Service type")
        t.column('Product', 'character', format="X(25)", initial="", max_width=50, label="Product", column_label="Product", position=4, order=40, help="Third party product")
        t.column('Provider', 'character', format="X(20)", initial="", max_width=40, label="Service Provider", column_label="Service Provider", position=5, order=50, help="Third party service provider")
        t.column('SerialNbr', 'character', format="x(25)", initial="", max_width=50, label="Serial Nbr", column_label="Serial Nbr", position=6, order=60, help="Device serial number")
        t.column('Status', 'character', format="x(15)", initial="", max_width=16, label="Order Status", column_label="Status", position=7, order=70, help="Service Status")
        t.column('TermReason', 'character', format="x(40)", initial="", max_width=80, label="TermReason", column_label="TermReason", position=8, order=80, help="Termination reason")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=9, order=90, help="CreatedTS")
        t.column('UpdatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdatedTS", column_label="UpdatedTS", position=10, order=100, help="UpdatedTS")
         
        t.index('ServSeq', ['ServSeq'], primary=True, Unique=True, area="Dyn_Index_1")
        t.index('MsSeq', [['MsSeq'], ['Product']], area="Dyn_Index_1", area="Dyn_Index_1")
        t.index('SerialNbr', ['SerialNbr'], area="Dyn_Index_1")
        t.index('CreatedTS', [['CreatedTS', 'DESC'], ['Status']], area="Dyn_Index_1")
        t.index('UpdatedTS', ['UpdatedTS', 'DESC'], area="Dyn_Index_1")

    def down(self):
        t = self.drop_table('TPService')
        t.drop_column('MsSeq')
        t.drop_column('ServSeq')
        t.drop_column('Product')
        t.drop_column('ServType')
        t.drop_column('Provider')
        t.drop_column('Status')
        t.drop_column('SerialNbr')
        t.drop_column('TermReason')
        t.drop_column('CreatedTS')
        t.drop_column('UpdatedTS') 
