from gearbox.migrations import Migration

class AddTableOrderTPService(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderTPService', area="Sta_Data_32", label="Order Third Party Service", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-ordertpservice.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-ordertpservice.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="ordertpservice", desc="Order Third Party Service")
        
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=1, order=10, description="Order sequence number")
        t.column('Product', 'character', format="X(25)", initial="", max_width=50, label="Product", column_label="Product", position=2, order=20, help="Third party product")
        t.column('Type', 'character', format="X(15)", initial="", max_width=30, label="Type", column_label="Type", position=3, order=30, help="Third party product type")
        t.column('Provider', 'character', format="X(20)", initial="", max_width=40, label="Service Provider", column_label="Service Provider", position=4, order=40, help="Third party service provider")
        t.column('Status', 'character', format="x(8)", initial="", max_width=16, label="Order Status", column_label="Status", position=5, order=50, description="Status of third party service")
        t.column('SerialNumber', 'character', format="x(25)", initial="", max_width=50, label="SerialNumber", position=6, order=60)
        t.column('TermReason', 'character', format="x(40)", initial="", max_width=80, label="Cancellation Reason", column_label="CancellationReason", position=7, order=70, description="Cancellation reason")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", position=8, order=80)
        t.column('UpdatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdatedTS", position=10, order=100)
         
       t.index('OrderId', ['OrderId'], area="Dyn_Index_1", primary=True)
       t.index('Status', [['Status'], ['CreatedTS', 'DESC']], area="Dyn_Index_1")
       t.index('UpdatedTS', [['UpdatedTS', 'DESC']], area="Dyn_Index_1")

    def down(self):
        t = self.drop_table('OrderTPService')
        t.drop_column('OrderId')
        t.drop_column('Product')
        t.drop_column('Type')
        t.drop_column('Provider')
        t.drop_column('Status')
        t.drop_column('SerialNumber')
        t.drop_column('TermReason')
        t.drop_column('CreatedTS')
        t.drop_column('UpdatedTS') 
