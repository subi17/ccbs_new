from gearbox.migrations import Migration

class AddTableOrderSubscription(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderSubscription', area="Sta_Data_64", multitenant="yes", dump_name="OrderSubscription", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-ordersubscription.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-ordersubscription.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}])
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order Number", help="OrderId")
        t.column('OrderProductID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="ProductID", column_label="ProductID", position=3, order=20, description="Order Product ID", help="ProductID")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="SubscrID", column_label="SubscrID", position=4, order=30, description="Subscription ID",help="Subscription ID" )
        t.column('ICC', 'character', format="x(20)", initial="", max_width=40, label="SIM Serial", column_label="ICC", position=5, order=40, description="SIM Card Serial No.", help="SIM Card Serial No.")
        t.column('CLIType', 'character', format="x(16)", initial="", max_width=16, label="CLIType", column_label="CLIType", position=6, order=50, description="CLI Type", help="CLI Type")
        t.column('NumberType', 'character', format="x(8)", initial="", max_width=16, label="NumberType", column_label="NumberType", position=7, order=60, description="NumberType", help="NumberType")
        t.column('CurrOper', 'character', format="x(16)", initial="", max_width=32, label="Current Operator", column_label="Current Operator", position=8, order=70, description="Current Operator", help="Current Operator")
        t.column('StatusCode', 'character', format="x(8)", initial="", max_width=16, label="Status", column_label="Status", position=9, order=80, description="Subscription Status Code", help="Status Code")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation Time", column_label="Creation Time", position=10, order=90, description="Creation Time", help="Creation Time")
        t.column('UpdatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Updation Time", column_label="Updation Time", position=11, order=100, description="Update Stamp", help="Update Stamp")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", column_label="MSISDN", position=12, order=110, description="Mobile Number", help="Mobile Number")        
        t.column('FixedNumber', 'character', format="X(12)", initial="", max_width=24, label="Fixed Number", column_label="Fixed Number", position=13, order=120, description="Fixed Number", help="Fixed Number")
        t.index('OrderID', [['OrderId']], area="Sta_Index_64", primary=True)
        t.index('OrderProductID', [['OrderId'], ['OrderProductID']], area="Sta_Index_64")
        t.index('MSSeq', [['MSSeq']], area="Sta_Index_64")

    def down(self):
        self.drop_table('OrderSubscription')
