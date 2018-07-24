from gearbox.migrations import Migration

class AddTableOrderMobile(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderMobile', area="Sta_Data_64", multitenant="yes", dump_name="OrderMobile", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-ordermobile.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-ordermobile.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}])
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('OrderId',         'integer',     format=">>>>>>>>9",  initial="0", max_width=4, label="OrderId", column_label="OrderId", position=3, order=20, description="Order Number", help="OrderId")
        t.column('OrderProductID',  'integer',     format=">>>>>>>>9",  initial="0", max_width=4, label="ProductID", column_label="ProductID", position=4, order=30, description="Order Product ID", help="ProductID")
        t.column('MsSeq',           'integer',     format=">>>>>>>9",   initial="0", max_width=4, label="SubscrID", column_label="SubscrID", position=5, order=40, description="Subscription ID",help="Subscription ID" )
        t.column('ICC',             'character',   format="x(20)",      initial="", max_width=40, label="SIM Serial", column_label="ICC", position=6, order=50, description="SIM Card Serial No.", help="SIM Card Serial No.")
        t.column('Product',         'character',   format="x(16)",      initial="", max_width=16, label="Product", column_label="Product", position=7, order=60, description="Product", help="Product")
        t.column('NumberType',      'character',   format="x(8)",       initial="", max_width=16, label="NumberType", column_label="NumberType", position=8, order=70, description="NumberType", help="NumberType")
        t.column('CurrOper',        'character',   format="x(16)",      initial="", max_width=32, label="Current Operator", column_label="Current Operator", position=9, order=80, description="Current Operator", help="Current Operator")
        t.column('StatusCode',      'character',   format="x(8)",       initial="", max_width=16, label="Status", column_label="Status", position=10, order=90, description="Subscription Status Code", help="Status Code")
        t.column('CreatedTS',       'decimal',     format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation Time", column_label="Creation Time", position=11, order=100, description="Creation Time", help="Creation Time")
        t.column('UpdatedTS',       'decimal',     format="99999999.99999", decimals=5, initial="0", max_width=17, label="Updated Time", column_label="Updated Time", position=12, order=110, description="Update Stamp", help="Update Stamp")
        t.column('CLI',             'character',   format="x(12)",      initial="", max_width=24, label="MSISDN", column_label="MSISDN", position=13, order=120, description="Mobile Number", help="Mobile Number")        
        t.column('ActivatedTS',     'decimal',     format="99999999.99999", decimals=5, initial="0", max_width=20, label="Activated Time", column_label="Activated Time", position=14, order=130, description="Activated TimeStamp", help="Time when subscription was activated in TMS (HLR response OK)")
        t.column('RequestedPortingDate','date',    format="99-99-9999", initial="?", max_width=4, label="PortingDate", column_label="PortingDate", position=15, order=140, description="PortingDate", help="PortingDate")
        t.column('PortingTime',     'decimal',     format="99.99", decimals=2, initial="0", max_width=17, label="PortingDate", column_label="PortingDate", position=16, order=150, description="PortingDate", help="PortingDate")
        t.column('MNPStatus',       'integer',     format="99", initial="0", max_width=4, label="MNPStatus", column_label="MNPStatus", position=17, order=160, description="MNPStatus",help="MNPStatus")
        t.column('OldICC',          'character',   format="x(20)", initial="", max_width=40, label="OldICC", column_label="OldICC", position=18, order=170, description="OldICC", help="OldICC")
        t.column('OldPayType',      'logical',     format="PostPaid/PrePaid", initial="no", max_width=1, label="OldPayType", column_label="OldPayType", position=19, order=180, description="OldPayType", help="OldPayType")
        t.column('PayType',         'logical',     format="PostPaid/PrePaid", initial="no", max_width=1, label="PayType", column_label="PayType", position=20, order=190, description="PayType", help="PayType")       
        t.index('OrderID', [['Brand'], ['OrderId']], area="Sta_Index_64", description="OrderId",  primary=True, unique=True)
        t.index('OrderProductID', [['Brand'], ['OrderID'], ['OrderProductID']], area="Sta_Index_64", description="ProductId")
        t.index('MSSeq', [['MSSeq']], area="Sta_Index_64", description="SubscriptionID")

    def down(self):
        self.drop_table('OrderMobile')
