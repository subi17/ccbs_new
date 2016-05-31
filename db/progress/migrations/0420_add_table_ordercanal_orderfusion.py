from gearbox.migrations import Migration

class AddTableOrderFusion(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderFusion', area="Sta_Data_32", label="Order Fusion", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-orderfusion.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-orderfusion.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="orderfusion", desc="Order Fusion")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10)
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=3, order=20, description="Order sequence number")
        t.column('FusionStatus', 'character', format="x(8)", initial="", max_width=16, label="Fusion Order Status", column_label="FusionStatus", position=4, order=30, description="Status of order")
        t.column('FixedNumberType', 'character', format="x(8)", initial="", max_width=16, label="Fixed Number Type", column_label="FixedNumberType", position=5, order=40, description="Fixed line order type NEW/MNP")
        t.column('FixedNumber', 'character', format="x(12)", initial="", max_width=24, label="Fixed Number", column_label="FixedNumber", position=6, order=50)
        t.column('Product', 'character', format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=7, order=60)
        t.column('FixedCurrOper', 'character', format="x(16)", initial="", help="Item type", max_width=32, label="Fixed Current Operator", column_label="CurrOper", position=8, order=70, description="Current operator")
        t.column('FixedOrderId', 'character', format="x(15)", initial="", max_width=30, label="Fixed Order Id", column_label="FixedOrderId", position=9, order=80)
        t.column('FixedStatus', 'character', format="x(15)", initial="", max_width=30, label="Fixed Status", column_label="FixedStatus", position=10, order=90, description="Fixed line order status code")
        t.column('FixedSubStatus', 'character', format="x(15)", initial="", max_width=30, label="Fixed Sub-status", column_label="FixedSubStatus", position=11, order=100, description="Fixed line order sub-status code")
        t.column('ExternalTicket', 'character', format="x(15)", initial="", max_width=30, label="External Ticket", column_label="ExternalTicket", position=12, order=110, description="External order ticket number")
        t.column('OrderDate', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="OrderDate", column_label="OrderDate", position=13, order=120, description="Order date")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=14, order=130)
        t.column('FixedMNPTime', 'character', format="x(16)", initial="", max_width=32, label="Fixed MNP Time", column_label="FixedMNPTime", position=15, order=140, description="Fixed line MNP time")
        t.column('CustomerType', 'character', format="x(12)", initial="", max_width=24, label="Customer Type", column_label="CustomerType", position=16, order=150, description="Fixed line customer type")
        t.column('PhoneBook', 'logical', format="Yes/No", initial="no", max_width=1, label="PhoneBook", column_label="PhoneBook", position=17, order=160)
        t.column('FixedContractID', 'character', format="x(12)", initial="", max_width=24, label="Contract ID", column_label="ContractID", position=18, order=170)
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, position=19, order=180)
        t.index('OrderId', [['Brand'], ['OrderId']], area="Dyn_Index_1", primary=True)
        t.index('FixedNumber', [['FixedNumber']], area="Dyn_Index_1")
        t.index('FixedOrderId', [['FixedOrderId']], area="Dyn_Index_1")
        t.index('FusionStatus', [['FusionStatus'], ['OrderDate', 'DESC']], area="Dyn_Index_1")
        t.index('Salesman', [['Salesman'], ['FusionStatus']], area="Dyn_Index_1")
        t.index('UpdateTS', [['UpdateTS']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('OrderFusion')
