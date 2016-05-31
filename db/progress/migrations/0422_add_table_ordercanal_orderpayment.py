from gearbox.migrations import Migration

class AddTableOrderPayment(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderPayment', area="Sta_Data_32", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-orderpayment.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-orderpayment.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="orderpay")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('Method', 'integer', format=">>9", initial="0", max_width=4, label="Method", column_label="Method", position=3, order=20)
        t.column('CCName', 'character', format="x(20)", initial="", max_width=40, label="CreditCard Name", column_label="CreditCard Name", position=4, order=30)
        t.column('CCNumber', 'character', format="x(20)", initial="", max_width=40, label="CreditCard Number", column_label="CreditCard Number", position=5, order=40)
        t.column('CCValid', 'character', format="x(20)", initial="", max_width=40, label="CreditCard Valid", column_label="CreditCard Valid", position=6, order=50)
        t.column('PaymentValid', 'logical', format="Yes/No", initial="no", max_width=1, label="Payment Validation", column_label="Payment Validation", position=7, order=60)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('CCReference', 'character', format="x(12)", initial="", max_width=24, label="Credit Card Reference", column_label="CC Reference", position=9, order=80, help="Credit card reference")
        t.column('AuthNumber', 'character', format="x(20)", initial="", max_width=40, label="Authorization Number", column_label="Authorization Number", position=10, order=90, description="Authorization Number")
        t.column('BinNumber', 'character', format="x(8)", initial="", max_width=16, label="BIN Number", column_label="BIN Number", position=11, order=100, description="BIN Number")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('OrderPayment')
