from gearbox.migrations import Migration

class AddTableCreditRate(Migration):

    database = "common"

    def up(self):
        t = self.table('CreditRate', area="Sta_Data_32", label="Credit Rating", dump_name="creditra", desc="Credit rating")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('PersonId', 'character', format="x(11)", initial="", help="Personal Number", max_width=22, label="PersonId", column_label="PersonId", position=3, order=20, description="Personal Number")
        t.column('CRReply', 'character', format="x(8)", initial="", max_width=16, label="Credit Rating Reply", column_label="Reply", position=4, order=30, help="Reply to credit rating request")
        t.column('Handler', 'character', format="x(20)", initial="", max_width=40, label="Handler", position=5, order=40, help="User id of the credit rating request handler")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Creation timestamp", max_width=20, label="Created", column_label="Created", position=6, order=50, description="Creation timestamp")
        t.index('OrderID', [['OrderId']], area="Sta_Index_2", primary=True)
        t.index('PersonId', [['PersonId'], ['CrStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CreditRate')
