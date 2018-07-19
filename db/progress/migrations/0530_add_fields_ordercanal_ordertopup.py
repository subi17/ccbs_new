from gearbox.migrations import Migration

class AddFieldOrderTopup(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderTopUp')
        t.column('OrderProductID', 'integer', format=">>>>>>9", initial="0", label="Product ID", column_label="Product ID", position=9, max_width=4, order=60, help="Product ID")
        t.column('DisplayAmount', 'decimal', format=">>>>9.99", decimals=2, initial="0", max_width=17, label="DispAmt", column_label="DispAmt", position=7, order=70, help="Disp Amount")
        t.column('TopUpType', 'integer', format=">>>>>>9", initial="0", label="Type", column_label="Type", position=80, max_width=4, order=80, help="Topup Type")
        t.column('MsSeq', 'integer', format=">>>>>>9", initial="0", label="SubscrID", column_label="SubscrID", position=90, max_width=4, order=90, help="SubscrID")
        
    def down(self):
        t = self.alter_table('OrderTopUp')
        t.drop_column('OrderProductID')
        t.drop_column('DisplayAmount')
        t.drop_column('TopUpType')
        t.drop_column('MsSeq')