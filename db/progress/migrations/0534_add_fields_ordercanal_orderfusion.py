from gearbox.migrations import Migration

class AddFieldsOrderFusion(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderFusion')
        t.column('MsSeq',           'integer',     format=">>>>>>>9",   initial="0", max_width=4, label="SubscrID", column_label="SubscrID", position=33, order=320, description="Subscription ID",help="Subscription ID" )
        t.column('CreatedTS',       'decimal',     format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation Time", column_label="Creation Time", position=34, order=330, description="Creation Time", help="Creation Time")
        t.column('OrderProductID',  'integer',     format=">>>>>>>>9", initial="0", max_width=4, label="OrderProductID", column_label="OrderProductID", position=35, order=340, description="Order product internal id")
        t.index('MsSeq', [['MSSeq']], area="Sta_Index_64", description="SubscriptionID")

    def down(self):
        t = self.alter_table('OrderFusion')
        t.drop_index('MsSeq')
        t.drop_column('MsSeq')
        t.drop_column('CreatedTS')
        t.drop_column('OrderProductID')
