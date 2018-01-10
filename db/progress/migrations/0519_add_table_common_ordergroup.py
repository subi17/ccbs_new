from gearbox.migrations import Migration

class AddTableOrderGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('OrderGroup', area="Sta_Data_128", multitenant="yes", dump_name="ordergroup")
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", help="Extra/Additional Line Order Number", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Extra/Additional Line Order number")
        t.column('GroupId', 'integer', format=">>>>>>>>9", initial="0", help="Main Line Order Number", max_width=4, label="GroupId", column_label="GroupId", position=3, order=20, description="MainLine Order number")
        t.column('GroupType', 'character', format="x(10)", initial="", help="Group Type", max_width=16, label="GroupType", column_label="GroupType", position=4, order=30, description="Group Type")
        t.column('Info', 'character', format="x(40)", initial="", help="Info", max_width=16, label="Info", column_label="Info", position=5, order=40, description="Info")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="When was the order group created", max_width=20, label="Created", column_label="Created", position=6, order=50, description="Create timestamp")
        t.index('OrderId', [['OrderId']], area="Sta_Index_3", primary=True)
        t.index('GroupId', [['GroupId'], ['GroupType', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('OrderGroup')
