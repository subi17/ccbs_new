from gearbox.migrations import Migration

class AddFieldDeliveryType(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('MNPCal')
        t.column('DeliveryType', 'integer', format=">>9", initial="0", max_width=4, label="Delivery Type", column_label="DelType", position=8, order=70, help="Delivery Type")

    def down(self):
        t = self.alter_table('MNPCal')
        t.drop_column('DeliveryType')
