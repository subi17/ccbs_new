from gearbox.migrations import Migration

class AddFieldPRO(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderCustomer')
        t.column('AddressId', 'character', format="x(30)", initial="", max_width=60, label="AddressId", column_label="AddressId", position=77, order=1160, help="Address Id")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.drop_column('AddressId')
