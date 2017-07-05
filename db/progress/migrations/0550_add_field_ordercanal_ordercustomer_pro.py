from gearbox.migrations import Migration

class AddFieldPRO(Migration):

    database = "ordercanal"
    
    def up(self):
        t = self.alter_table('OrderCustomer')
        t.column('PRO', 'logical', format="yes/no", initial="no", label="PRO", column_label="PRO", position=76, order=1150, help="PRO customer")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.drop_column('PRO')
