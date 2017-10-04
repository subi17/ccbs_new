from gearbox.migrations import Migration

class AddFieldDontSharePersData(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderCustomer')
        t.column('DontSharePersData', 'logical', format="Yes/No", initial="no", max_width=1, label="DontSharePersData", column_label="DontSharePersData", position=78, order=1170, help="Do Not Share Personal Data among Mas Movil Group")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.drop_column('DontSharePersData')

