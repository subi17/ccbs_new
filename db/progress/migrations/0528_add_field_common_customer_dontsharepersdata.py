from gearbox.migrations import Migration

class AddFieldDontSharePersData(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Customer')
        t.column('DontSharePersData', 'logical', format="Yes/No", initial="No", max_width=1, label="DontSharePersData", column_label="DontSharePersData", position=125, order=1590, help="Do Not Share Personal Data among Mas Movil Group")

    def down(self):
        t = self.alter_table('Customer')
        t.drop_column('DontSharePersData')
