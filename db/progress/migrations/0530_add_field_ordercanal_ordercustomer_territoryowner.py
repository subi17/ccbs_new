from gearbox.migrations import Migration

class AddFieldTerritoryOwner(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderCustomer')
	t.column('TerritoryOwner', 'character', format="x(15)", initial="", max_width=16, label="TerritoryOwner", column_label="TerritoryOwner", position=74, order=1130, help="Territory Owner")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.drop_column('TerritoryOwner')
