from gearbox.migrations import Migration

class AddFieldTerritoryOwner(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('TermMobSub')
	t.column('TerritoryOwner', 'character', format="x(15)", initial="", max_width=16, label="TerritoryOwner", column_label="TerritoryOwner", position=82, order=920, help="Territory Owner")

    def down(self):
        t = self.alter_table('TermMobSub')
        t.drop_column('TerritoryOwner')
