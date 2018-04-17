from gearbox.migrations import Migration

class AddFieldCountryISO3(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Country')
        t.column('CountryISO3', 'character', format="x(3)", initial="", label="Country ISO-3", column_label="Country ISO-3", position=5, max_width=6, order=40, help="ISO-3 country code")

    def down(self):
        t = self.alter_table('Country')
        t.drop_column('CountryISO3')

