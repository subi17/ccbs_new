from gearbox.migrations import Migration

class AddTableRoamCountry(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RoamCountry', area="Sta_Data_256", dump_name="roamcoun")
        t.column('CountryName', 'character', format="x(16)", initial="", max_width=32, label="CountryName", column_label="CountryName", position=2, order=30)
        t.column('RateZone', 'integer', format=">9", initial="0", max_width=4, label="Zone", column_label="Zone", position=3, order=20)
        t.column('Prefix', 'character', format="x(5)", initial="", max_width=10, label="Prefix", column_label="Prefix", position=4, order=10)
        t.index('RoamCountry', [['Prefix']], area="Sta_Index_1", primary=True)
        t.index('RateZone', [['RateZone'], ['Prefix']], area="Sta_Index_1")

    def down(self):
        self.drop_table('RoamCountry')
