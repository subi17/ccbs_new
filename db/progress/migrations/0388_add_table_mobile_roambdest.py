from gearbox.migrations import Migration

class AddTableRoamBDest(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RoamBDest', area="Sta_Data_256", dump_name="roambdes")
        t.column('TariffNum', 'integer', format=">>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('BDest', 'character', format="x(12)", initial="", max_width=24, label="BDest", column_label="BDest", position=3, order=20)
        t.column('TZFrom', 'character', format="99:99", initial="", max_width=10, label="From", column_label="From", position=4, order=30)
        t.column('TZTo', 'character', format="99:99", initial="", max_width=10, label="To", column_label="To", position=5, order=40)
        t.column('Tariff', 'decimal', format=">>9.999", decimals=5, initial="0", max_width=20, label="Tariff", column_label="Tariff", position=6, order=50)
        t.index('RoamBDest', [['TariffNum'], ['TZFrom'], ['TZTo']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('RoamBDest')
