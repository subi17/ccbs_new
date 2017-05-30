from gearbox.migrations import Migration

class AddTableRTItem(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RTItem', area="Sta_Data_256", dump_name="roamgprs")
        t.column('TariffNum', 'integer', format=">>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('StepNum', 'integer', format=">9", initial="0", max_width=4, label="StepNumber", column_label="StepNumber", position=3, order=20)
        t.column('Unit', 'character', format="x(3)", initial="", max_width=6, label="Unit", column_label="Unit", position=4, order=30)
        t.column('SizeLimit', 'integer', format=">>>>>9", initial="0", max_width=4, label="SizeLimit", column_label="SizeLimit", position=5, order=100)
        t.column('Tariff', 'decimal', format=">>9.999", decimals=7, initial="0", max_width=20, label="Tariff", column_label="Tariff", position=6, order=50)
        t.column('StepSize', 'integer', format=">>>>>9", initial="0", max_width=4, label="StepSize", column_label="StepSize", position=7, order=40)
        t.index('RTItem', [['TariffNum'], ['StepNum']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('RTItem')
