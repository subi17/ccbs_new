from gearbox.migrations import Migration

class AddTableRoamTariff(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RoamTariff', area="Sta_Data_256", dump_name="roamtari")
        t.column('TariffNum', 'integer', format=">>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="PriceList", column_label="PriceList", position=3, order=20)
        t.column('RoamGroup', 'character', format="x(8)", initial="", max_width=16, label="RoamGroup", column_label="RoamGroup", position=4, order=30)
        t.column('Service', 'character', format="x(8)", initial="", max_width=16, label="Service", column_label="Service", position=5, order=40)
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="From", position=6, order=50, help="Valid from")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="To", position=7, order=60, help="Valid to")
        t.column('RateType', 'integer', format=">9", initial="0", max_width=4, label="Type", column_label="Type", position=8, order=70)
        t.column('Tariff', 'decimal', format=">>9.999", decimals=5, initial="0", max_width=20, label="Tariff", column_label="Tariff", position=9, order=80)
        t.column('RoamingType', 'integer', format="9", initial="0", max_width=4, label="RoamingType", column_label="RoamingType", position=10, order=90)
        t.index('RoamTariff', [['TariffNum']], area="Sta_Index_1", primary=True)
        t.index('PriceList', [['PriceList'], ['ValidFrom', 'DESC'], ['ValidTo', 'DESC'], ['RateType'], ['RoamingType'], ['Service']], area="Sta_Index_1")
        t.index('RateType', [['RateType'], ['PriceList'], ['ValidFrom', 'DESC'], ['ValidTo', 'DESC'], ['RoamingType'], ['Service']], area="Sta_Index_1")

    def down(self):
        self.drop_table('RoamTariff')
