from gearbox.migrations import Migration

class AddTableShaperConf(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ShaperConf', area="Sta_Data_64", label="ShaperConf", dump_name="shaperconf", desc="SHAPER command parameters")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ShaperConfID', 'character', format="x(20)", initial="", max_width=40, label="ShaperConfID", position=3, order=20, help="Shaper configuration ID")
        t.column('Template', 'character', format="x(20)", initial="", max_width=40, label="Template", position=4, order=30)
        t.column('TariffType', 'character', format="x(20)", initial="", max_width=40, label="TariffType", position=5, order=40)
        t.column('Tariff', 'character', format="x(25)", initial="", max_width=50, label="Tariff", position=6, order=50)
        t.column('LimitUnshaped', 'decimal', format=">>>>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="LimitUnshaped", position=7, order=60)
        t.column('LimitShaped', 'decimal', format=">>>>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="LimitShaped", position=8, order=70)
        t.column('Active', 'logical', format="yes/no", initial="yes", max_width=1, label="Active", position=9, order=80, help="Is configuration active")
        t.index('ShaperConfID', [['Brand'], ['ShaperConfID']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('ShaperConf')
