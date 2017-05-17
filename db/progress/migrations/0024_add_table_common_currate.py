from gearbox.migrations import Migration

class AddTableCurRate(Migration):

    database = "common"

    def up(self):
        t = self.table('CurRate', area="Sta_Data_2_256", label="Exchange Rates", dump_name="currate", desc="Currency exchange rates")
        t.column('Currency', 'character', format="x(5)", initial="", max_width=10, label="Code", column_label="Code", position=2, order=10, help="Currency code")
        t.column('ExchRate', 'decimal', format=">>9.999999", decimals=6, initial="0", max_width=21, label="Rate", column_label="Rate", position=4, order=30, help="Currency rate")
        t.column('RateDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=5, order=40, help="Date when updated")
        t.index('Currency', [['Currency'], ['RateDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)
        t.index('RateDate', [['RateDate', 'DESC'], ['Currency']], area="Sta_Index_3")

    def down(self):
        self.drop_table('CurRate')
