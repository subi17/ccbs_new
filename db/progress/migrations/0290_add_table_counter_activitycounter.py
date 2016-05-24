from gearbox.migrations import Migration

class AddTableActivityCounter(Migration):

    database = "counter"

    def up(self):
        t = self.table('ActivityCounter', area="Sta_Data_256", dump_name="activitycounter")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('AcDate', 'date', format="99-99-9999", max_width=4, label="ActDate", column_label="ActDate", position=3, order=20)
        t.column('ACValue', 'character', format="x(8)", initial="", max_width=16, label="ACValue", column_label="ACValue", position=4, order=30)
        t.column('ACSubValue', 'character', format="x(8)", initial="", max_width=16, label="ACSubValue", column_label="ACSubValue", position=5, order=40)
        t.column('DecValue', 'decimal', format="->>>>>>>>9.9999", decimals=6, initial="0", max_width=21, label="Decimal Value", column_label="Dec", position=6, order=50, help="Decimal value")
        t.column('IntValue', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Value", column_label="Int", position=7, order=60, help="Integer value")
        t.index('AcDate', [['Brand'], ['AcDate'], ['ACValue'], ['ACSubValue']], area="Sta_Index_1", primary=True)
        t.index('ACValue', [['Brand'], ['ACValue'], ['ACSubValue'], ['AcDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('ActivityCounter')
