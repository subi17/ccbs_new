from gearbox.migrations import Migration

class AddTableMNPCal(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPCal', area="Sta_Data_32", dump_name="mnpcal")
        t.column('Region', 'character', format="x(12)", initial="", max_width=24, position=2, order=10)
        t.column('MessageType', 'character', format="x(8)", initial="", max_width=16, position=3, order=20)
        t.column('Periods', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=4, order=30)
        t.column('OrderChannel', 'character', format="x(16)", initial="", max_width=32, label="Order Channel", column_label="Channel", position=5, order=40, help="Order channel")
        t.column('MNPProduct', 'character', format="x(1)", initial="", max_width=2, label="Product", column_label="Product", position=6, order=50, help="Product")
        t.column('MNPTariff', 'character', format="x(8)", initial="", max_width=16, label="Tariff", column_label="Tariff", position=7, order=60, help="Tariff")
        t.index('Region', [['Region'], ['MessageType']], area="Sta_Index_2", primary=True)
        t.index('MessageType', [['MessageType'], ['Region']], area="Sta_Index_1")
        t.index('OrderChannel', [['OrderChannel'], ['Region'], ['MessageType']], area="Sta_Index_1")

    def down(self):
        self.drop_table('MNPCal')
