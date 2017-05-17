from gearbox.migrations import Migration

class AddTableCustomerReport(Migration):

    database = "common"

    def up(self):
        t = self.table('CustomerReport', area="Sta_Data_128", label="CustomerReport", dump_name="customerreport", desc="Temporary customer report values")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('StreetCode', 'character', format="X(8)", initial="", max_width=16, label="Street Code", column_label="Street Code", position=3, order=20, help="Address validation street code (CodC)")
        t.column('CityCode', 'character', format="X(8)", initial="", max_width=16, label="City Code", column_label="CityCode", position=4, order=30, help="Address validation city code (CodP)")
        t.column('TownCode', 'character', format="X(8)", initial="", max_width=16, label="Town Code", column_label="TownCode", position=5, order=40, help="Address validation town code (CodM)")
        t.index('CustNum', [['CustNum']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('CustomerReport')
