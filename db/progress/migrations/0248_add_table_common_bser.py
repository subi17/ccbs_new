from gearbox.migrations import Migration

class AddTableBSer(Migration):

    database = "common"

    def up(self):
        t = self.table('BSer', area="Sta_Data_256", dump_name="bser")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('CliFrom', 'character', format="x(12)", initial="", max_width=24, label="B-Sub From", column_label="B-Sub From", position=3, order=20, help="B-Number series lower limit")
        t.column('CliTo', 'character', format="x(12)", initial="", max_width=24, label="B-sub To", column_label="B-sub To", position=4, order=30, help="B-Number series upper limit")
        t.index('b-nr', [['CliFrom'], ['CliTo'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('b-asno', [['CustNum'], ['CliFrom']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BSer')
