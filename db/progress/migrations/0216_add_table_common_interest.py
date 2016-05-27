from gearbox.migrations import Migration

class AddTableInterest(Migration):

    database = "common"

    def up(self):
        t = self.table('Interest', area="Sta_Data_256", label="Overtime Interests Percents", dump_name="interest", desc='''Overtime interests for different time periods
''')
        t.column('ValidFrom', 'date', mandatory=True, format="99-99-99", initial=self.unknown, max_width=4, label="Valid", column_label="Valid", position=2, order=10, help="Date from which this interest is valid")
        t.column('IntPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="Interest %", column_label="Interest %", position=3, order=20, help="Amount of interest (%)")
        t.column('Memo', 'character', format="x(40)", initial="", max_width=80, label="Memo", column_label="Memo", position=4, order=30, help="Memo text")
        t.column('IntType', 'integer', format="9", initial="1", max_width=4, label="Interest type", column_label="IntType", position=5, order=40, help="1 = fixed interest, 2 = in addition to euribor")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('ValidFrom', [['Brand'], ['ValidFrom', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('IntPerc', [['Brand'], ['IntPerc'], ['ValidFrom']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Interest')
