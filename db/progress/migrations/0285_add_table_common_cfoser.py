from gearbox.migrations import Migration

class AddTablecfoSer(Migration):

    database = "common"

    def up(self):
        t = self.table('cfoSer', area="Sta_Data_256", dump_name="cfoSer")
        t.column('CliFrom', 'character', format="x(12)", initial="", max_width=24, label="B-Sub From", column_label="B-Sub From", position=3, order=20, help="B-Number series lower limit")
        t.column('CliTo', 'character', format="x(12)", initial="", max_width=24, label="B-sub To", column_label="B-sub To", position=4, order=30, help="B-Number series upper limit")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=6, order=50, help="Memo text")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="Valid From", position=7, order=60, help="The date FROM which this CFO series will be used.")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="Valid To", position=8, order=70, help="The date TO (until) which this CFO Series will be used.")
        t.index('b-nr', [['Brand'], ['CliFrom'], ['CliTo'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('cfoSer')
