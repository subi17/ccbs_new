from gearbox.migrations import Migration

class AddTableRatePref(Migration):

    database = "common"

    def up(self):
        t = self.table('RatePref', area="Sta_Data_2_256", dump_name="ratepref")
        t.column('Prefix', 'character', format="x(5)", initial="", max_width=10, label="Prefix", column_label="Prefix", position=2, order=10, help="Operator prefix where price list is attached to")
        t.column('DialType', 'integer', format=">9", initial="0", max_width=4, label="Dialling Type", column_label="DialType", position=3, order=20, help="Dialling Type")
        t.column('RatePref', 'character', format="x(5)", initial="", max_width=10, label="Rating Prefix", column_label="RatePref", position=4, order=30, help="Prefix that is used for rating")
        t.column('CustRate', 'logical', format="Yes/No", initial="no", max_width=1, label="CustRate", column_label="CustRate", position=5, order=40, help="Are customer related rates allowed")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('Prefix', [['Brand'], ['Prefix'], ['DialType']], area="Sta_Index_3", primary=True, unique=True)
        t.index('DialType', [['Brand'], ['DialType'], ['Prefix']], area="Sta_Index_3")

    def down(self):
        self.drop_table('RatePref')
