from gearbox.migrations import Migration

class AddTableBDestHist(Migration):

    database = "common"

    def up(self):
        t = self.table('BDestHist', area="Sta_Data_256", dump_name="bdesthis")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-subNo", column_label="B-subNo", position=2, order=10, help="B-number")
        t.column('vFrom', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ValidFrom", column_label="ValidFrom", position=3, order=20, help="Valid from")
        t.column('vTo', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ValidTo", column_label="ValidTo", position=4, order=30, help="ValidTo")
        t.column('CustNum', 'integer', format="zzzzzzzz", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=5, order=40, help="Customer number")
        t.column('BillTarget', 'integer', format="z9", initial="0", max_width=4, label="Bill Target", column_label="BT", position=6, order=50, help="Customer's Billing Target")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=7, order=60, help="Code Of Brand")
        t.index('BDest', [['Brand'], ['BDest'], ['vFrom'], ['vTo']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['BillTarget'], ['BDest'], ['vFrom'], ['vTo']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['BillTarget'], ['BDest'], ['vFrom'], ['vTo']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BDestHist')
