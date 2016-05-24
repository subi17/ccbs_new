from gearbox.migrations import Migration

class AddTableWInvRepo(Migration):

    database = "common"

    def up(self):
        t = self.table('WInvRepo', area="Sta_Data_256", label="Call Specification from Web", dump_name="winvrepo", desc="Call specification from Web Invoice")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="Cust.nr", column_label="Cust.nr", position=2, order=10, help="Customer number")
        t.column('InvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Inv.no", column_label="Inv.no", position=3, order=20, help="Report from this invoice number")
        t.column('RepType', 'integer', format=">9", initial="0", max_width=4, label="Type", column_label="Type", position=4, order=30, help="Type of report")
        t.column('Sent', 'logical', format="Yes/No", initial="no", max_width=1, label="Sent", column_label="Sent", position=5, order=40, help="Is the report sent")
        t.column('EMail', 'character', format="x(40)", initial="", max_width=80, label="E-mail", column_label="E-mail", position=6, order=50, help="Customer's e-mail address")
        t.column('RepParam', 'character', format="x(20)", initial="", max_width=40, label="Param", column_label="Param", position=7, order=60, help="Report parameter (product,ccn,asub,EMPTY)")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="DateFrom", column_label="DateFrom", position=8, order=70, help="Report FROM this date")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="DateTo", column_label="DateTo", position=9, order=80, help="Report TO this date")
        t.column('SentStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time when this report was sent", max_width=20, label="Xferred", column_label="Xferred", position=10, order=910, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=920, help="Code Of Brand")
        t.index('CustNum', [['Brand'], ['CustNum'], ['InvNum'], ['RepType'], ['Sent'], ['FromDate'], ['ToDate']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum_s', [['CustNum'], ['InvNum'], ['RepType'], ['Sent'], ['FromDate'], ['ToDate']], area="Sta_Index_2", unique=True)
        t.index('InvNum', [['Brand'], ['InvNum'], ['CustNum'], ['RepType'], ['Sent'], ['FromDate'], ['ToDate']], area="Sta_Index_2", unique=True)
        t.index('InvNum_s', [['InvNum'], ['CustNum'], ['RepType'], ['Sent'], ['FromDate'], ['ToDate']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('WInvRepo')
