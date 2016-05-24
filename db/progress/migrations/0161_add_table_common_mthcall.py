from gearbox.migrations import Migration

class AddTableMthCall(Migration):

    database = "common"

    def up(self):
        t = self.table('MthCall', area="Sta_Data_256", label="Customer's Monthly Calls", dump_name="mthcall", desc="Customer's monthly calls")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer number, 1 - 999999")
        t.column('Month', 'integer', format="999999", initial="0", help="Year and month (YYYYMM)", max_width=4, label="Month", column_label="Month", position=3, order=20, description="Year and month")
        t.column('Called', 'decimal', format="zz,zz9.99", decimals=2, initial="0", max_width=17, label="Called", column_label="Called", position=4, order=130, help="Called so far")
        t.column('Limit', 'integer', format=">,>>>,>>9", initial="0", help="Customer's monthly limit", max_width=4, label="CustLimit", column_label="CustLimit", position=5, order=140, description="Customer's monthly limit")
        t.column('CloseDate', 'date', format="99-99-99", help="Closing date", max_width=4, label="Closing Date", column_label="Cl.date", position=6, order=150, description="Closing date")
        t.column('Printed', 'logical', format="Yes/No", initial="no", max_width=1, label="Printed", column_label="Pr.", position=7, order=170, help="Is this record printed")
        t.column('CloseType', 'integer', format="9", initial="0", max_width=4, label="Closing Value", column_label="V", position=8, order=180, help="Closing value, 1 = Limit, 2 = Invoice, 3 = Both")
        t.column('Alarm', 'logical', format="Yes/No", initial="no", max_width=1, label="Alarm", column_label="Alarm", position=9, order=190, help="Is alarm given for this month ?")
        t.index('as-nro', [['CustNum'], ['Month']], area="Sta_Index_2", primary=True, unique=True)
        t.index('closed', [['CloseDate'], ['CustNum'], ['Month']], area="Sta_Index_2")
        t.index('Month', [['Month'], ['CustNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('MthCall')
