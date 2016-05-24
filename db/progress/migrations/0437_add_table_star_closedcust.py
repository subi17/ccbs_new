from gearbox.migrations import Migration

class AddTableClosedCust(Migration):

    database = "star"

    def up(self):
        t = self.table('ClosedCust', area="Sta_Data_256", label="ClosedCust", dump_name="closedcu", desc="Closed customers")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer number, 1 - 999999")
        t.column('Date', 'date', format="99-99-99", help="Closing date", max_width=4, label="ClDate", column_label="ClDate", position=3, order=20, description="Closing date")
        t.column('DateOpen', 'date', format="99-99-99", help="Opening date", max_width=4, label="OpDate", column_label="OpDate", position=4, order=150, description="Opening date")
        t.column('State', 'logical', format="Yes/No", initial="no", help="Deny opening (Y/N)", max_width=1, label="St.", column_label="St.", position=5, order=160, description="Can this customer be opened ?")
        t.column('Printed', 'logical', format="Yes/No", initial="no", max_width=1, label="Pr.", column_label="Pr.", position=6, order=170, help="Is this record printed")
        t.column('Called', 'integer', format="9", initial="0", max_width=4, label="Value", column_label="Value", position=7, order=180, help="Closing value, 1 = Limit, 2 = Invoice, 3 = Both")
        t.index('CustNum', [['CustNum']], area="Sta_Index_2", primary=True)
        t.index('Date', [['Date'], ['CustNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ClosedCust')
