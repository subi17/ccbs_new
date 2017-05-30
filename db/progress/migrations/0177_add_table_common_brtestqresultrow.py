from gearbox.migrations import Migration

class AddTableBRTestQResultRow(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestQResultRow', area="Sta_Data_2_256", label="BR Test Queue Result Row", dump_name="brtestqresultrow", desc="Billrun test queue result row")
        t.column('QResultRowID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Queue Result Row ID", column_label="Result Row ID", position=2, order=10, help="Test queue result row ID")
        t.column('BRTestQRowID', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Test Queue Row ID", column_label="Queue Row ID", position=3, order=20, help="Test queue row ID")
        t.column('ResultValue', 'decimal', format="->>>>>>>9.99", decimals=3, initial="0", max_width=18, label="Result Value", column_label="Result", position=4, order=30, help="Result value")
        t.column('TestResult', 'character', format="x(30)", initial="", max_width=60, label="Result", position=5, order=40, help="Result of the test")
        t.column('BRTestQResultID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue Result ID", column_label="Result ID", position=7, order=60, help="Test queue result ID")
        t.column('InvCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Inv.Cust", position=8, order=70, help="Invoice customer")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=9, order=80, help="Subscription ID")
        t.column('ExtInvID', 'character', format="x(12)", initial="", max_width=24, label="External Invoice ID", column_label="InvoiceID", position=10, order=90, help="External invoice ID")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Number", column_label="Invoice", position=11, order=100, help="Invoice number (internal)")
        t.index('QResultRowID', [['QResultRowID']], area="Sta_Index_4", primary=True, unique=True)
        t.index('BRTestQResultID', [['BRTestQResultID'], ['BRTestQRowID']], area="Sta_Index_4")
        t.index('BRTestQRowID', [['BRTestQRowID']], area="Sta_Index_4")

    def down(self):
        self.drop_table('BRTestQResultRow')
