from gearbox.migrations import Migration

class AddTableWInvSession(Migration):

    database = "common"

    def up(self):
        t = self.table('WInvSession', area="Sta_Data_256", label="Web Invoice Session", dump_name="winvsess", desc="Session id for Web Invoice")
        t.column('CustNum', 'integer', format="zzzzzzzz", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=3, order=20, help="Customer number")
        t.column('LastAction', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Last action time", column_label="Action", position=4, order=30, help="Time stamp for last action made")
        t.column('WsAdd', 'character', format="X(8)", initial="", max_width=16, label="Additional", position=5, order=40, help="Additional")
        t.column('WsAddInt', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Additional int", position=6, order=50, help="Additional int")
        t.column('ActionDate', 'date', format="99-99-9999", max_width=4, label="Action Date", column_label="Date", position=7, order=60, help="Date when last action occurred")
        t.column('SessionId', 'character', format="X(50)", initial="", max_width=100, label="SessionId", column_label="Id", position=8, case_sensitive=True, order=10, help="SessionId")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=9, order=70, help="Code Of Brand")
        t.index('SessionId', [['Brand'], ['SessionId']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ActionDate', [['Brand'], ['ActionDate']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('WInvSession')
