from gearbox.migrations import Migration

class AddTableSaldoCounter(Migration):

    database = "common"

    def up(self):
        t = self.table('SaldoCounter', area="Sta_Data_128", dump_name="callcoun")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=2, order=10, help="Invoice sequence")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="A-Sub", column_label="A-Sub", position=3, order=20, help="A-Subscriber number")
        t.column('Amt', 'decimal', format="->>>,>>9.99", decimals=3, initial="0", max_width=18, label="Amount", column_label="Amount", position=4, order=30, help="Unbilled Balance")
        t.column('MobLimit', 'integer', format=">>9", initial="0", help="limit", max_width=4, label="limit", column_label="limit", position=5, order=40, description="limit")
        t.column('CustLimit', 'integer', format=">>9", initial="0", help="limit", max_width=4, label="limit", column_label="limit", position=6, order=50, description="limit")
        t.column('qty', 'integer', format=">>>,>>>,>>9", initial="0", max_width=4, label="Qty", position=8, order=80, help="Quantity of PNP Calls")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MobSub Sequence", column_label="MobSub", position=9, order=8, help="Link to MobSub")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=10, order=5, help="Period what Callcounter is to be")
        t.index('MSSeq', [['MSSeq'], ['Period', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('SaldoCounter')
