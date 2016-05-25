from gearbox.migrations import Migration

class AddTableMinConsumption(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MinConsumption', area="Sta_Data_128", label="Minimum Consumption", dump_name="minconsumption", desc="Minimum consumption for a period")
        t.column('MSSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subs.ID", position=2, order=10, help="Sequence for a subscription")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=3, order=20, help="Period begin")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=4, order=30, help="Period end")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice", position=5, order=40, help="Invoice on which minimum consumption was handled")
        t.column('Amount', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Amount", column_label="Amt", position=6, order=50, help="Minimum consumption amount")
        t.index('MsSeq', [['MSSeq'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('InvNum', [['InvNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MinConsumption')
