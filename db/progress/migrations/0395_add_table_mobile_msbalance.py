from gearbox.migrations import Migration

class AddTableMSBalance(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSBalance', area="Sta_Data_128", label="Mobsub Balance", dump_name="msbalance", desc="Subscription balances")
        t.column('MSSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="SubsID", position=2, order=10, help="Subscription ID")
        t.column('BalType', 'character', format="x(8)", initial="", max_width=16, label="Balance Type", column_label="Balance", position=3, order=20, help="Balance type")
        t.column('Amount', 'decimal', format="->>>>>>>9.99", decimals=5, initial="0", max_width=20, label="Balance", position=4, order=30, help="Current balance")
        t.column('BalDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Balance Date", column_label="Date", position=5, order=40, help="Latest update to balance")
        t.column('CustNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=6, order=50, help="Customer number")
        t.index('MsSeq', [['MSSeq'], ['CustNum'], ['BalType']], area="Sta_Index_1", primary=True, unique=True)
        t.index('CustNum', [['CustNum'], ['MSSeq'], ['BalType']], area="Sta_Index_1")

    def down(self):
        self.drop_table('MSBalance')
