from gearbox.migrations import Migration

class AddTableCustBal(Migration):

    database = "common"

    def up(self):
        t = self.table('CustBal', area="Sta_Data_128", label="Customer's balances", dump_name="custbal", desc='''Customer's balances


''')
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('Deposit', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Deposit", position=3, order=20, help="Deposit balance")
        t.column('OverPaym', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Overpayment", column_label="OverPaym", position=4, order=30, help="Overpayment balance")
        t.column('AdvPaym', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Adv.Payment", column_label="Adv.Paym", position=5, order=40, help="Advance payment balance")
        t.column('Debt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Debt", position=6, order=50, help="Customer's open balance")
        t.column('Interest', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Interest Debt", column_label="Interest", position=7, order=60, help="Customer's interest debt")
        t.column('PaymMethod', 'integer', format="->>>9", initial="0", max_width=4, label="MethOfPaym", column_label="MethOfPaym", position=8, order=70, help="Customer's payment behavior (days +/- dueday)")
        t.column('PaymQty', 'integer', format=">>>>9", initial="0", help="Qty of customer's payments", max_width=4, label="Qty of Payments", column_label="PaymQty", position=9, order=80, description="Used in calculating payment behaviour")
        t.column('LatestPaym', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Latest Payment", column_label="LastPaymDay", position=10, order=90, help="Customer's latest payment date")
        t.column('LatestInv', 'integer', format="999999", initial="0", max_width=4, label="InvPeriod", column_label="InvPer", position=11, order=100, help="Period of latest invoice")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="MSISDN", position=12, order=110, help="MSISDN")
        t.column('Refund', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Refund", position=13, order=120, help="Refund balance")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=14, order=1400, help="Mobile subscription ID")
        t.column('CreditLoss', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Credit Loss", column_label="Cr.Loss", position=15, order=1410, help="Credit loss balance")
        t.index('CustNum', [['CustNum'], ['CLI']], area="Sta_Index_2", primary=True, unique=True)
        t.index('MsSeq', [['CustNum'], ['MsSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustBal')
