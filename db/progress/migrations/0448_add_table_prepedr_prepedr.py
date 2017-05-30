from gearbox.migrations import Migration

class AddTablePrepEDR(Migration):

    database = "prepedr"

    def up(self):
        t = self.table('PrepEDR', area="Dyn_Data_64", label="PrePaid EDR", dump_name="prepedr")
        t.column('CLI', 'character', format="x(18)", initial="", max_width=36, label="MSISDN", column_label="MSISDN", position=2, order=10, help="Contains the subscriber identity")
        t.column('TimeStart', 'integer', format="zzzzz9", initial="0", max_width=4, label="TimeSt", column_label="TimeSt", position=3, order=20, help="Time when the data record was generated (seconds from midnight)")
        t.column('DateSt', 'date', format="99.99.99", initial=self.unknown, max_width=4, label="CallDate", column_label="CallDate", position=4, order=30, help="Date when the data record was generated")
        t.column('ErrorCode', 'integer', format="zzz9", initial="0", max_width=4, label="Error Code", column_label="ErrC", position=5, order=40, help="Rating Error Code 1 ... 9999")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Subscr.ID", column_label="MsSeq", position=6, order=50, help="Sequence for a Subscription")
        t.column('DtlSeq', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, position=7, order=60)
        t.column('ReadinTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Read In Timestamp", column_label="ReadInTS", position=8, order=70)
        t.column('ReadDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Read In Date", column_label="ReadDate", position=9, order=80, help="Read in date")
        t.column('ServFeeExpDateBefore', 'character', format="x(10)", initial="", max_width=20, label="ServFeeExpDateBefore", column_label="ServFeeExpDateBefore", position=10, order=90, help="Service fee exp date before")
        t.column('NewSC', 'integer', format=">>>9", initial="0", max_width=4, label="New Service Class", column_label="NewSC", position=11, order=100)
        t.column('SuccessCode', 'integer', format=">>>9", initial="0", max_width=4, label="Success Code", column_label="SuccessCode", position=12, order=110)
        t.column('CustNum', 'integer', format="zzzzzz9", initial="0", max_width=4, label="CustNum", column_label="CustNum", position=13, order=120, help=" Customer No .")
        t.column('SubscriberFee', 'decimal', format=">>>,>>9.999", decimals=5, initial="0", max_width=20, label="SubscriberFee", column_label="SubscrFee", position=14, order=130, help="Subscriber fee amount")
        t.column('BalanceAfter', 'decimal', format=">>>,>>9.999", decimals=5, initial="0", max_width=20, label="BalanceAfter", column_label="BalanceAfter", position=15, order=140, help="Account balance after deduction")
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLIType", column_label="CLIType", position=16, order=150)
        t.column('ReadTime', 'integer', format="zzzzz9", initial="0", max_width=4, label="ReadTime", column_label="ReadTime", position=17, order=160, description="Time while collecting CDR's")
        t.index('Date', [['DateSt'], ['TimeStart']], area="Dyn_Index1", primary=True)
        t.index('CLI', [['CLI'], ['DateSt'], ['TimeStart']], area="Dyn_Index2")
        t.index('ErrorCode', [['ErrorCode']], area="Dyn_Index4")
        t.index('MsSeq', [['MsSeq'], ['DateSt'], ['TimeStart']], area="Dyn_Index3")
        t.index('ReadDate', [['ReadDate', 'DESC'], ['ReadTime', 'DESC']], area="Dyn_Index1")

    def down(self):
        self.drop_table('PrepEDR')
