from gearbox.migrations import Migration

class AddTableCallAlarm(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('CallAlarm', area="Sta_Data_64", dump_name="callalar")
        t.column('CASeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=2, order=10, help="Call Alarm Sequence")
        t.column('CustNo', 'integer', format="zzzzzzzzz", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=3, order=20, help="Customer Number")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="A-Sub", column_label="A-Sub", position=4, order=30, help="A-Subscriber number")
        t.column('DeliStat', 'integer', format="9", initial="1", max_width=4, label="DeliStat", column_label="DeliStat", position=5, order=40, help="Delivery Status")
        t.column('Delitype', 'integer', format=">9", initial="0", max_width=4, label="DeliType", column_label="DeliType", position=6, order=50, help="Delivere Type")
        t.column('DeliPara', 'character', format="x(40)", initial="", max_width=80, label="DeliverParam", column_label="DP", position=7, order=60, help="Delivere Parameter")
        t.column('DeliMsg', 'character', format="x(255)", initial="", max_width=510, label="Msg", column_label="Msg", position=8, order=70, help="Delivere message")
        t.column('ActStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time When Call Alarm was  activated", max_width=20, label="Activated", column_label="Activated", position=9, order=80, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CLSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=10, order=90, help="Call Limit Sequence")
        t.column('limit', 'integer', format=">>9", initial="0", help="limit", max_width=4, label="limit", column_label="limit", position=11, order=100, description="limit")
        t.column('CreditType', 'integer', format=">9", initial="0", max_width=4, label="CreditType", column_label="CreditType", position=12, order=110, help="Credit Type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=13, order=120, help="Code Of Brand")
        t.column('Orig', 'character', format="x(12)", initial="", max_width=24, label="OrigNo", column_label="", position=14, order=130, help="Originating number")
        t.column('ActInterval', 'character', format="x(12)", initial="", max_width=24, label="Activation Interval", column_label="Act.Interval", position=15, order=140, help="Time interval of day when CallAlarm handling is allowed")
        t.column('DeliStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time When Call Alarm was delivered", max_width=20, label="Delivered", column_label="Delivered", position=16, order=150, description="Time Stamp yyyymmdd.time (sec)")
        t.index('CustNo', [['Brand'], ['CustNo'], ['DeliStat']], area="Sta_Index_1", primary=True)
        t.index('ActStamp', [['Brand'], ['ActStamp', 'DESC']], area="Sta_Index_1")
        t.index('CLI', [['Brand'], ['CLI'], ['DeliStat']], area="Sta_Index_1")
        t.index('CLI_s', [['CLI'], ['DeliStat']], area="Sta_Index_1")
        t.index('Delistat', [['Brand'], ['DeliStat'], ['Delitype'], ['CustNo']], area="Sta_Index_1")
        t.index('TimeStamp', [['ActStamp', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('CallAlarm')
