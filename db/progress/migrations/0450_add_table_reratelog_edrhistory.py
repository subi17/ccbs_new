from gearbox.migrations import Migration

class AddTableEDRHistory(Migration):

    database = "reratelog"

    def up(self):
        t = self.table('EDRHistory', area="Sta_Data_64", label="EDRHistory", dump_name="edrhistory")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=22, label="MSISDN", column_label="MSISDN", position=2, order=10, help="MSISDN")
        t.column('DateSt', 'date', format="99-99-99", max_width=4, label="Event Date", column_label="Date", position=3, order=20, help="Event date")
        t.column('TimeStart', 'integer', format=">>>>9", initial="0", max_width=4, label="Start Time", column_label="Time", position=4, order=30, help="Starting time")
        t.column('DtlSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Stream Sequence", column_label="Stream Seq.", position=5, order=40, help="Daily stream sequence")
        t.column('ErrorCode', 'integer', format=">>>>9", initial="0", max_width=4, label="Error Code", column_label="Error", position=6, order=50, help="Error code")
        t.column('InvCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Customer", position=7, order=60, help="Invoice customer")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=8, order=70, help="Billing item")
        t.column('BDest', 'character', format="x(16)", initial="", max_width=24, label="B-Destination", column_label="BDest", position=9, order=80, help="B-Destination")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Rating Package", column_label="Package", position=10, order=90, help="Rating package (periodical contract)")
        t.column('TariffNum', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Tariff ID", column_label="Tariff", position=11, order=100, help="Tariff ID")
        t.column('Amount', 'decimal', format=">>>>>>9.99", decimals=5, initial="0", max_width=20, label="Amount", column_label="Amt", position=12, order=110, help="Amount")
        t.column('UpdateDate', 'date', format="99-99-99", max_width=4, label="Update Date", column_label="Upd.Date", position=13, order=120, help="Update date")
        t.column('UpdateTime', 'integer', format=">>>>9", initial="0", max_width=4, label="Update Time", column_label="Upd.Time", position=14, order=130, help="Update time")
        t.column('UpdateSource', 'character', format="x(20)", initial="", max_width=40, label="Update Source", column_label="Source", position=15, order=140, help="Source of update")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=16, order=150, help="Subscription ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=17, order=160, help="Brand")
        t.column('CCN', 'integer', format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=18, order=170, help="Consecutive country/service number of call's  destination")
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLIType", column_label="CLIType", position=19, order=180, help="Code of Subscription Type")
        t.index('CLI', [['Brand'], ['CLI'], ['DateSt', 'DESC'], ['TimeStart', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('InvCust', [['InvCust'], ['DateSt', 'DESC']], area="Sta_Index_3")
        t.index('MsSeq', [['MsSeq'], ['DateSt', 'DESC'], ['TimeStart', 'DESC']], area="Sta_Index_1")
        t.index('UpdateDate', [['Brand'], ['UpdateDate', 'DESC'], ['UpdateTime', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('EDRHistory')
