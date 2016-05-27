from gearbox.migrations import Migration

class AddTableTMQueue(Migration):

    database = "counter"

    def up(self):
        t = self.table('TMQueue', area="Sta_Data_128", label="TM Queue", dump_name="tmqueue", desc="TM queue")
        t.column('EventID', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, label="Event ID", column_label="ID", position=2, order=10, help="Event id")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=3, order=20, help="Mobile subscription ID")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=4, order=30, help="Customer number")
        t.column('DateSt', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Event Date", column_label="Date", position=5, order=40, help="Event date")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=6, order=50, help="Billing item code")
        t.column('RateCCN', 'integer', mandatory=True, format=">>9", initial="0", max_width=4, label="CCN", position=7, order=60, help="Call case number")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-number", column_label="BDest", position=8, order=70, help="B-number")
        t.column('SpoCMT', 'integer', format=">>>9", initial="0", max_width=4, label="TCC", position=9, order=80, help="Technical call case number")
        t.column('BillDur', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Billing Duration", column_label="Duration", position=10, order=90, help="Duration")
        t.column('Amount', 'decimal', format="->>>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Amount", position=11, order=100, help="Amount")
        t.column('Qty', 'integer', format="-9", initial="0", max_width=4, label="Quantity", column_label="Qty", position=12, order=110, help="Quantity")
        t.column('DataIn', 'decimal', format=">>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Data In", position=13, order=120, help="Data in")
        t.column('DataOut', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Data Out", position=14, order=130, help="Data out")
        t.column('InvCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Inv.Customer", column_label="InvCust", position=15, order=140, help="Invoicing customer's number")
        t.column('AgrCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Agr.Customer", column_label="AgrCust", position=16, order=150, help="Agreement customer's number")
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLI Type", column_label="CLIType", position=17, order=160, help="CLI type")
        t.column('PayType', 'integer', format="9", initial="0", max_width=4, label="Payment Type", column_label="PayType", position=18, order=170, help="Payment type")
        t.column('PPBalance', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Prepaid Balance", column_label="PP Balance", position=19, order=180, help="Prepaid balance after this ticket")
        t.column('Source', 'character', format="x(4)", initial="", max_width=8, label="Source", column_label="Source", position=20, order=190)
        t.column('PeriodFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="FromDate", column_label="Period from", position=21, order=200, help="Billing period from")
        t.column('PeriodTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="ToDate", column_label="Period to", position=22, order=210, help="Billing period to")
        t.column('InvSeq', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=23, order=220, help="InvSeq")
        t.column('CCN', 'integer', format=">>>9", initial="0", max_width=4, label="Reporting CCN", column_label="CCN", position=24, order=230, help="Reporting CCN")
        t.column('ReportingID', 'character', format="x(20)", initial="", max_width=40, label="Reporting ID", column_label="Rep.ID", position=25, order=240, help="Reporting ID")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Service Package ID", column_label="Service", position=26, order=250, help="Service package ID")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", position=27, order=260, help="MSISDN")
        t.column('TariffNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Tariff ID", column_label="Tariff", position=28, order=270, help="Tariff ID")
        t.column('VatIncl', 'logical', format="Incl/Excl", initial="no", max_width=1, label="Tax Included", column_label="Tax", position=29, order=280, help="Is tax Included/Excluded in amounts")
        t.column('RefPrice', 'decimal', format="->>>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Reference Price Amount", column_label="Ref.Price", position=30, order=290, help="Reference price amount")
        t.column('ExtraAmount', 'decimal', format="->>>>>>>9.99", decimals=6, initial="0", max_width=21, label="Extra Amount", column_label="Extra", position=31, order=300, help="Extra amount")
        t.column('AccumTarget', 'character', format="x(20)", initial="", max_width=40, label="Accumulation Target", column_label="Accum.Target", position=32, order=310, help="Accumulation target(s)")
        t.column('EventType', 'character', format="x(8)", initial="", max_width=16, label="Event Type", column_label="EventType", position=33, order=320, help="Event type of CDR")
        t.index('DateSt', [['DateSt']], area="Sta_Index_2", primary=True)
        t.index('InvSeq', [['InvSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TMQueue')
