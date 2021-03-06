from gearbox.migrations import Migration

class AddTableFixCDR(Migration):

    database = "star"

    def up(self):
        t = self.table('FixCDR', area="Sta_Data_256", label="FixCDR", dump_name="fixcdr", desc="Call Detail Records")
        t.column('Date', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="CallDate", column_label="CallDate", position=2, order=10, help="Call's date")
        t.column('TimeStart', 'integer', format="zzzz9", initial="0", max_width=4, label="Begin", column_label="Begin", position=3, order=20, help="Call started at")
        t.column('TimeEnd', 'integer', format="zzzz9", initial="0", max_width=4, label="End", column_label="End", position=4, order=30, help="Call ended")
        t.column('Duration', 'integer', format="zzzz9", initial="0", max_width=4, label="Durat", column_label="Durat", position=5, order=40, help="Calls duration (sec)")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="A-number", column_label="A-number", position=6, order=50, help="A-sub. number")
        t.column('BSub', 'character', format="x(25)", initial="", max_width=50, label="B-sub ", column_label="B-sub ", position=7, order=60, help="B-number")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Caller", column_label="Caller", position=8, order=71, help="Customer number of the A-subscriber")
        t.column('InvCust', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Payer", column_label="Payer", position=9, order=80, help="No. of customer who is being billed")
        t.column('RepCust', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Report Customer", column_label="Report Customer", position=10, order=90, help="Customer who receives call report(s)")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=11, order=100, help="Product code")
        t.column('SpecialDuration', 'integer', format="zzzzz9", initial="0", max_width=70, label="L.Eta", column_label="L.Eta", extent=5, position=12, order=109, help="Recycled field")
        t.column('SpecialPrice', 'decimal', format="z9.999", decimals=3, initial="0", max_width=70, label="Price p/s", column_label="Price p/s", extent=5, position=13, order=119, help="Recycled field")
        t.column('GrossPrice', 'decimal', format="zz,zz9.99", decimals=6, initial="0", max_width=21, label="GrossRate", column_label="GrossRate", position=14, order=130, help="Call's gross price")
        t.column('Disc%', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="Disc%", column_label="Disc%", position=15, order=140, help="Applicable volume discount %")
        t.column('Priced', 'logical', format="K/E", initial="no", max_width=1, label="Price", column_label="Price", position=16, order=150, help="Is the call rated")
        t.column('CCN', 'integer', format="zz9", initial="0", max_width=4, label="Country", column_label="Country", position=18, order=170, help="Land's number")
        t.column('RateCust', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="RatingCustomer", column_label="RatingCustomer", position=19, order=95, help="Customer No. whose price plan is used")
        t.column('BDest', 'character', format="x(25)", initial="", max_width=50, label="B-subNo", column_label="B-subNo", position=20, order=180, help="B-number's 'priced' share ")
        t.column('TrunkOut', 'character', format="x(7)", initial="", max_width=14, label="Trunk", column_label="Trunk", position=21, order=190, help="Outgoing trunk")
        t.column('TBPrice', 'decimal', format="zz9.99999", decimals=5, initial="0", max_width=120, label="Rate /sec", column_label="Rate /sec", extent=6, position=22, order=120, help="Call's price ex discounts / sec")
        t.column('TBDuration', 'integer', format="zzzzz9", initial="0", max_width=84, label="Duration", column_label="Duration", extent=6, position=23, order=110, help="Call's duration (sec) within each timeband")
        t.column('DiscValue', 'decimal', format="z,zz9.99", decimals=6, initial="0", max_width=21, label="Discount", column_label="Discount", position=24, order=210, help="Total amount of discount")
        t.column('StartFee', 'decimal', format="zzz9.99", decimals=2, initial="0", max_width=17, label="Startcharge", column_label="Startcharge", position=29, order=250, help="Start Charge")
        t.column('VatIncl', 'logical', format="Yes/No", initial="yes", max_width=1, label="VAT Included", column_label="VAT Incl", position=30, order=260, help="Is VAT included in price")
        t.column('DiscType', 'integer', format="9", initial="0", max_width=4, label="Discount", column_label="Discount", position=31, order=270, help="Rab. based to: 0) No 1)Land / B-sub 2) Product 3) Volum / prod.")
        t.column('Prefix', 'character', format="x(12)", initial="", max_width=24, label="CH-filname", column_label="CH-filname", position=32, order=280, help="Name of CDR File where this record was read")
        t.column('TrunkIn', 'character', format="x(7)", initial="", max_width=14, label="Trunk in", column_label="Trunk in", position=33, order=290, help="CGR In")
        t.column('PKDuration', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Peak secs.", column_label="PeakSecs", position=34, order=300, help="Seconds under the peak-period")
        t.column('OPDuration', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="OffPk Secs", column_label="OffPkSecs", position=35, order=310, help="Seconds under the off-peak period")
        t.column('OperIn', 'character', format="x(8)", initial="", max_width=16, label="ExchIn", column_label="ExchIn", position=38, order=340, help="Call Received from Exchange")
        t.column('OperOut', 'character', format="x(8)", initial="", max_width=16, label="ExchOut", column_label="ExchOut", position=39, order=350, help="Call Sent to Exchange")
        t.column('NetPrice', 'decimal', format="zz,zz9.99", decimals=6, initial="0", max_width=21, label="NetRate", column_label="NetRate", position=41, order=370, help="Call's Net rate")
        t.column('CSub', 'character', format="x(25)", initial="", max_width=50, label="C-sub", column_label="C-sub", position=46, order=420, help="C-number")
        t.column('ExCode', 'character', format="x(2)", initial="", max_width=4, label="Switch", column_label="Switch", position=47, order=430, help="Switch that created CDR for this call")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="Dialling Type", column_label="DialType", position=49, order=630, help="Dialling type")
        t.column('Connection', 'logical', format="I/D", initial="no", max_width=1, label="A-sub type", column_label="A-sub type", position=53, order=490, help="Is this a call with a-sub prefix (Indirect) or not (Direct)")
        t.column('TariffId', 'integer', format=">>>>>9", initial="0", max_width=4, label="RateRowid", column_label="RateRowid", position=54, order=500, help="Consecutive row identification number")
        t.column('BSubType', 'integer', format=">9", initial="0", max_width=4, label="B-Type", column_label="B-Type", position=56, order=520, help="B-subscriber type, used with FTAM server.")
        t.column('CLIType', 'integer', format=">9", initial="0", max_width=4, label="A-Type", column_label="A-Type", position=57, order=530, help="A-subscriber type, used with FTAM server.")
        t.column('PeakType', 'integer', format="9", initial="0", max_width=4, label="Peak type", column_label="Peak type", position=58, order=540, help="Calls peak type for calculating amounts (1=Peak,2=OffPeak)")
        t.column('PCMIn', 'integer', format=">>>9", initial="0", max_width=4, label="PCB In", column_label="PCB In", position=59, order=550, help="PCB In")
        t.column('TSIn', 'integer', format=">9", initial="0", max_width=4, label="TS In", column_label="TS In", position=60, order=560, help="TS in")
        t.column('PCMOut', 'integer', format=">>>9", initial="0", max_width=4, label="PCB Out", column_label="PCB Out", position=61, order=570, help="PCB out")
        t.column('TSOut', 'integer', format=">9", initial="0", max_width=4, label="TS Out", column_label="TS Out", position=62, order=580, help="TS out")
        t.column('OperSent', 'logical', format="Yes/No", initial=self.unknown, max_width=1, position=63, order=590, help="Is this call sent to invoicing operator ?")
        t.column('CSubType', 'integer', format=">9", initial=self.unknown, max_width=4, label="C-Type", column_label="C-Type", position=64, order=600, help="C-subscriber type")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=65, order=610, help="Invoice sequence")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Billing Target", column_label="Bill.Targ", position=66, order=620, help="Customer's billing target")
        t.column('CurrUnit', 'logical', format="Full/Sub", initial="?", max_width=1, label="CurrUnit", column_label="CurrUnit", position=67, order=70, help="Currency FULL (1) or SUB (1/100)")
        t.index('Date', [['Date'], ['TimeStart']], area="Sta_Index_2", primary=True)
        t.index('BSub', [['BSub'], ['Date', 'DESC'], ['TimeStart', 'DESC']], area="Sta_Index_2")
        t.index('CLI', [['CLI'], ['Date'], ['TimeEnd']], area="Sta_Index_2")
        t.index('InvCust', [['InvCust'], ['Date'], ['TimeStart']], area="Sta_Index_2")
        t.index('invseq', [['InvSeq']], area="Sta_Index_2")
        t.index('RepCust', [['RepCust'], ['CustNum'], ['Date'], ['TimeStart']], area="Sta_Index_2")

    def down(self):
        self.drop_table('FixCDR')
