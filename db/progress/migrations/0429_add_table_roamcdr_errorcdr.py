from gearbox.migrations import Migration

class AddTableErrorCDR(Migration):

    database = "roamcdr"

    def up(self):
        t = self.table('ErrorCDR', area="Dyn_Data_64", multitenant="yes", label="Error CDR", dump_name="ecdr")
        t.column('CLI', 'character', format="x(18)", initial="", max_width=36, label="GSMANR", column_label="GSMANR", position=3, order=20, help="Calling Party Number")
        t.column('GsmBnr', 'character', format="x(18)", initial="", max_width=36, label="GSMBNR", column_label="GSMBNR", position=8, order=71, help="Called Party Number")
        t.column('MpmRid', 'character', format="x(8)", initial="", max_width=12, label="Reporting ID", column_label="MpmRid", position=23, order=220, help="Reporting ID")
        t.column('ServRid', 'character', format="x(8)", initial="", max_width=12, label="Service ID", column_label="ServRid", position=24, order=230, help="Service reporting ID")
        t.column('CustNum', 'integer', format="zzzzzz9", initial="0", max_width=4, label="CustNum", column_label="CustNum", position=42, order=400, help=" Customer No .")
        t.column('InvCust', 'integer', format="zzzzzz9", initial="0", max_width=4, label="CustInv", column_label="CustInv", position=43, order=410, help="Customer who is being invoiced")
        t.column('BillTarget', 'integer', format="z9", initial="0", max_width=4, label="No", column_label="No", position=45, order=430, help="Consecutive No. for Customer's Invoicing Target")
        t.column('TimeStart', 'integer', format="zzzzz9", initial="0", max_width=4, label="TimeSt", column_label="TimeSt", position=46, order=440, help="Time when call  started (in terms of seconds from midnight)")
        t.column('BillDur', 'integer', format="zzzzz9", initial="0", max_width=4, label="BillDur", column_label="BillDur", position=48, order=460, help="Billable Duration of call, seconds")
        t.column('DateSt', 'date', format="99.99.99", initial=self.unknown, max_width=4, label="CallDate", column_label="CallDate", position=49, order=470, help="Date When call started")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=50, order=480, help="Consecutive country/service number of call's  destination")
        t.column('BDest', 'character', format="x(12)", initial="", max_width=24, label="Destin", column_label="Destin", position=51, order=490, help="Call's destination, recognised from B-sub. number")
        t.column('TotDisc', 'decimal', format="zzz,zz9.99", decimals=5, initial="0", max_width=17, label="TotDisc", column_label="TotDisc", position=53, order=510, help="Total discount, subtracted from Total Price")
        t.column('Amount', 'decimal', format="zzz,zz9.99", decimals=5, initial="0", max_width=17, label="NetPrice", column_label="NetPrice", position=54, order=520, help="Net (billed) Price of Call")
        t.column('DiscType', 'integer', format="9", initial="0", max_width=4, label="Discount Type", column_label="DiscType", position=56, order=530, help="Discount type")
        t.column('MPMAmt', 'decimal', format="zzz,zz9.999", decimals=5, initial="0", max_width=18, label="MPM Amount", column_label="MPM", position=57, order=2310, help="MPM Amount")
        t.column('IMEI', 'character', format="x(15)", initial="", max_width=30, label="x(15)", column_label="IMEI", position=58, order=2320)
        t.column('MSCID', 'character', format="x(4)", initial="", max_width=8, label="MSCID", column_label="MSCID", position=60, order=2340)
        t.column('StartCharge', 'decimal', format="zz9.999", decimals=5, initial="0", max_width=17, label="StFee", column_label="StFee", position=61, order=580, help="Start Fee")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="ProdCode", column_label="ProdCode", position=62, order=590, help="Product code, (call is billed with this product code)")
        t.column('ErrorCode', 'integer', format="zzz9", initial="0", max_width=4, label="ErrC", column_label="ErrC", position=63, order=600, help="Rating Error Code 1 ... 9999")
        t.column('EventSubType', 'character', format="x(8)", initial="", max_width=16, label="EventSubType", column_label="EventSubType", position=64, order=2380)
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLIType", column_label="CLIType", position=65, order=630, help="Code of Subscription Type")
        t.column('IMSI', 'character', format="x(15)", initial="", max_width=30, label="IMSI", column_label="IMSI", position=66, order=2400, help="IMSI")
        t.column('Currency', 'character', format="x(4)", initial="", max_width=8, label="CUR", column_label="CUR", position=67, order=2410)
        t.column('Disc%', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="Discount%", column_label="Disc%", position=68, order=650, help="Discount percent")
        t.column('DiscFP', 'decimal', format="z9.99", decimals=5, initial="0", max_width=17, label="Fix%", column_label="Fix%", position=69, order=660, help="Amount of Destination-based Fixed Discount (%%)")
        t.column('Fdisc', 'decimal', format="zz,zzz,zz9.99", decimals=5, initial="0", max_width=17, label="FixedDiscount", column_label="FixedDiscount", position=70, order=670, help="Amount  of Fixed Discount")
        t.column('DiscValue', 'decimal', format="zz,zz9.99", decimals=5, initial="0", max_width=17, label="Discount", column_label="Disc", position=71, order=665, help="Value of discount")
        t.column('GrossAmt', 'decimal', format=">>,>>9.999", decimals=5, initial="0", max_width=17, label="GrossAmt", column_label="GrossAmt", position=72, order=2460, help="GrossAmoun")
        t.column('RefPrice', 'decimal', format="zzz,zz9.99", decimals=5, initial="0", max_width=17, label="RefPrice", column_label="RefPrice", position=73, order=690, help="Reference Price (gen. rate without any discounts)")
        t.column('SubsType', 'character', format="x(1)", initial="", max_width=2, label="SubsType", column_label="SubsType", position=74, order=2480, help="Subscription type")
        t.column('TariffNum', 'integer', format=">>>>>>>>>9", initial="0", help="ID of rate  record being used", max_width=4, label="rateid", column_label="rateid", position=76, order=720, description="ID of nnhinta record")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", help="Sequence for a Subscription", max_width=4, label="SubSeq", column_label="SubSeq", position=77, order=730, description="Id of a mobsub record. Note that mobsub can have been deleted")
        t.column('Charge', 'decimal', format=">>>,>>9.999", decimals=5, initial="0", max_width=18, label="Charge", column_label="Charge", position=94, order=911, help="Charge")
        t.column('BNET', 'character', format="x(5)", initial=self.unknown, max_width=10, label="Bnet", column_label="Bnet", position=101, order=980, help="Mobile Operator/Service Provider")
        t.column('InvSeq', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, position=102, order=990, help="Invoice sequence")
        t.column('VatIncl', 'logical', format="Yes/No", initial="no", max_width=1, label="VAT Included", column_label="VAT", position=103, order=1000, help="Is VAT included in call's price")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="Dialling Type", column_label="DialType", position=104, order=1010, help="Dialling Type")
        t.column('CurrUnit', 'logical', format="Full/Sub", initial="?", max_width=1, label="CurrUnit", column_label="CurrUnit", position=105, order=70, help="Currency FULL (1) or SUB (1/100)")
        t.column('DataIn', 'decimal', format=">>>>>>>>>9", decimals=0, initial="0", max_width=15, label="DataIn", column_label="DataIn", position=108, order=1050, help="Data Amount Incoming")
        t.column('DataOut', 'decimal', format=">>>>>>>>>9", decimals=2, initial="0", max_width=17, label="DataOut", column_label="DataOut", position=109, order=1060, help="Data Amount Outgoing")
        t.column('tariffClass', 'character', format="x(2)", initial="", max_width=4, label="TaC", column_label="Tac", position=112, order=1100, help="Tariff Class")
        t.column('Pulses', 'integer', format=">>>>>9", initial="0", help="Pulses", max_width=4, label="Pulses", column_label="Pulses", position=114, order=1120, description='''

''')
        t.column('ServiceName', 'character', format="x(20)", initial="", max_width=40, label="ServiceName", column_label="ServiceName", position=123, order=1210, help="Service Name")
        t.column('ServiceAddress', 'character', format="x(15)", initial="", max_width=30, label="ServiceAddress", column_label="ServiceAddress", position=124, order=1220, help="Service Address")
        t.column('AType', 'integer', format=">9", initial="0", max_width=4, label="AType", column_label="AType", position=127, order=1250, help="A-Type")
        t.column('BType', 'integer', format=">9", initial="0", max_width=4, label="BType", column_label="BType", position=128, order=1260, help="B-Type")
        t.column('BPref', 'character', format="x(5)", initial="", max_width=10, label="BPref", column_label="BPref", position=131, order=1290, help="B-Prefix")
        t.column('RateCCN', 'integer', format=">>9", initial="0", max_width=4, label="RateCCN", column_label="RateCCN", position=139, order=1380, help="Rating CCN")
        t.column('RoutingNumber', 'character', format="x(15)", initial=self.unknown, max_width=30, label="RoutingNumber", column_label="RoutingNumber", position=146, order=1430, help="Routing Number")
        t.column('SPOcmt', 'integer', format=">9", initial="0", max_width=4, label="Scmt", column_label="Scmt", position=170, order=880, help="SPO Call Module type")
        t.column('OrigRecordType', 'integer', format=">9", initial=self.unknown, max_width=4, label="OrigRecordType", column_label="OrigRecordType", position=180, order=2230, help="Original Record Type")
        t.column('DtlSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, position=188, order=2300)
        t.column('Ccharge', 'decimal', format=">>,>>9.999", decimals=5, initial="0", max_width=18, label="CCharge", column_label="CCharge", position=189, order=881, help="Call Charge")
        t.column('IMEI2', 'character', format="x(15)", initial="", max_width=30, label="IMEI2", column_label="IMEI2", position=190, order=2330)
        t.column('AddBPref', 'character', format="x(8)", initial="", max_width=16, label="AddBPref", column_label="AddBPref", position=191, order=2350)
        t.column('RoamingInd', 'integer', format="9", initial="0", max_width=4, label="RoamingIND", column_label="RoamingInd", position=192, order=2360)
        t.column('ForwardingType', 'integer', format="9", initial="0", max_width=4, label="ForwardingType", column_label="ForwardingType", position=193, order=2370)
        t.column('IMSI2', 'character', format="x(15)", initial="", max_width=30, label="IMSI2", column_label="IMSI2", position=194, order=2390)
        t.column('PPFlag', 'integer', format="9", initial="0", max_width=4, label="PrePaid", column_label="PrePAid", position=195, order=2420, help="Prepaid")
        t.column('xSub', 'character', format="x(12)", initial="", max_width=24, label="Xsub", column_label="Xsub", position=196, order=2430)
        t.column('ReadinTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ReadinTS", column_label="ReadinTS", position=197, order=2440)
        t.column('CaseType', 'character', format="x(2)", initial="", max_width=4, label="CaseType", column_label="CT", position=198, order=2450, help="CaseType")
        t.column('EventType', 'character', format="x(8)", initial="", max_width=16, position=199, order=2470)
        t.column('DCType', 'character', format="x(8)", initial="", max_width=16, label="Contract Type", column_label="CType", position=200, order=2490, help="Contract type")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Periodical Contract", column_label="P.Contract", position=201, order=2500, help="Periodical contract")
        t.column('RerateID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="RerateID", column_label="RerateID", position=202, order=2510, description="Rerating run ID")
        t.column('RerateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="RerateTS", column_label="RerateTS", position=203, order=2520, description="Date and time when rerating was done")
        t.column('ReadDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Read In Date", column_label="Read", position=204, order=2530, help="Read in date")
        t.index('Date', [['DateSt'], ['TimeStart']], area="Dyn_Index1", primary=True)
        t.index('CLI', [['CLI'], ['DateSt'], ['TimeStart']], area="Dyn_Index2")
        t.index('ErrorCode', [['ErrorCode']], area="Dyn_Index1")
        t.index('gsmbnr', [['DateSt'], ['GsmBnr']], area="Dyn_Index2")
        t.index('ReadDate', [['ReadDate', 'DESC']], area="Dyn_Index1")
        t.index('spocmt', [['SPOcmt']], area="Dyn_Index1")

    def down(self):
        self.drop_table('ErrorCDR')
