from gearbox.migrations import Migration

class AddTableMCDRDtl(Migration):

    database = "mcdrdtl"

    def up(self):
        t = self.table('MCDRDtl', area="Dtl_Data_Post", label="Mobile Cdr", dump_name="mcdrdtl")
        t.column('CallDate', 'character', format="x(8)", initial="", max_width=16, label="EXPDAT", column_label="EXPDAT", position=4, order=30, help="Date For Start Of Charge (YYYYMMDD)")
        t.column('CallTime', 'character', format="x(6)", initial="", max_width=12, label="EXPTID", column_label="EXPTID", position=5, order=40, help="Time For Start Of Charge (HHMMSS)")
        t.column('Duration', 'character', format="x(6)", initial="", max_width=12, label="DEBTID", column_label="DEBTID", position=6, order=50, help="Chargeable duration (HHMMSS)")
        t.column('Bmod', 'character', format="x", initial="", max_width=2, label="B-MOD", column_label="B-MOD", position=7, order=60, help="B-number Modification (1=yes)")
        t.column('CIN', 'character', format="x(8)", initial="", max_width=16, label="CIN", column_label="CIN", position=9, order=80, help="Call Identification Number")
        t.column('IMSI', 'character', format="x(15)", initial="", max_width=30, label="IMSI", column_label="IMSI", position=10, order=90, help="Subscriber IMSI")
        t.column('IMEI', 'character', format="x(15)", initial="", max_width=30, label="IMEI", column_label="IMEI", position=11, order=100, help="Subscriber IMEI")
        t.column('TeleSC', 'character', format="x(2)", initial="", max_width=4, label="TELESC", column_label="TELESC", position=12, order=110, help="GSM Teleservice Code")
        t.column('BearSC', 'character', format="x(2)", initial="", max_width=4, label="BEARSC", column_label="BEARSC", position=13, order=120, help="GSM Bearerservice Code")
        t.column('TrpyInd', 'character', format="x", initial="", max_width=2, label="TRPYIND", column_label="TRPYIND", position=15, order=140, help="Transparancy Indicator")
        t.column('SSI', 'character', format="xx", initial="", max_width=4, label="SSI", column_label="SSI", position=16, order=150, help="Suplementary Service")
        t.column('RFPowc', 'character', format="x", initial="", max_width=2, label="RFPOWC", column_label="RFPOWC", position=17, order=160, help="RF Powerclass")
        t.column('LocArea', 'character', format="x(4)", initial="", max_width=8, label="LOCAREA", column_label="LOCAREA", position=18, order=170, help="Cellid For First Cell Used")
        t.column('Xsub', 'character', format="x(15)", initial="", max_width=30, label="XSUB", column_label="XSUB", position=19, order=180, help="Original Called Number")
        t.column('RN', 'character', format="xx", initial="", max_width=4, label="RN", column_label="RN", position=20, order=190, help="Partial Output Record Number")
        t.column('LastPart', 'character', format="x", initial="", max_width=2, label="LASTPART", column_label="LASTPART", position=21, order=200, help="Last Partial Output")
        t.column('EndTime', 'character', format="x(6)", initial="", max_width=12, label="STOP-TID", column_label="STOP-TID", position=22, order=210, help="Time For Stop of Charging")
        t.column('ScAddr', 'character', format="x(15)", initial="", max_width=30, label="SCADDR", column_label="SCADDR", position=25, order=240, help="Service Center Address")
        t.column('MsCID', 'character', format="x(12)", initial="", max_width=24, label="MSCID", column_label="MSCID", position=26, order=250, help="MSC Identification")
        t.column('MpCall', 'character', format="x", initial="", max_width=2, label="MPCALL", column_label="MPCALL", position=27, order=260, help="Multiparty Indicator")
        t.column('SSReg', 'character', format="x", initial="", max_width=2, label="SSREQ", column_label="SSREQ", position=28, order=270, help="SSRequest")
        t.column('OrgLocNr', 'character', format="x(7)", initial="", max_width=14, label="ORGLOCNR", column_label="ORGLOCNR", position=29, order=280, help="Originating Location Area")
        t.column('RegDep', 'character', format="xx", initial="", max_width=4, label="REGDEP", column_label="REGDEP", position=30, order=290, help="Regional Dep Charging")
        t.column('DiscPart', 'character', format="x", initial="", max_width=2, label="DISCPART", column_label="DISCPART", position=31, order=300, help="Disconnecting Party")
        t.column('PpFlag', 'character', format="x", initial="", max_width=2, label="PPFLAG", column_label="PPFLAG", position=32, order=310, help="Prepaid Flagga")
        t.column('AntEvent', 'character', format="x", initial="", max_width=2, label="ANTEVENT", column_label="ANTEVENT", position=33, order=330, help="Number Of Event Modules")
        t.column('EMT', 'character', format="x(3)", initial="", max_width=6, label="EMT", column_label="EMT", position=34, order=340, help="Event Module Type")
        t.column('SSCode', 'character', format="xx", initial="", max_width=4, label="SSCODE", column_label="SSCODE", position=35, order=350, help="SSCode")
        t.column('EventDuration', 'character', format="x(6)", initial="", max_width=12, label="EVTID", column_label="EVTID", position=36, order=360, help="Time For Event")
        t.column('CellID', 'character', format="x(6)", initial="", max_width=12, label="CELLID", column_label="CELLID", position=37, order=175, help="Cellid For First Cell Used")
        t.column('SSReg2', 'character', format="x", initial="", max_width=2, label="SSREQ2", column_label="SSREQ2", position=38, order=355, help="SSRequest 2")
        t.column('FileSeq', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Sequence", column_label="Sequence", position=39, order=370, help="Consecutive Number of one file")
        t.column('BillLevel', 'character', format="x(10)", initial="", max_width=20, label="Level", column_label="Level", position=44, order=420, help="Billing level (where the subscription is assigned)")
        t.column('UserSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="SeqNo", column_label="SeqNo", position=47, order=610, help="Internal, consecutive sequence no of user")
        t.column('GrossAmt', 'decimal', format="zzz,zz9.99", decimals=3, initial="0", max_width=17, label="TotalPrice", column_label="TotalPrice", position=52, order=500, help="Total 'gross' Price of Call")
        t.column('Billable', 'logical', format="yes/no", initial="no", max_width=1, label="Billable", column_label="Billable", position=55, order=375, help="Shall this mobile cdr be billed (y/n)")
        t.column('RateType', 'logical', format="Gen/Net", initial="no", max_width=1, label="Rate", column_label="Rate", position=57, order=540, help="Type of Rate used: (G)eneral rate / (N)et rate")
        t.column('DiscGroup', 'character', format="x(8)", initial="", max_width=16, label="DgCode", column_label="DgCode", position=58, order=550, help="Code of Discount Group")
        t.column('RateTB', 'decimal', format="zz9.99999", decimals=5, initial="0", max_width=120, label="Secrate", column_label="SecRate", extent=6, position=60, order=570, help="Rate/sec on each individual time band")
        t.column('DiscPlan', 'character', format="x(12)", initial="", max_width=24, label="DPCode", column_label="DPlan", position=64, order=620, help="Code of a Discount Plan")
        t.column('TimeEnd', 'integer', format="zzzzz9", initial="0", max_width=4, label="TimeEnd", column_label="TimeEnd", position=66, order=445, help="Time when call ended (in terms of seconds from midnight)")
        t.column('FOC', 'logical', format="FoC", initial="no", max_width=1, label="FoC", position=67, order=640, help="Is this call Free Of Charge (Y/N) ?")
        t.column('IRID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="InvRowId", column_label="InvRowId", position=74, order=700, help="Invoice row identification number")
        t.column('InvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=75, order=710, help="Invoice No where this call was billed")
        t.column('Roaming', 'integer', format="9", initial="0", max_width=4, label="Roaming", column_label="Roaming", position=78, order=740, help="Type of Roaming Call")
        t.column('RoamCC', 'character', format="x(8)", initial="", max_width=16, label="RoamCC", column_label="RoamCC", position=79, order=750, help="Roaming Country Code, Country Where R-Call was orig or terminat")
        t.column('RoamOp', 'character', format="x(8)", initial="", max_width=16, label="RoamOp", column_label="RoamOp", position=80, order=760, help="Code of Foreign Operator Whose Network Was Used")
        t.column('RoamP', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="RoamP", column_label="RoamP", position=81, order=770, help="Percent Being Added To The Original Fee")
        t.column('RoaMarginal', 'decimal', format="->>,>>9.99", decimals=3, initial="0", max_width=17, label="RoaMarg", column_label="RoaMarg", position=82, order=780, help="Margin Added into Roamrate")
        t.column('RoamRate', 'decimal', format="->>,>>9.99", decimals=3, initial="0", max_width=17, label="RoamRate", column_label="RoamRate", position=83, order=790, help="Original Rate of Roaming Call, Set by Foreign Operator")
        t.column('RoamRateMethod', 'integer', format="9", initial="0", max_width=4, label="RoamRM", column_label="RoamRM", position=84, order=800, help="Rating Method")
        t.column('RepCodes', 'character', format="x(30)", initial="", max_width=60, label="ReportCode", column_label="ReportCode", position=85, order=810, help="Rapportdefinition")
        t.column('ChItem', 'character', format="x(1)", initial="", max_width=2, label="CItem", column_label="CItem", position=86, order=830, help="Chargable Item")
        t.column('CapTon', 'character', format="x(1)", initial="", max_width=2, label="CaPtNo", column_label="CaPtNo", position=87, order=840, help="Called Party Ton")
        t.column('CaPNP', 'character', format="x(1)", initial="", max_width=2, label="CaPNP", column_label="CaPNP", position=88, order=850, help="Called Party NP")
        t.column('Currency', 'character', format="x(3)", initial="", max_width=6, label="Currency", column_label="Currency", position=89, order=861, help="Charging Currency")
        t.column('Ctimeo', 'character', format="x(5)", initial="", max_width=10, label="CTimeO", column_label="CTimeO", position=90, order=871, help="Call Time Offset")
        t.column('Ccharge', 'decimal', format=">>,>>9.999", decimals=3, initial="0", max_width=17, label="CCharge", column_label="CCharge", position=91, order=881, help="Call Charge")
        t.column('CDPerc', 'integer', format=">>9", initial="0", max_width=4, label="disc%", column_label="Disc%", position=92, order=891, help="Call Discount Percentage")
        t.column('CDAMT', 'decimal', format=">>>,>>9.999", decimals=3, initial="0", max_width=18, label="CDAmt", column_label="CDAmt", position=93, order=901, help="Call Discount Amount")
        t.column('DDPerc', 'integer', format=">>9", initial="0", max_width=4, label="DDPerc", column_label="DDPerc", position=95, order=920, help="CFO Discount Percentage")
        t.column('DDAMT', 'decimal', format=">>>,>>9.999", decimals=3, initial="0", max_width=18, label="DDAmt", column_label="DDAmt", position=96, order=931, help="CFO Discount Amount")
        t.column('EDPerc', 'integer', format=">>9", initial="0", max_width=4, label="EDPerc", column_label="EDPerc", position=98, order=950, help="Event Discount Percent")
        t.column('EDAMT', 'decimal', format=">>>,>>9.999", decimals=3, initial="0", max_width=18, label="EDAmt", column_label="EDAmt", position=99, order=960, help="Event Discount Amount")
        t.column('RoamCall', 'logical', format="Y/N", initial="N", max_width=1, label="RoamCall", column_label="RoamCall", position=100, order=970, help="Is this roaming call")
        t.column('Complete', 'integer', format="9", max_width=4, label="Complete", column_label="Complete", position=106, order=2050, help="0 = Connection still active 1= Connection ready")
        t.column('MSMark', 'integer', format="9", initial="0", max_width=4, label="MSMark", column_label="MSMark", position=107, order=1040, help="Mobile Station Mark")
        t.column('TaxCode', 'character', format="x(1)", initial="", max_width=2, label="TC", column_label="TC", position=110, order=1080, help="TaxCode")
        t.column('Tax', 'decimal', format=">>>,>>9.999", decimals=3, initial="0", max_width=18, label="Tax", column_label="Tax", position=111, order=1090, help="Tax")
        t.column('SOPrice', 'decimal', format=">>>,>>9.999", decimals=3, initial="0", max_width=18, label="SOPRICE", column_label="SOPRICE", position=113, order=1110, help="Service operator price")
        t.column('CallType', 'integer', format=">9", initial="0", max_width=4, label="CallType", column_label="CallType", position=115, order=1130, help="Call Type")
        t.column('CaseType', 'character', format="x(2)", initial="", max_width=4, label="CaseType", column_label="CaseType", position=116, order=1140, help="CaseType")
        t.column('DataInd', 'character', format="x(1)", initial="", max_width=2, label="DataInd", column_label="DataInd", position=117, order=1150, help="Data Indicator")
        t.column('ConnType', 'character', format="x(1)", initial="", max_width=2, label="ConnType", column_label="ConnType", position=118, order=1160, help="Connection Type")
        t.column('GPRSConn', 'logical', format="yes/no", initial="no", max_width=1, label="GPRSC", column_label="GPRSC", position=119, order=1170, help="GPRS connection ready")
        t.column('PartialInd', 'character', format="x(1)", initial="", max_width=2, label="PartialInd", column_label="PartialInd", position=120, order=1180, help="Partial type indicator")
        t.column('APNNI', 'character', format="x(63)", initial="", max_width=126, label="APNNI", column_label="APNNI", position=121, order=1190, help="Access Ppoint name NI")
        t.column('APNOI', 'character', format="x(37)", initial="", max_width=74, label="APNOI", column_label="APNOI", position=122, order=1200, help="Access Point Name OI")
        t.column('TermSMS', 'integer', format=">>9", initial="0", max_width=4, label="TermSMS", column_label="TermSMS", position=125, order=1230, help="Number of terminated SMS")
        t.column('OrigSMS', 'integer', format=">>9", initial="0", max_width=4, label="OrigSMS", column_label="OrigSMS", position=126, order=1240, help="Number of originated SMS")
        t.column('CType', 'integer', format=">9", initial="0", max_width=4, label="CType", column_label="CType", position=129, order=1270, help="C-Type")
        t.column('BPrefCode', 'character', format="x(1)", initial="", max_width=2, label="BPrefCode", column_label="BPrefCode", position=130, order=1280, help="B-PrefixCode")
        t.column('AddInfo', 'character', format="x(10)", initial="", max_width=20, label="AddInfo", column_label="AddInfo", position=132, order=1300, help="Additional Info")
        t.column('GsmCnr', 'character', format="x(20)", initial="", max_width=40, position=133, order=1020)
        t.column('NetType', 'character', format="x(1)", initial="", max_width=2, label="NetType", column_label="NetType", position=134, order=1330, help="Net Type")
        t.column('RoamSeq', 'character', format="x(19)", initial="", max_width=38, label="RoamSeq", column_label="RoamSeq", position=135, order=1340, help="Roaming Sequence")
        t.column('NearCall', 'logical', format="yes/no", initial="no", max_width=1, label="NearCall", column_label="NearCall", position=136, order=1350, help="Near Call")
        t.column('SubsInfo', 'character', format="x(1)", initial="", max_width=2, label="SubsInfo", column_label="SubsInfo", position=137, order=1360, help="Subscription Information")
        t.column('ChargingID', 'character', format="x(16)", initial="", max_width=32, label="ChargingID", column_label="ChargingID", position=138, order=1370, help="Chargind ID")
        t.column('ActionCode', 'character', format="x(1)", initial="", max_width=2, label="AC", column_label="AC", position=140, order=1310, help="Action Code")
        t.column('RoamInd', 'logical', format="yes/no", initial="no", max_width=1, label="RoamInd", column_label="RoamInd", position=141, order=1320, help="Roaming Indicator")
        t.column('UTC', 'character', format="x(5)", initial="", max_width=10, label="UTC", column_label="UTC", position=142, order=1410, help="Start UTC offset")
        t.column('ServiceType', 'character', format="x(1)", initial="", max_width=2, label="ServiceType", column_label="ServiceType", position=143, order=1390, help="ServiceType")
        t.column('BServiceCode', 'character', format="x(2)", initial="", max_width=4, label="BSCode", column_label="BScode", position=144, order=1400, help="Baseic Service Code")
        t.column('NetworkOwner', 'character', format="x(5)", initial="", max_width=10, label="Owner", column_label="Owner", position=145, order=1420, help="Network Owner")
        t.column('NPQStatus', 'character', format="x(2)", max_width=4, label="NPQStatus", column_label="NPQStatus", position=147, order=1440, help="Number Portability query status")
        t.column('InServiceID', 'character', format="x(4)", initial="", max_width=8, label="InServiceID", column_label="InServiceID", position=148, order=1470, help="In Service ID")
        t.column('URL', 'character', format="x(111)", initial="", max_width=222, label="URL", column_label="URL", position=149, order=1480, help="URL")
        t.column('Success', 'integer', format="9", initial="0", max_width=4, label="Success", column_label="Success", position=150, order=1450, help="Success")
        t.column('InServiceType', 'character', format="x(2)", initial="", max_width=4, label="InServiceType", column_label="InServiceType", position=151, order=1460, help="In service type")
        t.column('MessageClass', 'character', format="x(1)", initial="", max_width=2, label="Message Class", column_label="Message Class", position=152, order=1510, help="Message Class")
        t.column('Priority', 'character', format="x(1)", initial="", max_width=2, label="Priority", column_label="Prio", position=153, order=1520, help="Priority")
        t.column('MessageStatus', 'integer', format=">>9", initial="0", max_width=4, label="Message Status", column_label="Message Status", position=154, order=1530, help="Status of message")
        t.column('SPID', 'integer', format=">>9", initial="0", max_width=4, label="Service Provider ID", column_label="Service Provider ID", position=155, order=1540, help="Service Provider ID")
        t.column('MessageID', 'character', format="x(21)", initial="", max_width=42, label="Message ID", column_label="Message ID", position=156, order=1550, help="Message ID")
        t.column('MessageFrom', 'character', format="x(64)", initial="", max_width=128, label="Message From", column_label="Message From", position=157, order=1560, help="Message From")
        t.column('MessageTo', 'character', format="x(128)", initial="", max_width=256, label="Message To", column_label="Message To", position=158, order=1570, help="Message to")
        t.column('ForwardedTo', 'character', format="x(128)", initial="", max_width=256, label="ForwardedTo", column_label="ForwardedTo", position=159, order=1580, help="Forwarded to")
        t.column('NumberOfRecipients', 'integer', format=">>9", initial="0", max_width=4, label="NumberOfRecipients", column_label="NOR", position=160, order=1590, help="Number Of Recipients")
        t.column('DeliveryReportRequest', 'logical', format="yes/no", initial="no", max_width=1, label="Delivery Report Requests", column_label="DRR", position=161, order=1600, help="Delivery Report Requests")
        t.column('MessageOriginInd', 'character', format="x(1)", initial="", max_width=2, label="Message Origin Indicator", column_label="MOInd", position=162, order=1610, help="Message Origin Indicator")
        t.column('ExternalApplicationAddress', 'character', format="x(40)", initial="", max_width=80, label="External Application Address", column_label="ExtApplAddr", position=163, order=1620, help="External Application Address")
        t.column('MessageSize', 'decimal', format=">>>>>>>>>9", decimals=0, initial="0", max_width=15, label="MessageSize", column_label="MessageSize", position=164, order=1490, help="Message size")
        t.column('MessageCT', 'character', format="x(1)", initial="", max_width=2, label="MessageCT", column_label="MessageCT", position=165, order=1500, help="Message Content Type")
        t.column('MMSEventType', 'character', format="x(6)", initial="", max_width=12, label="MMS Event Type", column_label="MMS Type", position=166, order=1630, help="MMS Event Type")
        t.column('RepCust', 'integer', format=">>>>>>>>>", initial="0", max_width=4, label="Report Customer", column_label="Report Customer", position=167, order=1640)
        t.column('SPOOper', 'character', format="x(8)", initial="", max_width=16, label="Operator", column_label="Operator", position=168, order=860, help="SPO-operator")
        t.column('SPOSeqno', 'integer', format=">>>,>>9", initial="0", max_width=4, label="SeqNo", column_label="SeqNo", position=169, order=870, help="spo Sequence number")
        t.column('SPOlis', 'character', format="x(8)", initial="", max_width=16, label="SpoOther", column_label="SpoOther", position=171, order=890, help="Others SPO serveices")
        t.column('sposij', 'character', format="x(1)", initial="", max_width=2, label="Location", column_label="Location", position=173, order=910, help="Location (D=local, M=other)")
        t.column('orgfee', 'decimal', format=">>,>>9.99", decimals=5, initial="0", max_width=20, label="OrgFee", column_label="OrgFee", position=174, order=930, help="Originating fee  (calltype/puhlaji)")
        t.column('mstype', 'character', format="x(8)", max_width=16, label="MType", column_label="Mtype", position=175, order=940, help="Type Of Mobsub (connection type)")
        t.column('DataVol', 'integer', format=">>>>>>9", max_width=4, label="Data Vol", column_label="Data Vol", position=176, order=2020, help="Data Volume")
        t.column('DataRef', 'integer', format=">>>>>9", max_width=4, position=177, order=2030, help="Data Volume ref")
        t.column('UserMsisdn', 'character', format="x(24)", max_width=60, label="UserMsisdn", column_label="UserMsisdn", position=178, order=2150, help="Target MSISDN")
        t.column('ReadInTS', 'decimal', format="99999999.99999", decimals=5, max_width=17, label="TimeStamp", column_label="TimeStamp", position=179, order=2210, help="Read In Timestamp")
        t.column('MMSServiceName', 'character', format="x(20)", max_width=40, label="MMSServiceName", column_label="MMSServiceName", position=181, order=2240, help="MMS Service Name")
        t.column('MMSServiceTarget', 'character', format="x(41)", max_width=82, label="MMS Service Target", column_label="MMS Service Target", position=182, order=2250, help="MMS Service Target")
        t.column('MMSPlatformStatus', 'character', format="x(4)", max_width=8, label="Platform", column_label="Platform", position=183, order=2260, help="MMS Platform Status")
        t.column('RateCategory', 'character', format="x(4)", max_width=8, label="Rate Category", column_label="Rate Category", position=184, order=2270, help="Rate Category")
        t.column('SMSServType', 'character', format="x(2)", initial="", max_width=4, label="SMSServType", column_label="SMSServType", position=186, order=2280, help="SMS Service Type")
        t.column('ServiceSubject', 'character', format="x(8)", initial="", max_width=16, label="ServiceSubject", column_label="ServiceSubject", position=187, order=2290)
        t.column('DateSt', 'date', format="99.99.99", max_width=4, label="CallDate", column_label="CallDate", position=188, order=470, help="Date When call started")
        t.column('DtlSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, position=189, order=2300)
        t.column('Version', 'character', format="x(6)", initial="", max_width=12, label="Version", column_label="Version", position=190, order=2310)
        t.index('DtlSeq', [['DateSt'], ['DtlSeq']], area="Dtl_Index1", primary=True)

    def down(self):
        self.drop_table('MCDRDtl')
