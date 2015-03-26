from gearbox.migrations import Migration

class AddThreeTables(Migration):

    dumped_on = 'propus'
    database = 'mcdrdtl'

    def up(self):
        t = self.table('DtlSeq', area='Dtl_Data_64',
                       dump_name='dtlseq')
        t.column('SeqDate', 'date', format='99.99.99',
                 label='Date',
                 column_label='Date')
        t.column('SeqStream', 'integer', format='>9', initial='0',
                 label='Stream')
        t.column('SeqVal', 'integer', format='99999999', initial='0',
                 label='Value')
        t.index('SeqSeq', ['SeqDate', 'SeqStream'], area='Dtl_Index2',
                primary=True)

        t = self.table('MCDRDtl', area='Dtl_Data_64',
                       label='Mobile Cdr',
                       dump_name='mcdrdtl')
        t.column('CallDate', 'character', initial='',
                 label='EXPDAT',
                 column_label='EXPDAT',
                 help='Date For Start Of Charge (YYYYMMDD)')
        t.column('CallTime', 'character', format='x(6)', initial='',
                 label='EXPTID',
                 column_label='EXPTID',
                 help='Time For Start Of Charge (HHMMSS)')
        t.column('Duration', 'character', format='x(6)', initial='',
                 label='DEBTID',
                 column_label='DEBTID',
                 help='Chargeable duration (HHMMSS)')
        t.column('Bmod', 'character', format='x', initial='',
                 label='B-MOD',
                 column_label='B-MOD',
                 help='B-number Modification (1=yes)')
        t.column('CIN', 'character', initial='',
                 column_label='CIN',
                 help='Call Identification Number')
        t.column('IMSI', 'character', format='x(15)', initial='',
                 column_label='IMSI',
                 help='Subscriber IMSI')
        t.column('IMEI', 'character', format='x(15)', initial='',
                 column_label='IMEI',
                 help='Subscriber IMEI')
        t.column('TeleSC', 'character', format='x(2)', initial='',
                 label='TELESC',
                 column_label='TELESC',
                 help='GSM Teleservice Code')
        t.column('BearSC', 'character', format='x(2)', initial='',
                 label='BEARSC',
                 column_label='BEARSC',
                 help='GSM Bearerservice Code')
        t.column('TrpyInd', 'character', format='x', initial='',
                 label='TRPYIND',
                 column_label='TRPYIND',
                 help='Transparancy Indicator')
        t.column('SSI', 'character', format='xx', initial='',
                 column_label='SSI',
                 help='Suplementary Service')
        t.column('RFPowc', 'character', format='x', initial='',
                 label='RFPOWC',
                 column_label='RFPOWC',
                 help='RF Powerclass')
        t.column('LocArea', 'character', format='x(4)', initial='',
                 label='LOCAREA',
                 column_label='LOCAREA',
                 help='Cellid For First Cell Used')
        t.column('CellID', 'character', format='x(6)', initial='',
                 label='CELLID',
                 column_label='CELLID',
                 help='Cellid For First Cell Used')
        t.column('Xsub', 'character', format='x(15)', initial='',
                 label='XSUB',
                 column_label='XSUB',
                 help='Original Called Number')
        t.column('RN', 'character', format='xx', initial='',
                 column_label='RN',
                 help='Partial Output Record Number')
        t.column('LastPart', 'character', format='x', initial='',
                 label='LASTPART',
                 column_label='LASTPART',
                 help='Last Partial Output')
        t.column('EndTime', 'character', format='x(6)', initial='',
                 label='STOP-TID',
                 column_label='STOP-TID',
                 help='Time For Stop of Charging')
        t.column('ScAddr', 'character', format='x(15)', initial='',
                 label='SCADDR',
                 column_label='SCADDR',
                 help='Service Center Address')
        t.column('MsCID', 'character', format='x(12)', initial='',
                 label='MSCID',
                 column_label='MSCID',
                 help='MSC Identification')
        t.column('MpCall', 'character', format='x', initial='',
                 label='MPCALL',
                 column_label='MPCALL',
                 help='Multiparty Indicator')
        t.column('SSReg', 'character', format='x', initial='',
                 label='SSREQ',
                 column_label='SSREQ',
                 help='SSRequest')
        t.column('OrgLocNr', 'character', format='x(7)', initial='',
                 label='ORGLOCNR',
                 column_label='ORGLOCNR',
                 help='Originating Location Area')
        t.column('RegDep', 'character', format='xx', initial='',
                 label='REGDEP',
                 column_label='REGDEP',
                 help='Regional Dep Charging')
        t.column('DiscPart', 'character', format='x', initial='',
                 label='DISCPART',
                 column_label='DISCPART',
                 help='Disconnecting Party')
        t.column('PpFlag', 'character', format='x', initial='',
                 label='PPFLAG',
                 column_label='PPFLAG',
                 help='Prepaid Flagga')
        t.column('AntEvent', 'character', format='x', initial='',
                 label='ANTEVENT',
                 column_label='ANTEVENT',
                 help='Number Of Event Modules')
        t.column('EMT', 'character', format='x(3)', initial='',
                 column_label='EMT',
                 help='Event Module Type')
        t.column('SSCode', 'character', format='xx', initial='',
                 label='SSCODE',
                 column_label='SSCODE',
                 help='SSCode')
        t.column('SSReg2', 'character', format='x', initial='',
                 label='SSREQ2',
                 column_label='SSREQ2',
                 help='SSRequest 2')
        t.column('EventDuration', 'character', format='x(6)', initial='',
                 label='EVTID',
                 column_label='EVTID',
                 help='Time For Event')
        t.column('FileSeq', 'integer', format='zzzzzzzz9', initial='0',
                 label='Sequence',
                 column_label='Sequence',
                 help='Consecutive Number of one file')
        t.column('Billable', 'logical', initial='no',
                 column_label='Billable',
                 help='Shall this mobile cdr be billed (y/n)')
        t.column('BillLevel', 'character', format='x(10)', initial='',
                 label='Level',
                 column_label='Level',
                 help='Billing level (where the subscription is assigned)')
        t.column('TimeEnd', 'integer', format='zzzzz9', initial='0',
                 column_label='TimeEnd',
                 help='Time when call ended (in terms of seconds from midnight)')
        t.column('DateSt', 'date', format='99.99.99',
                 label='CallDate',
                 column_label='CallDate',
                 help='Date When call started')
        t.column('GrossAmt', 'decimal', decimals=3, format='zzz,zz9.99', initial='0',
                 label='TotalPrice',
                 column_label='TotalPrice',
                 help='Total \'gross\' Price of Call')
        t.column('RateType', 'logical', format='Gen/Net', initial='no',
                 label='Rate',
                 column_label='Rate',
                 help='Type of Rate used: (G)eneral rate / (N)et rate')
        t.column('DiscGroup', 'character', initial='',
                 label='DgCode',
                 column_label='DgCode',
                 help='Code of Discount Group')
        t.column('RateTB', 'decimal', extent=6, decimals=5, format='zz9.99999', initial='0',
                 label='Secrate',
                 column_label='SecRate',
                 help='Rate/sec on each individual time band')
        t.column('UserSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='SeqNo',
                 column_label='SeqNo',
                 help='Internal, consecutive sequence no of user')
        t.column('DiscPlan', 'character', format='x(12)', initial='',
                 label='DPCode',
                 column_label='DPlan',
                 help='Code of a Discount Plan')
        t.column('FOC', 'logical', format='FoC', initial='no',
                 label='FoC',
                 help='Is this call Free Of Charge (Y/N) ?')
        t.column('IRID', 'integer', format='>>>>>>>9', initial='0',
                 label='InvRowId',
                 column_label='InvRowId',
                 help='Invoice row identification number')
        t.column('InvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Invoice No where this call was billed')
        t.column('Roaming', 'integer', format='9', initial='0',
                 column_label='Roaming',
                 help='Type of Roaming Call')
        t.column('RoamCC', 'character', initial='',
                 column_label='RoamCC',
                 help='Roaming Country Code, Country Where R-Call was orig or terminat')
        t.column('RoamOp', 'character', initial='',
                 column_label='RoamOp',
                 help='Code of Foreign Operator Whose Network Was Used')
        t.column('RoamP', 'decimal', decimals=2, format='>9.99', initial='0',
                 column_label='RoamP',
                 help='Percent Being Added To The Original Fee')
        t.column('RoaMarginal', 'decimal', decimals=3, initial='0',
                 label='RoaMarg',
                 column_label='RoaMarg',
                 help='Margin Added into Roamrate')
        t.column('RoamRate', 'decimal', decimals=3, initial='0',
                 column_label='RoamRate',
                 help='Original Rate of Roaming Call, Set by Foreign Operator')
        t.column('RoamRateMethod', 'integer', format='9', initial='0',
                 label='RoamRM',
                 column_label='RoamRM',
                 help='Rating Method')
        t.column('RepCodes', 'character', format='x(30)', initial='',
                 label='ReportCode',
                 column_label='ReportCode',
                 help='Rapportdefinition')
        t.column('ChItem', 'character', format='x(1)', initial='',
                 label='CItem',
                 column_label='CItem',
                 help='Chargable Item')
        t.column('CapTon', 'character', format='x(1)', initial='',
                 label='CaPtNo',
                 column_label='CaPtNo',
                 help='Called Party Ton')
        t.column('CaPNP', 'character', format='x(1)', initial='',
                 column_label='CaPNP',
                 help='Called Party NP')
        t.column('SPOOper', 'character', initial='',
                 label='Operator',
                 column_label='Operator',
                 help='SPO-operator')
        t.column('Currency', 'character', format='x(3)', initial='',
                 column_label='Currency',
                 help='Charging Currency')
        t.column('SPOSeqno', 'integer', format='>>>,>>9', initial='0',
                 label='SeqNo',
                 column_label='SeqNo',
                 help='spo Sequence number')
        t.column('Ctimeo', 'character', format='x(5)', initial='',
                 label='CTimeO',
                 column_label='CTimeO',
                 help='Call Time Offset')
        t.column('Ccharge', 'decimal', decimals=3, format='>>,>>9.999', initial='0',
                 label='CCharge',
                 column_label='CCharge',
                 help='Call Charge')
        t.column('SPOlis', 'character', initial='',
                 label='SpoOther',
                 column_label='SpoOther',
                 help='Others SPO serveices')
        t.column('CDPerc', 'integer', format='>>9', initial='0',
                 label='disc%',
                 column_label='Disc%',
                 help='Call Discount Percentage')
        t.column('CDAMT', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='CDAmt',
                 column_label='CDAmt',
                 help='Call Discount Amount')
        t.column('sposij', 'character', format='x(1)', initial='',
                 label='Location',
                 column_label='Location',
                 help='Location (D=lähi, M=muu)')
        t.column('DDPerc', 'integer', format='>>9', initial='0',
                 column_label='DDPerc',
                 help='CFO Discount Percentage')
        t.column('orgfee', 'decimal', decimals=5, format='>>,>>9.99', initial='0',
                 label='OrgFee',
                 column_label='OrgFee',
                 help='Originating fee  (calltype/puhlaji)')
        t.column('DDAMT', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='DDAmt',
                 column_label='DDAmt',
                 help='CFO Discount Amount')
        t.column('mstype', 'character',
                 label='MType',
                 column_label='Mtype',
                 help='Type Of Mobsub (connection type)')
        t.column('EDPerc', 'integer', format='>>9', initial='0',
                 column_label='EDPerc',
                 help='Event Discount Percent')
        t.column('EDAMT', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='EDAmt',
                 column_label='EDAmt',
                 help='Event Discount Amount')
        t.column('RoamCall', 'logical', format='Y/N', initial='N',
                 column_label='RoamCall',
                 help='Is this roaming call')
        t.column('GsmCnr', 'character', format='x(20)', initial='')
        t.column('MSMark', 'integer', format='9', initial='0',
                 column_label='MSMark',
                 help='Mobile Station Mark')
        t.column('TaxCode', 'character', format='x(1)', initial='',
                 label='TC',
                 column_label='TC',
                 help='TaxCode')
        t.column('Tax', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 column_label='Tax')
        t.column('SOPrice', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='SOPRICE',
                 column_label='SOPRICE',
                 help='Service operator price')
        t.column('CallType', 'integer', format='>9', initial='0',
                 column_label='CallType',
                 help='Call Type')
        t.column('CaseType', 'character', format='x(2)', initial='',
                 column_label='CaseType')
        t.column('DataInd', 'character', format='x(1)', initial='',
                 column_label='DataInd',
                 help='Data Indicator')
        t.column('ConnType', 'character', format='x(1)', initial='',
                 column_label='ConnType',
                 help='Connection Type')
        t.column('GPRSConn', 'logical', initial='no',
                 label='GPRSC',
                 column_label='GPRSC',
                 help='GPRS connection ready')
        t.column('PartialInd', 'character', format='x(1)', initial='',
                 column_label='PartialInd',
                 help='Partial type indicator')
        t.column('APNNI', 'character', format='x(63)', initial='',
                 column_label='APNNI',
                 help='Access Ppoint name NI')
        t.column('APNOI', 'character', format='x(37)', initial='',
                 column_label='APNOI',
                 help='Access Point Name OI')
        t.column('TermSMS', 'integer', format='>>9', initial='0',
                 column_label='TermSMS',
                 help='Number of terminated SMS')
        t.column('OrigSMS', 'integer', format='>>9', initial='0',
                 column_label='OrigSMS',
                 help='Number of originated SMS')
        t.column('CType', 'integer', format='>9', initial='0',
                 column_label='CType',
                 help='C-Type')
        t.column('BPrefCode', 'character', format='x(1)', initial='',
                 column_label='BPrefCode',
                 help='B-PrefixCode')
        t.column('AddInfo', 'character', format='x(10)', initial='',
                 column_label='AddInfo',
                 help='Additional Info')
        t.column('ActionCode', 'character', format='x(1)', initial='',
                 label='AC',
                 column_label='AC',
                 help='Action Code')
        t.column('RoamInd', 'logical', initial='no',
                 column_label='RoamInd',
                 help='Roaming Indicator')
        t.column('NetType', 'character', format='x(1)', initial='',
                 column_label='NetType',
                 help='Net Type')
        t.column('RoamSeq', 'character', format='x(19)', initial='',
                 column_label='RoamSeq',
                 help='Roaming Sequence')
        t.column('NearCall', 'logical', initial='no',
                 column_label='NearCall',
                 help='Near Call')
        t.column('SubsInfo', 'character', format='x(1)', initial='',
                 column_label='SubsInfo',
                 help='Subscription Information')
        t.column('ChargingID', 'character', format='x(16)', initial='',
                 column_label='ChargingID',
                 help='Chargind ID')
        t.column('ServiceType', 'character', format='x(1)', initial='',
                 column_label='ServiceType')
        t.column('BServiceCode', 'character', format='x(2)', initial='',
                 label='BSCode',
                 column_label='BScode',
                 help='Baseic Service Code')
        t.column('UTC', 'character', format='x(5)', initial='',
                 column_label='UTC',
                 help='Start UTC offset')
        t.column('NetworkOwner', 'character', format='x(5)', initial='',
                 label='Owner',
                 column_label='Owner',
                 help='Network Owner')
        t.column('NPQStatus', 'character', format='x(2)',
                 column_label='NPQStatus',
                 help='Number Portability query status')
        t.column('Success', 'integer', format='9', initial='0',
                 column_label='Success')
        t.column('InServiceType', 'character', format='x(2)', initial='',
                 column_label='InServiceType',
                 help='In service type')
        t.column('InServiceID', 'character', format='x(4)', initial='',
                 column_label='InServiceID',
                 help='In Service ID')
        t.column('URL', 'character', format='x(111)', initial='',
                 column_label='URL')
        t.column('MessageSize', 'decimal', decimals=0, format='>>>>>>>>>9', initial='0',
                 column_label='MessageSize',
                 help='Message size')
        t.column('MessageCT', 'character', format='x(1)', initial='',
                 column_label='MessageCT',
                 help='Message Content Type')
        t.column('MessageClass', 'character', format='x(1)', initial='',
                 label='Message Class',
                 column_label='Message Class')
        t.column('Priority', 'character', format='x(1)', initial='',
                 column_label='Prio')
        t.column('MessageStatus', 'integer', format='>>9', initial='0',
                 label='Message Status',
                 column_label='Message Status',
                 help='Status of message')
        t.column('SPID', 'integer', format='>>9', initial='0',
                 label='Service Provider ID',
                 column_label='Service Provider ID')
        t.column('MessageID', 'character', format='x(21)', initial='',
                 label='Message ID',
                 column_label='Message ID')
        t.column('MessageFrom', 'character', format='x(64)', initial='',
                 label='Message From',
                 column_label='Message From')
        t.column('MessageTo', 'character', format='x(128)', initial='',
                 label='Message To',
                 column_label='Message To',
                 help='Message to')
        t.column('ForwardedTo', 'character', format='x(128)', initial='',
                 column_label='ForwardedTo',
                 help='Forwarded to')
        t.column('NumberOfRecipients', 'integer', format='>>9', initial='0',
                 column_label='NOR',
                 help='Number Of Recipients')
        t.column('DeliveryReportRequest', 'logical', initial='no',
                 label='Delivery Report Requests',
                 column_label='DRR')
        t.column('MessageOriginInd', 'character', format='x(1)', initial='',
                 label='Message Origin Indicator',
                 column_label='MOInd')
        t.column('ExternalApplicationAddress', 'character', format='x(40)', initial='',
                 label='External Application Address',
                 column_label='ExtApplAddr')
        t.column('MMSEventType', 'character', format='x(6)', initial='',
                 label='MMS Event Type',
                 column_label='MMS Type')
        t.column('RepCust', 'integer', format='>>>>>>>>>', initial='0',
                 label='Report Customer',
                 column_label='Report Customer')
        t.column('DataVol', 'integer', format='>>>>>>9',
                 label='Data Vol',
                 column_label='Data Vol',
                 help='Data Volume')
        t.column('DataRef', 'integer', format='>>>>>9',
                 help='Data Volume ref')
        t.column('Complete', 'integer', format='9',
                 column_label='Complete',
                 help='0 = Connection still active 1= Connection ready')
        t.column('UserMsisdn', 'character', format='x(24)',
                 column_label='UserMsisdn',
                 help='Target MSISDN')
        t.column('ReadInTS', 'decimal', decimals=5, format='99999999.99999',
                 label='TimeStamp',
                 column_label='TimeStamp',
                 help='Read In Timestamp')
        t.column('MMSServiceName', 'character', format='x(20)',
                 column_label='MMSServiceName',
                 help='MMS Service Name')
        t.column('MMSServiceTarget', 'character', format='x(41)',
                 label='MMS Service Target',
                 column_label='MMS Service Target')
        t.column('MMSPlatformStatus', 'character', format='x(4)',
                 label='Platform',
                 column_label='Platform',
                 help='MMS Platform Status')
        t.column('RateCategory', 'character', format='x(4)',
                 label='Rate Category',
                 column_label='Rate Category')
        t.column('SMSServType', 'character', format='x(2)', initial='',
                 column_label='SMSServType',
                 help='SMS Service Type')
        t.column('ServiceSubject', 'character', initial='',
                 column_label='ServiceSubject')
        t.column('DtlSeq', 'integer', format='>>>>>>9', initial='0')
        t.column('Version', 'character', format='x(6)', initial='',
                 column_label='Version')
        t.index('DtlSeq', ['DateSt', 'DtlSeq'], area='Dtl_Index1',
                primary=True)

        t = self.table('McdrDtl2', area='Dtl_Data_64',
                       dump_name='mcdrdtl2')
        t.column('DateSt', 'date', format='99.99.99',
                 label='CallDate',
                 column_label='CallDate',
                 help='Date When call started')
        t.column('DtlSeq', 'integer', format='>>>>>>9', initial='0')
        t.column('Version', 'character', format='x(6)', initial='',
                 column_label='Version')
        t.column('Detail', 'character', format='x(50)', initial='',
                 column_label='Detail')
        t.index('DtlSeq', [('DateSt', 'DESCENDING'), 'DtlSeq'], area='Dtl_Index2',
                primary=True)


    def down(self):
        self.drop_table('DtlSeq')
        self.drop_table('MCDRDtl')
        self.drop_table('McdrDtl2')

