from gearbox.migrations import Migration

class AddVASCDR(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('VASCDR', area='Sta_Data_256',
                       label='Mobile Cdr',
                       dump_name='vascdr')
        t.column('CMT', 'character', mandatory=True, format='x(3)', initial='',
                 column_label='CMT',
                 help='Callmodule Type: MSO, MST, CFO, TMM, OMM, SSP')
        t.column('CLI', 'character', format='x(18)', initial='',
                 label='GSMANR',
                 column_label='GSMANR',
                 help='Calling Party Number')
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
        t.column('CurrUnit', 'logical', format='Full/Sub', initial='?',
                 column_label='CurrUnit',
                 help='Currency FULL (1) or SUB (1/100)')
        t.column('GsmBnr', 'character', format='x(18)', initial='',
                 label='GSMBNR',
                 column_label='GSMBNR',
                 help='Called Party Number (with country code or 0: Sweden)')
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
        t.column('ChrInfo', 'character', format='x', initial='',
                 label='CHRINFO',
                 column_label='CHRINFO',
                 help='Channel Rate Info')
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
        t.column('Intr', 'character', format='x(6)', initial='',
                 label='INTR',
                 column_label='INTR',
                 help='Interruption Time')
        t.column('TRFS', 'character', format='x(6)', initial='',
                 label='TFRS',
                 column_label='TFRS',
                 help='Time From Register Seizure To Start Of Charging')
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
        t.column('Rated', 'logical', initial='no',
                 label='rated',
                 column_label='rated',
                 help='Is this mobile cdr rated ?')
        t.column('CustNum', 'integer', format='zzzzzz9', initial='0',
                 label='CustAsub',
                 column_label='CustAsub',
                 help='Customer No who owns the A-Sub No.')
        t.column('InvCust', 'integer', format='zzzzzz9', initial='0',
                 label='CustInv',
                 column_label='CustInv',
                 help='Customer who is being invoiced')
        t.column('BillLevel', 'character', format='x(10)', initial='',
                 label='Level',
                 column_label='Level',
                 help='Billing level (where the subscription is assigned)')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='No',
                 column_label='No',
                 help='Consecutive No. for Customer\'s Invoicing Target')
        t.column('TimeStart', 'integer', format='zzzzz9', initial='0',
                 label='TimeSt',
                 column_label='TimeSt',
                 help='Time when call  started (in terms of seconds from midnight)')
        t.column('TimeEnd', 'integer', format='zzzzz9', initial='0',
                 column_label='TimeEnd',
                 help='Time when call ended (in terms of seconds from midnight)')
        t.column('BillDur', 'integer', format='zzzzz9', initial='0',
                 column_label='BillDur',
                 help='Billable Duration of call, seconds')
        t.column('DateSt', 'date', format='99.99.99',
                 label='CallDate',
                 column_label='CallDate',
                 help='Date When call started')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Consecutive country/service number of call\'s  destination')
        t.column('BDest', 'character', format='x(12)', initial='',
                 label='Destin',
                 column_label='Destin',
                 help='Call\'s destination, recognised from B-sub. number')
        t.column('GrossAmt', 'decimal', decimals=2, format='zzz,zz9.99', initial='0',
                 label='TotalPrice',
                 column_label='TotalPrice',
                 help='Total \'gross\' Price of Call')
        t.column('TotDisc', 'decimal', decimals=2, format='zzz,zz9.99', initial='0',
                 column_label='TotDisc',
                 help='Total discount, subtracted from Total Price')
        t.column('Amount', 'decimal', decimals=2, format='zzz,zz9.99', initial='0',
                 label='NetPrice',
                 column_label='NetPrice',
                 help='Net (billed) Price of Call')
        t.column('DiscType', 'integer', format='9', initial='0',
                 label='Discount Type',
                 column_label='DiscType',
                 help='Discount type')
        t.column('RateType', 'logical', format='Gen/Net', initial='no',
                 label='Rate',
                 column_label='Rate',
                 help='Type of Rate used: (G)eneral rate / (N)et rate')
        t.column('DiscGroup', 'character', initial='',
                 label='DgCode',
                 column_label='DgCode',
                 help='Code of Discount Group')
        t.column('SecTB', 'integer', extent=6, format='zz,zz9', initial='0',
                 label='SecTb',
                 column_label='SecTb',
                 help='Call\'s billable secs splitted on different rated Time Bands')
        t.column('RateTB', 'decimal', extent=6, decimals=2, format='zz9.99999', initial='0',
                 label='Secrate',
                 column_label='SecRate',
                 help='Rate/sec on each individual time band')
        t.column('StartCharge', 'decimal', decimals=3, format='zz9.999', initial='0',
                 label='StFee',
                 column_label='StFee',
                 help='Start Fee')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='ProdCode',
                 column_label='ProdCode',
                 help='Product code, (call is billed with this product code)')
        t.column('ErrorCode', 'integer', format='zzz9', initial='0',
                 label='ErrC',
                 column_label='ErrC',
                 help='Rating Error Code 1 ... 9999')
        t.column('UserSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='SeqNo',
                 column_label='SeqNo',
                 help='Internal, consecutive sequence no of user')
        t.column('DiscPlan', 'character', format='x(12)', initial='',
                 label='DPCode',
                 column_label='DPlan',
                 help='Code of a Discount Plan')
        t.column('CLIType', 'character', format='x(12)', initial='',
                 column_label='CLIType',
                 help='Code of Subscription Type')
        t.column('FOC', 'logical', format='FoC', initial='no',
                 label='FoC',
                 help='Is this call Free Of Charge (Y/N) ?')
        t.column('Disc%', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Discount%',
                 column_label='Disc%',
                 help='Discount percent')
        t.column('DiscFP', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Fix%',
                 column_label='Fix%',
                 help='Amount of Destination-based Fixed Discount (%%)')
        t.column('DiscValue', 'decimal', decimals=2, format='zz,zz9.99', initial='0',
                 label='Discount',
                 column_label='Disc',
                 help='Value of discount')
        t.column('Fdisc', 'decimal', decimals=2, format='zz,zzz,zz9.99', initial='0',
                 label='FixedDiscount',
                 column_label='FixedDiscount',
                 help='Amount  of Fixed Discount')
        t.column('PNP', 'logical', initial='no',
                 column_label='PNP',
                 help='Private Numbering Plan (i.e. B-sub in \'own group\')')
        t.column('RefPrice', 'decimal', decimals=2, format='zzz,zz9.99', initial='0',
                 column_label='RefPrice',
                 help='Reference Price (gen. rate without any discounts)')
        t.column('IRID', 'integer', format='>>>>>>>9', initial='0',
                 label='InvRowId',
                 column_label='InvRowId',
                 help='Invoice row identification number')
        t.column('InvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Invoice No where this call was billed')
        t.column('TariffNum', 'integer', format='>>>>>>>>>9', initial='0',
                 label='rateid',
                 column_label='rateid',
                 help='ID of rate  record being used',
                 description='ID of nnhinta record')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription',
                 description='Id of a mobsub record. Note that mobsub can have been deleted')
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
        t.column('RoaMarginal', 'decimal', decimals=2, initial='0',
                 label='RoaMarg',
                 column_label='RoaMarg',
                 help='Margin Added into Roamrate')
        t.column('RoamRate', 'decimal', decimals=2, initial='0',
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
        t.column('Currency', 'character', format='x(3)', initial='',
                 column_label='Currency',
                 help='Charging Currency')
        t.column('Ctimeo', 'character', format='x(5)', initial='',
                 label='CTimeO',
                 column_label='CTimeO',
                 help='Call Time Offset')
        t.column('Ccharge', 'decimal', decimals=2, format='>>,>>9.999', initial='0',
                 label='CCharge',
                 column_label='CCharge',
                 help='Call Charge')
        t.column('CDPerc', 'integer', format='>>9', initial='0',
                 label='disc%',
                 column_label='Disc%',
                 help='Call Discount Percentage')
        t.column('CDAMT', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='CDAmt',
                 column_label='CDAmt',
                 help='Call Discount Amount')
        t.column('Charge', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 column_label='Charge')
        t.column('DDPerc', 'integer', format='>>9', initial='0',
                 column_label='DDPerc',
                 help='CFO Discount Percentage')
        t.column('DDAMT', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='DDAmt',
                 column_label='DDAmt',
                 help='CFO Discount Amount')
        t.column('Echarge', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='ECharge',
                 column_label='ECharge',
                 help='Event Charge')
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
        t.column('BNET', 'character', format='x(5)',
                 label='Bnet',
                 column_label='Bnet',
                 help='Mobile Operator/Service Provider')
        t.column('InvSeq', 'integer', format='>>>>>>>>>>9', initial='0',
                 help='Invoice sequence')
        t.column('VatIncl', 'logical', initial='no',
                 label='VAT Included',
                 column_label='VAT',
                 help='Is VAT included in call\'s price')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DialType')
        t.column('GsmCnr', 'character', format='x(20)', initial='')
        t.column('MSMark', 'integer', format='9', initial='0',
                 column_label='MSMark',
                 help='Mobile Station Mark')
        t.column('DataIn', 'decimal', decimals=0, format='>>>>>>>>>9', initial='0',
                 column_label='DataIn',
                 help='Data Amount Incoming')
        t.column('DataOut', 'decimal', decimals=2, format='>>>>>>>>>9', initial='0',
                 column_label='DataOut',
                 help='Data Amount Outgoing')
        t.column('TaxCode', 'character', format='x(1)', initial='',
                 label='TC',
                 column_label='TC',
                 help='TaxCode')
        t.column('Tax', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 column_label='Tax')
        t.column('tariffClass', 'character', format='x(2)', initial='',
                 label='TaC',
                 column_label='Tac',
                 help='Tariff Class')
        t.column('SOPrice', 'decimal', decimals=3, format='>>>,>>9.999', initial='0',
                 label='SOPRICE',
                 column_label='SOPRICE',
                 help='Service operator price')
        t.column('pulses', 'integer', format='>>>>>9', initial='0',
                 label='Pulses',
                 column_label='Pulses')
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
        t.column('ServiceName', 'character', format='x(20)', initial='',
                 column_label='ServiceName',
                 help='Service Name')
        t.column('ServiceAddress', 'character', format='x(15)', initial='',
                 column_label='ServiceAddress',
                 help='Service Address')
        t.column('TermSMS', 'integer', format='>>9', initial='0',
                 column_label='TermSMS',
                 help='Number of terminated SMS')
        t.column('OrigSMS', 'integer', format='>>9', initial='0',
                 column_label='OrigSMS',
                 help='Number of originated SMS')
        t.column('AType', 'integer', format='>9', initial='0',
                 column_label='AType',
                 help='A-Type')
        t.column('BType', 'integer', format='>9', initial='0',
                 column_label='BType',
                 help='B-Type')
        t.column('CType', 'integer', format='>9', initial='0',
                 column_label='CType',
                 help='C-Type')
        t.column('BPrefCode', 'character', format='x(1)', initial='',
                 column_label='BPrefCode',
                 help='B-PrefixCode')
        t.column('BPref', 'character', format='x(5)', initial='',
                 column_label='BPref',
                 help='B-Prefix')
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
        t.column('RateCCN', 'integer', format='>>9', initial='0',
                 column_label='RateCCN',
                 help='Rating CCN')
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
        t.column('RountingNumber', 'character', format='x(15)',
                 column_label='RountingNumber',
                 help='Routing Number')
        t.column('NPQStatus', 'character', format='x(2)',
                 column_label='NPQStatus',
                 help='Number Portability query status')
        t.column('VASPrice', 'decimal', decimals=5, format='>>>>>>>9.99', initial='0',
                 label='VAS Operator Price',
                 column_label='VAS Price',
                 help='VAS Operator price')
        t.index('CLI', ['CLI', 'DateSt', 'TimeStart'], area='Sta_Index_2')
        t.index('CustNum', ['CustNum', 'DateSt', 'TimeStart'], area='Sta_Index_2')
        t.index('Date', ['DateSt', 'TimeStart'], area='Sta_Index_2',
                primary=True)
        t.index('ErrorCode', ['ErrorCode'], area='Sta_Index_2')
        t.index('gsmbnr', ['DateSt', 'GsmBnr'], area='Sta_Index_2')
        t.index('InvCust', ['InvCust', 'DateSt', 'TimeStart'], area='Sta_Index_2')
        t.index('invseq', ['InvCust', 'InvSeq'], area='Sta_Index_2')

    def down(self):
        self.drop_table('VASCDR')

