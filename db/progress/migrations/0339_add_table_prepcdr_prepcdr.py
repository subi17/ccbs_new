from gearbox.migrations import Migration

class AddPrepCDR(Migration):

    dumped_on = 'propus'
    database = 'prepcdr'

    def up(self):
        t = self.table('PrepCDR', area='Dyn_Data_64',
                       label='PrePaid CDR',
                       dump_name='prepcdr')
        t.column('CLI', 'character', format='x(18)', initial='',
                 label='GSMANR',
                 column_label='GSMANR',
                 help='Calling Party Number')
        t.column('CurrUnit', 'logical', format='Full/Sub', initial='?',
                 column_label='CurrUnit',
                 help='Currency FULL (1) or SUB (1/100)')
        t.column('GsmBnr', 'character', format='x(18)', initial='',
                 label='GSMBNR',
                 column_label='GSMBNR',
                 help='Called Party Number (with country code or 0: Sweden)')
        t.column('MpmRid', 'character', initial='',
                 label='Reporting ID',
                 column_label='MpmRid')
        t.column('ServRid', 'character', initial='',
                 label='Service ID',
                 column_label='ServRid',
                 help='Service reporting ID')
        t.column('CustNum', 'integer', format='zzzzzz9', initial='0',
                 column_label='CustNum',
                 help=' Customer No .')
        t.column('InvCust', 'integer', format='zzzzzz9', initial='0',
                 label='CustInv',
                 column_label='CustInv',
                 help='Customer who is being invoiced')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='No',
                 column_label='No',
                 help='Consecutive No. for Customer\'s Invoicing Target')
        t.column('TimeStart', 'integer', format='zzzzz9', initial='0',
                 label='TimeSt',
                 column_label='TimeSt',
                 help='Time when call  started (in terms of seconds from midnight)')
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
        t.column('TotDisc', 'decimal', decimals=5, format='zzz,zz9.99', initial='0',
                 column_label='TotDisc',
                 help='Total discount, subtracted from Total Price')
        t.column('Amount', 'decimal', decimals=5, format='zzz,zz9.99', initial='0',
                 label='NetPrice',
                 column_label='NetPrice',
                 help='Net (billed) Price of Call')
        t.column('DiscType', 'integer', format='9', initial='0',
                 label='Discount Type',
                 column_label='DiscType',
                 help='Discount type')
        t.column('StartCharge', 'decimal', decimals=5, format='zz9.999', initial='0',
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
        t.column('CLIType', 'character', format='x(12)', initial='',
                 column_label='CLIType',
                 help='Code of Subscription Type')
        t.column('Disc%', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Discount%',
                 column_label='Disc%',
                 help='Discount percent')
        t.column('DiscFP', 'decimal', decimals=5, format='z9.99', initial='0',
                 label='Fix%',
                 column_label='Fix%',
                 help='Amount of Destination-based Fixed Discount (%%)')
        t.column('DiscValue', 'decimal', decimals=5, format='zz,zz9.99', initial='0',
                 label='Discount',
                 column_label='Disc',
                 help='Value of discount')
        t.column('Fdisc', 'decimal', decimals=5, format='zz,zzz,zz9.99', initial='0',
                 label='FixedDiscount',
                 column_label='FixedDiscount',
                 help='Amount  of Fixed Discount')
        t.column('RefPrice', 'decimal', decimals=5, format='zzz,zz9.99', initial='0',
                 column_label='RefPrice',
                 help='Reference Price (gen. rate without any discounts)')
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
        t.column('SPOcmt', 'integer', format='>9', initial='0',
                 label='Scmt',
                 column_label='Scmt',
                 help='SPO Call Module type')
        t.column('Ccharge', 'decimal', decimals=5, format='>>,>>9.999', initial='0',
                 label='CCharge',
                 column_label='CCharge',
                 help='Call Charge')
        t.column('Charge', 'decimal', decimals=5, format='>>>,>>9.999', initial='0',
                 column_label='Charge')
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
        t.column('DataIn', 'decimal', decimals=0, format='>>>>>>>>>9', initial='0',
                 column_label='DataIn',
                 help='Data Amount Incoming')
        t.column('DataOut', 'decimal', decimals=2, format='>>>>>>>>>9', initial='0',
                 column_label='DataOut',
                 help='Data Amount Outgoing')
        t.column('tariffClass', 'character', format='x(2)', initial='',
                 label='TaC',
                 column_label='Tac',
                 help='Tariff Class')
        t.column('Pulses', 'integer', format='>>>>>9', initial='0',
                 column_label='Pulses')
        t.column('ServiceName', 'character', format='x(20)', initial='',
                 column_label='ServiceName',
                 help='Service Name')
        t.column('ServiceAddress', 'character', format='x(15)', initial='',
                 column_label='ServiceAddress',
                 help='Service Address')
        t.column('AType', 'integer', format='>9', initial='0',
                 column_label='AType',
                 help='A-Type')
        t.column('BType', 'integer', format='>9', initial='0',
                 column_label='BType',
                 help='B-Type')
        t.column('BPref', 'character', format='x(5)', initial='',
                 column_label='BPref',
                 help='B-Prefix')
        t.column('RateCCN', 'integer', format='>>9', initial='0',
                 column_label='RateCCN',
                 help='Rating CCN')
        t.column('RoutingNumber', 'character', format='x(15)',
                 column_label='RoutingNumber',
                 help='Routing Number')
        t.column('OrigRecordType', 'integer', format='>9',
                 column_label='OrigRecordType',
                 help='Original Record Type')
        t.column('DtlSeq', 'integer', format='>>>>>>9', initial='0')
        t.column('MPMAmt', 'decimal', decimals=5, format='zzz,zz9.999', initial='0',
                 label='MPM Amount',
                 column_label='MPM')
        t.column('IMEI', 'character', format='x(15)', initial='',
                 label='x(15)',
                 column_label='IMEI')
        t.column('IMEI2', 'character', format='x(15)', initial='',
                 column_label='IMEI2')
        t.column('MSCID', 'character', format='x(4)', initial='',
                 column_label='MSCID')
        t.column('AddBPref', 'character', initial='',
                 column_label='AddBPref')
        t.column('RoamingInd', 'integer', format='9', initial='0',
                 label='RoamingIND',
                 column_label='RoamingInd')
        t.column('ForwardingType', 'integer', format='9', initial='0',
                 column_label='ForwardingType')
        t.column('EventSubType', 'character', initial='',
                 column_label='EventSubType')
        t.column('IMSI2', 'character', format='x(15)', initial='',
                 column_label='IMSI2')
        t.column('IMSI', 'character', format='x(15)', initial='',
                 column_label='IMSI')
        t.column('Currency', 'character', format='x(4)', initial='',
                 label='CUR',
                 column_label='CUR')
        t.column('PPFlag', 'integer', format='9', initial='0',
                 label='PrePaid',
                 column_label='PrePAid',
                 help='Prepaid')
        t.column('xSub', 'character', format='x(12)', initial='',
                 label='Xsub',
                 column_label='Xsub')
        t.column('ReadinTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='ReadinTS')
        t.column('CaseType', 'character', format='x(2)', initial='',
                 column_label='CT')
        t.column('GrossAmt', 'decimal', decimals=5, format='>>,>>9.999', initial='0',
                 column_label='GrossAmt',
                 help='GrossAmoun')
        t.column('EventType', 'character', initial='')
        t.index('CLI', ['CLI', 'DateSt', 'TimeStart'], area='Dyn_Index2')
        t.index('CustNum', ['CustNum', 'DateSt', 'TimeStart'], area='Dyn_Index3')
        t.index('Date', ['DateSt', 'TimeStart'], area='Dyn_Index1',
                primary=True)
        t.index('ErrorCode', ['ErrorCode'], area='Dyn_Index4')
        t.index('gsmbnr', ['DateSt', 'GsmBnr'], area='Dyn_Index5')
        t.index('InvCust', ['InvCust', 'DateSt', 'TimeStart'], area='Dyn_Index6')
        t.index('invseq', ['InvCust', 'InvSeq'], area='Dyn_Index7')
        t.index('spocmt', ['SPOcmt'], area='Dyn_Index5')

    def down(self):
        self.drop_table('PrepCDR')

