from gearbox.migrations import Migration

class AddFixCDR(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('FixCDR', area='Sta_Data_256',
                       label='FixCDR',
                       dump_name='fixcdr',
                       desc='Call Detail Records')
        t.column('Date', 'date', format='99-99-99',
                 label='CallDate',
                 column_label='CallDate',
                 help='Call\'s date')
        t.column('TimeStart', 'integer', format='zzzz9', initial='0',
                 label='Begin',
                 column_label='Begin',
                 help='Call started at')
        t.column('TimeEnd', 'integer', format='zzzz9', initial='0',
                 label='End',
                 column_label='End',
                 help='Call ended')
        t.column('Duration', 'integer', format='zzzz9', initial='0',
                 label='Durat',
                 column_label='Durat',
                 help='Calls duration (sec)')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-number',
                 column_label='A-number',
                 help='A-sub. number')
        t.column('BSub', 'character', format='x(25)', initial='',
                 label='B-sub ',
                 column_label='B-sub ',
                 help='B-number')
        t.column('CurrUnit', 'logical', format='Full/Sub', initial='?',
                 column_label='CurrUnit',
                 help='Currency FULL (1) or SUB (1/100)')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Caller',
                 column_label='Caller',
                 help='Customer number of the A-subscriber')
        t.column('InvCust', 'integer', format='>>>>>>>>9', initial='0',
                 label='Payer',
                 column_label='Payer',
                 help='No. of customer who is being billed')
        t.column('RepCust', 'integer', format='>>>>>>>>9', initial='0',
                 label='Report Customer',
                 column_label='Report Customer',
                 help='Customer who receives call report(s)')
        t.column('RateCust', 'integer', format='>>>>>>>>9', initial='0',
                 label='RatingCustomer',
                 column_label='RatingCustomer',
                 help='Customer No. whose price plan is used')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code')
        t.column('SpecialDuration', 'integer', extent=5, format='zzzzz9', initial='0',
                 label='L.Eta',
                 column_label='L.Eta',
                 help='Recycled field')
        t.column('TBDuration', 'integer', extent=6, format='zzzzz9', initial='0',
                 label='Duration',
                 column_label='Duration',
                 help='Call\'s duration (sec) within each timeband')
        t.column('SpecialPrice', 'decimal', extent=5, decimals=3, format='z9.999', initial='0',
                 label='Price p/s',
                 column_label='Price p/s',
                 help='Recycled field')
        t.column('TBPrice', 'decimal', extent=6, decimals=5, format='zz9.99999', initial='0',
                 label='Rate /sec',
                 column_label='Rate /sec',
                 help='Call\'s price ex discounts / sec')
        t.column('GrossPrice', 'decimal', decimals=6, format='zz,zz9.99', initial='0',
                 label='GrossRate',
                 column_label='GrossRate',
                 help='Call\'s gross price')
        t.column('Disc%', 'decimal', decimals=2, format='z9.99', initial='0',
                 column_label='Disc%',
                 help='Applicable volume discount %')
        t.column('Priced', 'logical', format='K/E', initial='no',
                 label='Price',
                 column_label='Price',
                 help='Is the call rated')
        t.column('CCN', 'integer', format='zz9', initial='0',
                 label='Country',
                 column_label='Country',
                 help='Land\'s number')
        t.column('BDest', 'character', format='x(25)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-number\'s \'priced\' share ')
        t.column('TrunkOut', 'character', format='x(7)', initial='',
                 label='Trunk',
                 column_label='Trunk',
                 help='Outgoing trunk')
        t.column('DiscValue', 'decimal', decimals=6, format='z,zz9.99', initial='0',
                 label='Discount',
                 column_label='Discount',
                 help='Total amount of discount')
        t.column('StartFee', 'decimal', decimals=2, format='zzz9.99', initial='0',
                 label='Startcharge',
                 column_label='Startcharge',
                 help='Start Charge')
        t.column('VatIncl', 'logical', initial='yes',
                 label='VAT Included',
                 column_label='VAT Incl',
                 help='Is VAT included in price')
        t.column('DiscType', 'integer', format='9', initial='0',
                 label='Discount',
                 column_label='Discount',
                 help='Rab. based to: 0) No 1)Land / B-sub 2) Product 3) Volum / prod.')
        t.column('Prefix', 'character', format='x(12)', initial='',
                 label='CH-filname',
                 column_label='CH-filname',
                 help='Name of CDR File where this record was read')
        t.column('TrunkIn', 'character', format='x(7)', initial='',
                 label='Trunk in',
                 column_label='Trunk in',
                 help='CGR In')
        t.column('PKDuration', 'integer', format='zzzzzzz9', initial='0',
                 label='Peak secs.',
                 column_label='PeakSecs',
                 help='Seconds under the peak-period')
        t.column('OPDuration', 'integer', format='zzzzzzz9', initial='0',
                 label='OffPk Secs',
                 column_label='OffPkSecs',
                 help='Seconds under the off-peak period')
        t.column('OperIn', 'character', initial='',
                 label='ExchIn',
                 column_label='ExchIn',
                 help='Call Received from Exchange')
        t.column('OperOut', 'character', initial='',
                 label='ExchOut',
                 column_label='ExchOut',
                 help='Call Sent to Exchange')
        t.column('NetPrice', 'decimal', decimals=6, format='zz,zz9.99', initial='0',
                 label='NetRate',
                 column_label='NetRate',
                 help='Call\'s Net rate')
        t.column('CSub', 'character', format='x(25)', initial='',
                 label='C-sub',
                 column_label='C-sub',
                 help='C-number')
        t.column('ExCode', 'character', format='x(2)', initial='',
                 label='Switch',
                 column_label='Switch',
                 help='Switch that created CDR for this call')
        t.column('Connection', 'logical', format='I/D', initial='no',
                 label='A-sub type',
                 column_label='A-sub type',
                 help='Is this a call with a-sub prefix (Indirect) or not (Direct)')
        t.column('TariffId', 'integer', format='>>>>>9', initial='0',
                 label='RateRowid',
                 column_label='RateRowid',
                 help='Consecutive row identification number')
        t.column('BSubType', 'integer', format='>9', initial='0',
                 label='B-Type',
                 column_label='B-Type',
                 help='B-subscriber type, used with FTAM server.')
        t.column('CLIType', 'integer', format='>9', initial='0',
                 label='A-Type',
                 column_label='A-Type',
                 help='A-subscriber type, used with FTAM server.')
        t.column('PeakType', 'integer', format='9', initial='0',
                 label='Peak type',
                 column_label='Peak type',
                 help='Calls peak type for calculating amounts (1=Peak,2=OffPeak)')
        t.column('PCMIn', 'integer', format='>>>9', initial='0',
                 label='PCB In',
                 column_label='PCB In')
        t.column('TSIn', 'integer', format='>9', initial='0',
                 label='TS In',
                 column_label='TS In',
                 help='TS in')
        t.column('PCMOut', 'integer', format='>>>9', initial='0',
                 label='PCB Out',
                 column_label='PCB Out',
                 help='PCB out')
        t.column('TSOut', 'integer', format='>9', initial='0',
                 label='TS Out',
                 column_label='TS Out',
                 help='TS out')
        t.column('OperSent', 'logical',
                 help='Is this call sent to invoicing operator ?')
        t.column('CSubType', 'integer', format='>9',
                 label='C-Type',
                 column_label='C-Type',
                 help='C-subscriber type')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='Invoice sequence')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='Billing Target',
                 column_label='Bill.Targ',
                 help='Customer\'s billing target')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DialType',
                 help='Dialling type')
        t.index('BSub', ['BSub', ('Date', 'DESCENDING'), ('TimeStart', 'DESCENDING')], area='Sta_Index_2')
        t.index('CLI', ['CLI', 'Date', 'TimeEnd'], area='Sta_Index_2')
        t.index('Date', ['Date', 'TimeStart'], area='Sta_Index_2',
                primary=True)
        t.index('InvCust', ['InvCust', 'Date', 'TimeStart'], area='Sta_Index_2')
        t.index('invseq', ['InvSeq'], area='Sta_Index_2')
        t.index('RepCust', ['RepCust', 'CustNum', 'Date', 'TimeStart'], area='Sta_Index_2')

    def down(self):
        self.drop_table('FixCDR')

