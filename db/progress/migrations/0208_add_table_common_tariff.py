from gearbox.migrations import Migration

class AddTariff(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Tariff', area='Sta_Data_32',
                       label='Tariffs',
                       dump_name='tariff',
                       desc='B-number tariffs, either general or customer depended')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number')
        t.column('BDest', 'character', mandatory=True, format='x(16)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-subscriber/destination ')
        t.column('TZFrom', 'character', extent=6, format='99:99', initial='00:00',
                 label='Time From',
                 column_label='Tfrom',
                 help='Time from which the rate is valid')
        t.column('TZTo', 'character', extent=6, format='99:99', initial='00:00',
                 label='Time To',
                 column_label='Tto',
                 help='Time till which the rate is valid')
        t.column('Price', 'decimal', extent=6, decimals=5, format='zz9.99999', initial='0',
                 label='Rate',
                 column_label='Rate',
                 help='Rate: 1/100 per sec (ENTER)  or 1/1 per min (HOME)')
        t.column('Discount', 'logical', extent=6, initial='no',
                 label='Discnt',
                 column_label='Disc',
                 help='Shall discounts be applied to this rate',
                 description='1= direct discount allowed\
2= volume discount allowed\
3= absolute price\
4= minimum seconds or starting fee\
6= price per minute or second')
        t.column('DiscPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Discnt%',
                 column_label='Dsc%',
                 help='Fixed Discount %')
        t.column('PriceList', 'character', initial='',
                 label='Price List',
                 column_label='Price List',
                 help='Code (identifier) for a Price List')
        t.column('MinSec', 'integer', format='zzz9', initial='0',
                 label='Min.sec',
                 column_label='Min.sec',
                 help='Minimum amt of charged seconds')
        t.column('StartCharge', 'decimal', extent=6, decimals=5, format='zzz9.999', initial='0',
                 label='St.charge',
                 column_label='St.charge',
                 help='Starting charge for each time zone')
        t.column('TariffNum', 'integer', format='>>>>>9', initial='0',
                 label='rowid',
                 column_label='rowid',
                 help='Consecutive row identification number')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='Valid From',
                 help='The date FROM which this price will be used.')
        t.column('DayType', 'integer', extent=6, format='9', initial='0',
                 column_label='DayType',
                 help='Day type: 1) WeekDay, 2) WeekEnd, 3) National holiday')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='Valid To',
                 help='The date TO (until) which this price will be used.')
        t.column('ASubType', 'integer', format='9', initial='0',
                 label='A-Type',
                 column_label='A-Type',
                 help='ASUB (Dialler) type')
        t.column('RateType', 'integer', format='99', initial='1',
                 label='Type',
                 column_label='Type',
                 help='RATE TYPE: how many seconds is one rating unit')
        t.column('TZName', 'character', extent=6, format='X(30)', initial='',
                 label='Timezone',
                 help='Description for the timezone')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call case number')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code')
        t.column('VPrice', 'decimal', extent=6, decimals=5, format='zz9.99999', initial='0',
                 label='Rate with VAT',
                 column_label='VATRate',
                 help='Rate with VAT: 1/100 per sec (ENTER)  or 1/1 per min (HOME)')
        t.column('VStartCharge', 'decimal', extent=6, decimals=3, format='zzz9.999', initial='0',
                 label='St.charge with VAT',
                 column_label='VAT St.chrg',
                 help='Starting charge with VAT for each time zone')
        t.column('TariffType', 'integer', format='>>9', initial='1',
                 column_label='TariffType',
                 help='Tariff type for browsing windows and analysis')
        t.column('DataType', 'integer', format='>>9', initial='0',
                 label='Data Type',
                 column_label='Data Type',
                 help='Data type for tariff')
        t.column('CurrUnit', 'logical', format='Full/Sub', initial='yes',
                 column_label='CurrUnit',
                 help='Currency FULL (1) or SUB (1/100)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('MpmRid', 'character', initial='',
                 label='Reporting ID',
                 column_label='MpmRid',
                 help='Reporting id')
        t.column('ServRid', 'character', initial='',
                 label='Service ID',
                 column_label='ServRid',
                 help='Service reporting id')
        t.index('CCN', ['Brand', 'CCN', 'PriceList', 'BDest', ('ValidFrom', 'DESCENDING'), ('ValidTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('CCNCust', ['Brand', 'CCN', 'CustNum', 'BDest', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', 'CCN', ('ValidFrom', 'DESCENDING'), ('ValidTo', 'DESCENDING'), 'BDest'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'CCN', ('ValidFrom', 'DESCENDING'), ('ValidTo', 'DESCENDING'), 'BDest'], area='Sta_Index_2')
        t.index('PriceList', ['Brand', 'PriceList', 'CCN', 'BDest', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('TariffNum', ['Brand', 'TariffNum'], area='Sta_Index_2',
                unique=True)
        t.index('TariffType', ['Brand', 'TariffType'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Tariff')

