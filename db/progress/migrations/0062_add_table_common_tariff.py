from gearbox.migrations import Migration

class AddTableTariff(Migration):

    database = "common"

    def up(self):
        t = self.table('Tariff', area="Sta_Data_32", label="Tariffs", dump_name="tariff", desc="B-number tariffs, either general or customer depended")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=20, help="Customer number")
        t.column('BDest', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="B-subNo", column_label="B-subNo", position=3, order=30, help="B-subscriber/destination ")
        t.column('Price', 'decimal', format="zz9.99999", decimals=5, initial="0", max_width=120, label="Rate", column_label="Rate", extent=6, position=6, order=60, help="Rate: 1/100 per sec (ENTER)  or 1/1 per min (HOME)")
        t.column('Discount', 'logical', format="Yes/No", initial="no", help="Shall discounts be applied to this rate", max_width=48, label="Discnt", column_label="Disc", extent=6, position=8, order=70, description='''1= direct discount allowed
2= volume discount allowed
3= absolute price
4= minimum seconds or starting fee
6= price per minute or second''')
        t.column('DiscPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="Discnt%", column_label="Dsc%", position=10, order=80, help="Fixed Discount %")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Price List", column_label="Price List", position=12, order=90, help="Code (identifier) for a Price List")
        t.column('MinSec', 'integer', format="zzz9", initial="0", max_width=4, label="Min.sec", column_label="Min.sec", position=13, order=100, help="Minimum amt of charged seconds")
        t.column('StartCharge', 'decimal', format="zzz9.999", decimals=5, initial="0", max_width=108, label="St.charge", column_label="St.charge", extent=6, position=14, order=110, help="Starting charge for each time zone")
        t.column('TariffNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="rowid", column_label="rowid", position=15, order=120, help="Consecutive row identification number")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="Valid From", position=16, order=130, help="The date FROM which this price will be used.")
        t.column('DayType', 'integer', format="9", initial="0", max_width=24, label="DayType", column_label="DayType", extent=6, position=18, order=150, help="Day type: 1) WeekDay, 2) WeekEnd, 3) National holiday")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=19, order=220, help="Call case number")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=20, order=230, help="Billing item code")
        t.column('TZFrom', 'character', format="99:99", initial="00:00", max_width=72, label="Time From", column_label="Tfrom", extent=6, position=21, order=40, help="Time from which the rate is valid")
        t.column('TZTo', 'character', format="99:99", initial="00:00", max_width=72, label="Time To", column_label="Tto", extent=6, position=22, order=50, help="Time till which the rate is valid")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="Valid To", position=23, order=180, help="The date TO (until) which this price will be used.")
        t.column('ASubType', 'integer', format="9", initial="0", max_width=4, label="A-Type", column_label="A-Type", position=24, order=190, help="ASUB (Dialler) type")
        t.column('RateType', 'integer', format="99", initial="1", max_width=4, label="Type", column_label="Type", position=25, order=200, help="RATE TYPE: how many seconds is one rating unit")
        t.column('TZName', 'character', format="X(30)", initial="", max_width=372, label="Timezone", extent=6, position=26, order=210, help="Description for the timezone")
        t.column('VPrice', 'decimal', format="zz9.99999", decimals=5, initial="0", max_width=120, label="Rate with VAT", column_label="VATRate", extent=6, position=27, order=240, help="Rate with VAT: 1/100 per sec (ENTER)  or 1/1 per min (HOME)")
        t.column('VStartCharge', 'decimal', format="zzz9.999", decimals=3, initial="0", max_width=108, label="St.charge with VAT", column_label="VAT St.chrg", extent=6, position=28, order=250, help="Starting charge with VAT for each time zone")
        t.column('TariffType', 'integer', format=">>9", initial="1", max_width=4, label="TariffType", column_label="TariffType", position=29, order=260, help="Tariff type for browsing windows and analysis")
        t.column('DataType', 'integer', format=">>9", initial="0", max_width=4, label="Data Type", column_label="Data Type", position=30, order=270, help="Data type for tariff")
        t.column('CurrUnit', 'logical', format="Full/Sub", initial="yes", max_width=1, label="CurrUnit", column_label="CurrUnit", position=31, order=280, help="Currency FULL (1) or SUB (1/100)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=32, order=290, help="Code Of Brand")
        t.column('MpmRid', 'character', format="x(8)", initial="", max_width=16, label="Reporting ID", column_label="MpmRid", position=33, order=300, help="Reporting id")
        t.column('ServRid', 'character', format="x(8)", initial="", max_width=16, label="Service ID", column_label="ServRid", position=34, order=310, help="Service reporting id")
        t.index('CCN', [['Brand'], ['CCN'], ['PriceList'], ['BDest'], ['ValidFrom', 'DESC'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('CCNCust', [['Brand'], ['CCN'], ['CustNum'], ['BDest'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['CCN'], ['ValidFrom', 'DESC'], ['ValidTo', 'DESC'], ['BDest']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['CCN'], ['ValidFrom', 'DESC'], ['ValidTo', 'DESC'], ['BDest']], area="Sta_Index_2")
        t.index('PriceList', [['Brand'], ['PriceList'], ['CCN'], ['BDest'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('TariffNum', [['Brand'], ['TariffNum']], area="Sta_Index_2", unique=True)
        t.index('TariffType', [['Brand'], ['TariffType']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Tariff')
