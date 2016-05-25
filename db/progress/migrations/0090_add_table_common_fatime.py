from gearbox.migrations import Migration

class AddTableFATime(Migration):

    database = "common"

    def up(self):
        t = self.table('FATime', area="Sta_Data_64", label="Free Air Time", table_trigger=[{'crc': '?', 'procedure': 'rd-fatime.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-fatime.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="fatime", desc="Free air time (or free qty or free sum)")
        t.column('FATNum', 'integer', format=">>>>>>9", initial="0", max_width=4, label="FatId", position=2, order=10, help="Unique id for fat-definition")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=3, order=20, help="Period when the free air time is to be used")
        t.column('MsSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Mobsub", column_label="Msub", position=4, order=30, help="Link to mobsub-table")
        t.column('Amt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Free amount", column_label="FAT", position=5, order=40, help="Free amount (minutes, qty or sum)")
        t.column('QtyUnit', 'character', format="x(8)", initial="", max_width=16, label="Qty Unit", column_label="Unit", position=6, order=50, help="Unit of the free amount")
        t.column('Transfer', 'logical', format="yes/no", initial="no", max_width=1, label="Transferrable", column_label="Transfer", position=7, order=60, help="Transferrable to next period")
        t.column('PayerType', 'character', format="x(8)", initial="", max_width=16, label="Payer Type", column_label="PType", position=8, order=70, help="Type of the payer (f.ex. reseller)")
        t.column('FTGrp', 'character', format="x(8)", initial="", max_width=16, label="FatGroup", column_label="FtGrp", position=9, order=90, help="Fat Group (for products)")
        t.column('BillPrice', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Billing price", column_label="Price", position=10, order=100, help="Price for billing the payer")
        t.column('PriceUnit', 'integer', format="9", initial="0", max_width=4, label="Price Unit", column_label="PUnit", position=11, order=110, help="Is the price per unit or a total price")
        t.column('Used', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Used Amount", column_label="Used", position=12, order=120, help="Used FAT amount")
        t.column('InvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=13, order=130, help="Invoice number, where FAT was used")
        t.column('InvRowId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Line", column_label="InvLine", position=14, order=140, help="Invoice line identification number")
        t.column('TransQty', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Transferred", column_label="Tr.Amt", position=15, order=150, help="Transferred amount")
        t.column('TransPeriod', 'integer', format="999999", initial="0", max_width=4, label="Transf.Period", column_label="Tr.Period", position=16, order=160, help="Period which the unused qty is transferred to")
        t.column('OrigFat', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Original FAT", column_label="Orig.FAT", position=17, order=170, help="Link to the original FAT (from which transferred)")
        t.column('FFItemNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Link to BItem", column_label="BItem", position=18, order=180, help="Link to BITEM which is created to bill the payer")
        t.column('InvAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Invoiced Amount", column_label="Invoiced", position=19, order=190, help="Invoiced amount (billed from the payer)")
        t.column('PInvRowNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="P.Invoice line", column_label="P.InvLine", position=20, order=200, help="Invoice line where FAT was billed from the payer")
        t.column('Payer', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Payer", column_label="Payer", position=21, order=220, help="Payer's code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=22, order=230, help="Explanation / memory field for Free Air Time")
        t.column('CLI', 'character', format="x(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=23, order=240, help="MSISDN Subscriber No")
        t.column('FATId', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="fatimeId", position=24, order=250, help="Fatime table's primary key.")
        t.column('FATClass', 'logical', format="Calls/FixedFees", initial="yes", max_width=1, label="Class", column_label="Class", position=25, order=260, help="(C)alls / (F)ixed fees")
        t.column('Interval', 'integer', valexp="Interval > 0 and Interval < 13", format="z9", initial="1", max_width=4, label="Interval", column_label="Interval", position=26, order=270, valmsg="FATime interval must be between 1 and 12 !", help="FATime interval; number of months betw. 2 consecutive bills")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=29, order=280, help="Customer's number")
        t.column('VatIncl', 'logical', format="Included/Excluded", initial="Yes", max_width=1, label="VAT Included", column_label="VAT", position=30, order=310, help="Is VAT included or excluded in prices")
        t.column('FATPerc', 'decimal', format="->>>>>9.99", decimals=2, initial="0", help="FATime percentage", max_width=17, label="Percentage", column_label="Perc.", position=31, order=320, description='''

''')
        t.column('HostTable', 'character', format="x(16)", initial="", max_width=32, label="HostTable", position=32, order=330, help="Host table")
        t.column('KeyValue', 'character', format="x(20)", initial="", max_width=40, label="Link Key", column_label="Key", position=33, order=340, help="Link key to hosttable")
        t.column('LastPeriod', 'integer', format="999999", initial="0", max_width=4, label="Last Period", column_label="Last", position=34, order=350, help="Last period which FAtime can be applied to")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=36, order=360, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('FATType', 'integer', format="9", initial="0", help="FATime type", max_width=4, label="FATime Type", column_label="Type", position=40, order=290, description="0=calls, 1=fixed fees, 2=all")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=41, order=300, help="Code Of Brand")
        t.index('FatType', [['CustNum'], ['CLI'], ['FATType'], ['InvNum'], ['Period']], area="Sta_Index_2", primary=True)
        t.index('CLI', [['Brand'], ['CLI']], area="Sta_Index_2")
        t.index('CLI_s', [['CLI']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum']], area="Sta_Index_2")
        t.index('FATId', [['Brand'], ['FATId']], area="Sta_Index_2")
        t.index('FATNum', [['Brand'], ['FATNum']], area="Sta_Index_2", unique=True)
        t.index('ftgrd', [['Brand'], ['FTGrp']], area="Sta_Index_2")
        t.index('HostTable', [['Brand'], ['HostTable'], ['KeyValue']], area="Sta_Index_2")
        t.index('InvNum', [['InvNum'], ['SubInvNum']], area="Sta_Index_2")
        t.index('Mobsub', [['Brand'], ['MsSeq'], ['Period']], area="Sta_Index_2")
        t.index('OrigFat', [['Brand'], ['OrigFat']], area="Sta_Index_2")

    def down(self):
        self.drop_table('FATime')
