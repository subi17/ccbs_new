from gearbox.migrations import Migration

class AddTableMCDRFile(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MCDRFile', area="Sta_Data_32", label="Mobile CDR start/end rec", dump_name="mcdrfile", desc='''
''')
        t.column('CMT', 'character', mandatory=True, format="x(3)", initial="", max_width=6, label="Call Module Type", column_label="Call Module Type", position=2, order=10, help="Call Module Type")
        t.column('Station', 'character', format="x(4)", initial="", max_width=8, label="Station", column_label="Station", position=3, order=20, help="Station")
        t.column('FromDate', 'character', format="x(8)", initial="", max_width=16, label="From Date", column_label="From Date", position=4, order=30, help="Calls From (Date) YYYYMMDD")
        t.column('FromTime', 'character', format="x(6)", initial="", max_width=12, label="From Time", column_label="From Time", position=5, order=40, help="Calls From (Time) HHMMSS")
        t.column('ToDate', 'character', format="x(8)", initial="", max_width=16, label="Call Date", column_label="Call Date", position=6, order=50, help="Calls till (date) YYYYMMDD")
        t.column('ToTime', 'character', format="x(6)", initial="", max_width=12, label="Call Time", column_label="Call Time", position=7, order=60, help="Calls till (time) HHMMSS")
        t.column('FileNumber', 'integer', format="999999", initial="0", max_width=4, label="FLOPNR", column_label="FLOPNR", position=8, order=70, help="Consecutive Number of a Mobile CDR File")
        t.column('MTXID', 'character', format="xx", initial="", max_width=4, label="MTXID", column_label="MTXID", position=9, order=80, help="Flex MTXID")
        t.column('Qty', 'integer', format="zzzzzz9", initial="0", max_width=4, label="Calls", column_label="Calls", position=10, order=90, help="Number of Call (Trans) Records in this file")
        t.column('RecordInQtu', 'decimal', format="zzzzzzzzzzz9", decimals=0, initial="0", max_width=15, label="ANTPOSTIN", column_label="ANTPOSTIN", position=11, order=100, help="Number Of Records IN")
        t.column('RecordQty', 'decimal', format="zzzzzzzzzzz9", decimals=0, initial="0", max_width=15, label="ANTKPOST", column_label="ANTKPOST", position=12, order=110, help="Number Of Converted Records")
        t.column('BlockQty', 'decimal', format="zzzzzzzzzzz9", decimals=0, initial="0", max_width=15, label="Blocks", column_label="Blocks", position=13, order=120, help="Number Of Blocks (In)")
        t.column('mc-seq', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Sequence", column_label="Sequence", position=14, order=130, help="Consecutive Number of one file")
        t.column('FileName', 'character', format="x(50)", initial="", max_width=100, label="FileName", column_label="FileName", position=15, order=140, help="Name of FLEX CDR file from TELIA")
        t.column('TaxRate', 'decimal', format=">9.99", decimals=2, initial="0", max_width=120, label="Tax Rate", column_label="Tax Rate", extent=10, position=16, order=150, help="Header Tax Rate")
        t.column('Amount', 'decimal', format=">>>>>>>>>9.999", decimals=3, initial="0", max_width=18, label="Charge", column_label="Charge", position=17, order=160, help="Total Charge")
        t.column('SPAmt', 'decimal', format=">>>>>>>>>9.999", decimals=3, initial="0", max_width=18, label="sphinta", column_label="sphinta", position=18, order=170, help="Total Sphinta")
        t.column('FileSeq', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, position=19, order=180, help="File Sequence")
        t.column('Soper', 'character', format="x(5)", initial="", max_width=10, label="SOper", column_label="SOper", position=20, order=190, help="Sending Operator")
        t.column('Roper', 'character', format="x(5)", initial="", max_width=10, label="ROper", column_label="ROper", position=21, order=200, help="Receiving Operator")
        t.column('TaxTreat', 'character', format="x(1)", initial="", max_width=2, label="TaxTreatment", column_label="TaxTreatment", position=22, order=210, help="Tax Treatment")
        t.column('CrDate', 'character', format="x(8)", initial="", max_width=16, label="Created", column_label="Created", position=23, order=220, help="File Creation Date")
        t.column('TransferDate', 'character', format="x(8)", initial="", max_width=16, label="Transmitted", column_label="Transmitted", position=24, order=230, help="File Transmission Date")
        t.column('CuttingTS', 'character', format="x(11)", initial="", max_width=22, label="CutTs", column_label="CutTs", position=25, order=240, help="Cut Of Timestamp")
        t.column('UTC', 'character', format="x(5)", initial="", max_width=10, label="UTC", column_label="UTC", position=26, order=250, help="UTC Time Offset")
        t.column('Version', 'character', format="x(2)", initial="", max_width=4, label="Version", column_label="Version", position=27, order=260, help="Spec. version number")
        t.column('AcCode', 'character', format="x(12)", initial="", max_width=24, label="AcCode", column_label="AcCode", position=28, order=270, help="International Access Code")
        t.column('Ccode', 'character', format="x(8)", initial="", max_width=16, label="Country", column_label="Country", position=29, order=280, help="Country Code")
        t.column('Releas', 'integer', format=">9", initial="0", max_width=4, label="Release", column_label="Release", position=30, order=290, help="Release Version")
        t.column('CrTime', 'character', format="x(6)", initial="", max_width=12, label="CreateTime", column_label="CreateTime", position=31, order=300, help="File Creation Time")
        t.column('FileType', 'character', format="x(1)", initial="", max_width=2, label="FileType", column_label="FileType", position=32, order=310, help="File Type Indicator")
        t.column('TollFee', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, position=33, order=320, help="Toll fee")
        t.column('CamelFee', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, position=34, order=330, help="Camel fee")
        t.index('mtxfork', [['Station'], ['FileNumber', 'DESC'], ['mc-seq', 'DESC']], area="Sta_Index_3", primary=True, unique=True)
        t.index('filename', [['FileName']], area="Sta_Index_3", unique=True)
        t.index('FileSeq', [['FileSeq', 'DESC']], area="Sta_Index_3", unique=True)
        t.index('FromDate', [['FromDate', 'DESC'], ['FromTime', 'DESC'], ['Station']], area="Sta_Index_3")

    def down(self):
        self.drop_table('MCDRFile')
