from gearbox.migrations import Migration

class AddMCDRFile(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MCDRFile', area='Sta_Data_32',
                       label='Mobile CDR start/end rec',
                       dump_name='mcdrfile',
                       desc='\
')
        t.column('CMT', 'character', mandatory=True, format='x(3)', initial='',
                 label='Call Module Type',
                 column_label='Call Module Type')
        t.column('Station', 'character', format='x(4)', initial='',
                 column_label='Station')
        t.column('FromDate', 'character', initial='',
                 label='From Date',
                 column_label='From Date',
                 help='Calls From (Date) YYYYMMDD')
        t.column('FromTime', 'character', format='x(6)', initial='',
                 label='From Time',
                 column_label='From Time',
                 help='Calls From (Time) HHMMSS')
        t.column('ToDate', 'character', initial='',
                 label='Call Date',
                 column_label='Call Date',
                 help='Calls till (date) YYYYMMDD')
        t.column('ToTime', 'character', format='x(6)', initial='',
                 label='Call Time',
                 column_label='Call Time',
                 help='Calls till (time) HHMMSS')
        t.column('FileNumber', 'integer', format='999999', initial='0',
                 label='FLOPNR',
                 column_label='FLOPNR',
                 help='Consecutive Number of a Mobile CDR File')
        t.column('MTXID', 'character', format='xx', initial='',
                 column_label='MTXID',
                 help='Flex MTXID')
        t.column('Qty', 'integer', format='zzzzzz9', initial='0',
                 label='Calls',
                 column_label='Calls',
                 help='Number of Call (Trans) Records in this file')
        t.column('RecordInQtu', 'decimal', decimals=0, format='zzzzzzzzzzz9', initial='0',
                 label='ANTPOSTIN',
                 column_label='ANTPOSTIN',
                 help='Number Of Records IN')
        t.column('RecordQty', 'decimal', decimals=0, format='zzzzzzzzzzz9', initial='0',
                 label='ANTKPOST',
                 column_label='ANTKPOST',
                 help='Number Of Converted Records')
        t.column('BlockQty', 'decimal', decimals=0, format='zzzzzzzzzzz9', initial='0',
                 label='Blocks',
                 column_label='Blocks',
                 help='Number Of Blocks (In)')
        t.column('mc-seq', 'integer', format='zzzzzzzz9', initial='0',
                 label='Sequence',
                 column_label='Sequence',
                 help='Consecutive Number of one file')
        t.column('FileName', 'character', format='x(50)', initial='',
                 column_label='FileName',
                 help='Name of FLEX CDR file from TELIA')
        t.column('TaxRate', 'decimal', extent=10, decimals=2, format='>9.99', initial='0',
                 label='Tax Rate',
                 column_label='Tax Rate',
                 help='Header Tax Rate')
        t.column('Amount', 'decimal', decimals=3, format='>>>>>>>>>9.999', initial='0',
                 label='Charge',
                 column_label='Charge',
                 help='Total Charge')
        t.column('SPAmt', 'decimal', decimals=3, format='>>>>>>>>>9.999', initial='0',
                 label='sphinta',
                 column_label='sphinta',
                 help='Total Sphinta')
        t.column('FileSeq', 'integer', format='>>>>>>>>>9', initial='0',
                 help='File Sequence')
        t.column('Soper', 'character', format='x(5)', initial='',
                 label='SOper',
                 column_label='SOper',
                 help='Sending Operator')
        t.column('Roper', 'character', format='x(5)', initial='',
                 label='ROper',
                 column_label='ROper',
                 help='Receiving Operator')
        t.column('TaxTreat', 'character', format='x(1)', initial='',
                 label='TaxTreatment',
                 column_label='TaxTreatment',
                 help='Tax Treatment')
        t.column('CrDate', 'character', initial='',
                 label='Created',
                 column_label='Created',
                 help='File Creation Date')
        t.column('TransferDate', 'character', initial='',
                 label='Transmitted',
                 column_label='Transmitted',
                 help='File Transmission Date')
        t.column('CuttingTS', 'character', format='x(11)', initial='',
                 label='CutTs',
                 column_label='CutTs',
                 help='Cut Of Timestamp')
        t.column('UTC', 'character', format='x(5)', initial='',
                 column_label='UTC',
                 help='UTC Time Offset')
        t.column('Version', 'character', format='x(2)', initial='',
                 column_label='Version',
                 help='Spec. version number')
        t.column('AcCode', 'character', format='x(12)', initial='',
                 column_label='AcCode',
                 help='International Access Code')
        t.column('Ccode', 'character', initial='',
                 label='Country',
                 column_label='Country',
                 help='Country Code')
        t.column('Releas', 'integer', format='>9', initial='0',
                 label='Release',
                 column_label='Release',
                 help='Release Version')
        t.column('CrTime', 'character', format='x(6)', initial='',
                 label='CreateTime',
                 column_label='CreateTime',
                 help='File Creation Time')
        t.column('FileType', 'character', format='x(1)', initial='',
                 column_label='FileType',
                 help='File Type Indicator')
        t.column('TollFee', 'decimal', decimals=2, initial='0',
                 help='Toll fee')
        t.column('CamelFee', 'decimal', decimals=2, initial='0',
                 help='Camel fee')
        t.index('filename', ['FileName'], area='Sta_Index_3',
                unique=True)
        t.index('FileSeq', [('FileSeq', 'DESCENDING')], area='Sta_Index_3',
                unique=True)
        t.index('FromDate', [('FromDate', 'DESCENDING'), ('FromTime', 'DESCENDING'), 'Station'], area='Sta_Index_3')
        t.index('mtxfork', ['Station', ('FileNumber', 'DESCENDING'), ('mc-seq', 'DESCENDING')], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MCDRFile')

