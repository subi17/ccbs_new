from gearbox.migrations import Migration

class AddMSOwner(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSOwner', area='Sta_Data_64',
                       label='Owner of a MSISDN Number',
                       dump_name='msowner1',
                       desc='\
\
\
')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='InvTarg',
                 column_label='IT',
                 help='Invoicing Target No')
        t.column('TsBegin', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Usage begun',
                 column_label='Usage begun',
                 help='Time Stamp: when usage begun')
        t.column('TsEnd', 'decimal', decimals=5, format='99999999.99999', initial='99999999,99999',
                 label='Usage ended',
                 column_label='Usage ended',
                 help='Time Stamp, when usage ended')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription',
                 description='Id of a mobsub record. Note that mobsub can have been deleted')
        t.column('IMSI', 'character', format='x(18)', initial='',
                 label='IMSI Number',
                 column_label='IMSI Number')
        t.column('CliEvent', 'character', format='x(1)', initial='',
                 label='Type',
                 column_label='Type',
                 help='Type of MSISDN number')
        t.column('Clitype', 'character', initial='',
                 label='MType',
                 column_label='Mtype',
                 help='Type Of Mobsub (connection type)')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('Contract', 'character', initial='',
                 label='ContractID',
                 column_label='ContrID',
                 help='Contract ID')
        t.column('RepCodes', 'character', format='x(30)', initial='',
                 label='ReportCode',
                 column_label='ReportCode',
                 help='Report Codes')
        t.column('InPortOper', 'character', initial='',
                 column_label='InPortOper',
                 help='In porter operator code')
        t.column('OutPortOper', 'character', initial='',
                 column_label='OutPortOper',
                 help='Out ported operator code')
        t.column('AgrCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Agr.Customer',
                 column_label='AgrCust',
                 help='Agreement customer\'s number')
        t.column('InvCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Inv.Customer',
                 column_label='InvCust',
                 help='Invoicing customer\'s number')
        t.column('PayType', 'logical', format='PrePaid/PostPaid', initial='FALSE',
                 column_label='PayType')
        t.index('AgrCust', ['AgrCust', 'CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1')
        t.index('BillTarget', ['Brand', 'CustNum', 'BillTarget'], area='Sta_Index_1')
        t.index('CLI', ['Brand', 'CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1',
                primary=True, unique=True)
        t.index('CLIEvent', ['CLI', 'TsBegin', 'TsEnd', 'CliEvent'], area='Sta_Index_1')
        t.index('CLI_s', ['CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1',
                unique=True)
        t.index('Contract', ['Brand', 'Contract'], area='Sta_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', 'CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1',
                unique=True)
        t.index('CustNum_s', ['CustNum', 'CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1')
        t.index('imsi', ['Brand', 'IMSI', ('TsBegin', 'DESCENDING')], area='Sta_Index_1')
        t.index('imsi_s', ['IMSI', ('TsBegin', 'DESCENDING')], area='Sta_Index_1')
        t.index('InvCust', ['InvCust', 'CLI', ('TsEnd', 'DESCENDING')], area='Sta_Index_1')
        t.index('MsSeq', ['MsSeq', ('TsBegin', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('MSOwner')

