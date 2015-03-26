from gearbox.migrations import Migration

class AddPrePaidRequest(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('PrePaidRequest', area='Sta_Data_32',
                       dump_name='prepaidr')
        t.column('PPRequest', 'integer', format='>>>>>>>9', initial='0',
                 column_label='PPRequest')
        t.column('CLI', 'character', initial='',
                 column_label='CLI')
        t.column('Reference', 'character', initial='',
                 column_label='Reference')
        t.column('Request', 'character', initial='',
                 column_label='Request')
        t.column('CommLine', 'character', initial='')
        t.column('Response', 'character', initial='',
                 column_label='Response')
        t.column('TSRequest', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='TSRequest')
        t.column('TSResponse', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='TSResponse')
        t.column('PPStatus', 'integer', format='>>9', initial='0',
                 label='Status',
                 column_label='Status')
        t.column('TopUpAmt', 'decimal', decimals=2, initial='0',
                 label='TopUpAmount',
                 column_label='TopUpAmount')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Source', 'character', initial='',
                 column_label='Source',
                 help='Source of request',
                 description='Source of request')
        t.column('RespCode', 'integer', format='>>9', initial='0',
                 label='ResponseCode',
                 column_label='ResponseCode')
        t.column('VatAmt', 'decimal', decimals=2, initial='0',
                 label='VatAmount',
                 column_label='VatAmount')
        t.column('Fecha', 'character', initial='',
                 column_label='Fecha',
                 description='For ATM double checking')
        t.column('Entidad', 'character', initial='',
                 column_label='Entidad',
                 description='For ATM double checking')
        t.column('ClaveLocal', 'character', initial='',
                 column_label='ClaveLocal',
                 description='For ATM double checking')
        t.column('OrigRequest', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Original Request ID',
                 column_label='Orig.Request',
                 help='Original request ID')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='TaxZone',
                 help='Tax zone')
        t.column('UserCode', 'character', initial='',
                 label='User',
                 column_label='User')
        t.column('PPReqPrefix', 'character', initial='',
                 label='Request Prefix',
                 column_label='Prefix',
                 help='Request prefix')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Subs.ID')
        t.column('ReqCParam1', 'character', format='x(30)', initial='',
                 label='Char Parameter 1',
                 column_label='Char Parameter 1')
        t.index('ATM', ['Entidad', 'Fecha', 'ClaveLocal'], area='Sta_Index_1')
        t.index('CLI', ['Brand', 'CLI', ('TSRequest', 'DESCENDING')], area='Sta_Index_1')
        t.index('MsSeq', ['Brand', 'MSSeq', ('TSRequest', 'DESCENDING')], area='Sta_Index_1')
        t.index('OrigRequest', ['Brand', 'OrigRequest'], area='Sta_Index_1')
        t.index('PPRequest', ['Brand', 'PPRequest'], area='Sta_Index_1',
                primary=True)
        t.index('PPStatus', ['Brand', 'PPStatus', ('TSRequest', 'DESCENDING')], area='Sta_Index_1')
        t.index('Reference', ['Brand', 'Reference', 'Request'], area='Sta_Index_1')
        t.index('Source', ['Brand', 'Source', ('TSRequest', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('PrePaidRequest')

