from gearbox.migrations import Migration

class AddM2MQueue(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('M2MQueue', area='Sta_Data_32',
                       dump_name='m2mqueue')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('CLIType', 'character', format='x(3)', initial='',
                 column_label='CLIType',
                 help='CLI type info')
        t.column('TSCreated', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created')
        t.column('TSNumpac', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='TS Numpac',
                 column_label='TS Numpac')
        t.column('TSRequest', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='TS Request',
                 column_label='TS Request')
        t.column('TSResponse', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='TS Response',
                 column_label='TS Response')
        t.column('XMLMessage', 'character', initial='',
                 label='XML Message',
                 column_label='XML Message')
        t.column('Response', 'character', initial='',
                 column_label='Response',
                 help='Response/error message')
        t.column('ReqStatus', 'integer', format='>>9', initial='999',
                 label='Status',
                 column_label='Status')
        t.column('Command', 'character', format='x(4)', initial='',
                 column_label='Command')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('AgrCust', 'integer', format='>>>>>>>9', initial='0',
                 label='Agreement Customer',
                 column_label='Agr.Cust',
                 help='Agreement customer')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID, 1 - 8 characters')
        t.column('UserDate', 'date', format='99-99-99',
                 column_label='UsedDate',
                 help='User / handling date')
        t.column('ReqType', 'integer', format='>9', initial='0',
                 label='Req.Type',
                 column_label='Req.Type',
                 help='Type of request (f.ex in or out)')
        t.column('RequestId', 'character', format='x(12)', initial='',
                 column_label='RequestId')
        t.column('CustType', 'integer', format='>9', initial='0',
                 label='Customer Type',
                 column_label='CustType',
                 help='Customer type: person/company')
        t.column('M2MRequest', 'integer', format='>>>>>>9', initial='0')
        t.column('NumDetails', 'integer', format='>>>9', initial='0',
                 column_label='NumDetails',
                 help='Nr. of details (amount)')
        t.column('CommNum', 'integer', format='>>9', initial='0',
                 column_label='CommNum',
                 help='Command as integer')
        t.column('AddtlInfo', 'character', format='x(40)', initial='',
                 label='Info',
                 column_label='Info',
                 help='Additional information text')
        t.index('CommNum', ['CommNum'], area='Sta_Index_2')
        t.index('M2MRequest', ['M2MRequest', 'TSCreated'], area='Sta_Index_2',
                primary=True)
        t.index('ReqStatus', ['ReqType', 'ReqStatus'], area='Sta_Index_2')
        t.index('RequestId', ['ReqType', 'RequestId'], area='Sta_Index_2')

    def down(self):
        self.drop_table('M2MQueue')

