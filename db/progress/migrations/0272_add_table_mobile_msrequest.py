from gearbox.migrations import Migration

class AddMSRequest(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSRequest', area='Sta_Data_64',
                       label='MobSub Requests',
                       dump_name='msreques',
                       desc='MobSub related action requests\
')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='MobSub Sequence',
                 column_label='SubSeq',
                 help='Sequence for a subscription')
        t.column('CLI', 'character', format='X(15)', initial='',
                 label='MSISDN',
                 column_label='CLI',
                 help='MSISDN subscriber number')
        t.column('CustNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number',
                 description='mobsub.custnum')
        t.column('Salesman', 'character', initial='',
                 column_label='Sman')
        t.column('ActStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Activation Time',
                 column_label='Activate',
                 help='Time when request will be processed')
        t.column('UserCode', 'character', initial='',
                 label='UserId',
                 column_label='UserId',
                 help='User who made the request')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Creation Time',
                 column_label='Created',
                 help='Time when request was made')
        t.column('ReqStatus', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='Stat',
                 help='Status of request')
        t.column('ReqType', 'integer', format='>9', initial='0',
                 label='Request Type',
                 column_label='Type',
                 help='Type of request')
        t.column('ReqCParam1', 'character', format='x(30)', initial='',
                 label='Char Parameter 1',
                 column_label='CParam1',
                 help='Character parameter 1')
        t.column('ReqCParam2', 'character', format='x(30)', initial='',
                 label='Char Parameter 2',
                 column_label='CParam2',
                 help='Character parameter 2')
        t.column('ReqDParam1', 'decimal', decimals=5, initial='0',
                 label='Dec Parameter 1',
                 column_label='DParam1',
                 help='Decimal parameter 1')
        t.column('ReqDParam2', 'decimal', decimals=5, initial='0',
                 label='Dec Parameter 2',
                 column_label='DParam2',
                 help='Decimal parameter 2')
        t.column('MsRequest', 'integer', initial='0',
                 label='Request ID',
                 column_label='ID',
                 help='Unique ID for request')
        t.column('DoneStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Handling Time',
                 column_label='Handled',
                 help='Time when request was handled (completed)')
        t.column('SendSMS', 'integer', format='9', initial='0',
                 label='Send SMS',
                 column_label='SMS',
                 help='Send SMS when request is handled',
                 description='0=no, 1=to user, 2=to owner, 3=both')
        t.column('CreateFees', 'logical', initial='yes',
                 label='Create Fees',
                 column_label='Fees',
                 help='Create fees that are associated to request')
        t.column('Memo', 'character', format='x(60)', initial='')
        t.column('Forced', 'logical', initial='no',
                 label='Forced Run',
                 column_label='Forced',
                 help='Request has been run immediately, bypassing original scheduling')
        t.column('ReqIParam1', 'integer', format='->>>>>>>9', initial='0',
                 label='Integer Parameter 1',
                 column_label='IntParam1',
                 help='Integer parameter 1')
        t.column('ReqIParam2', 'integer', format='->>>>>>>9', initial='0',
                 label='Integer Parameter 2',
                 column_label='IntParam2',
                 help='Integer parameter 2')
        t.column('ReqCParam3', 'character', format='x(30)', initial='',
                 label='Char Parameter 3',
                 column_label='CParam3',
                 help='Character parameter 3')
        t.column('ReqCParam4', 'character', format='x(30)', initial='',
                 label='Char Parameter 4',
                 column_label='CParam4',
                 help='Character parameter 4')
        t.column('SoLog', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Solog ID',
                 column_label='Solog',
                 help='Sequence for Solog')
        t.column('OrigRequest', 'integer', format='>>>>>>>>9', initial='0',
                 label='Original Request ID',
                 column_label='Orig.ID',
                 help='ID of the original (father) request')
        t.column('ReqSource', 'character', initial='',
                 label='Source',
                 help='Request source (orderer)')
        t.column('ReqIParam3', 'integer', format='->>>>>>>9', initial='0',
                 label='Integer Parameter 3',
                 column_label='IntParam3',
                 help='Integer parameter 3')
        t.column('ReqIParam4', 'integer', format='->>>>>>>9', initial='0',
                 label='Integer Parameter 4',
                 column_label='IntParam4',
                 help='Integer parameter 4')
        t.column('ReqDtParam1', 'date', format='99-99-99',
                 label='Date parameter 1',
                 column_label='DateParam1')
        t.column('ReqDtParam2', 'date', format='99-99-99',
                 label='Date parameter 2',
                 column_label='DateParam2')
        t.column('Mandatory', 'integer', format='9', initial='0',
                 column_label='Mand.',
                 help='This request must be handled before father request can continue')
        t.column('UpdateStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='Last updated',
                 help='When this request was last updated')
        t.column('SMSText', 'character', format='x(12)', initial='',
                 label='SMS Text',
                 column_label='SMS',
                 help='SMS text that is sent')
        t.index('BrandReq', ['Brand', ('MsRequest', 'DESCENDING')], area='Sta_Index_3')
        t.index('BrandType', ['Brand', 'ReqType', ('MsRequest', 'DESCENDING')], area='Sta_Index_3')
        t.index('CLI', ['Brand', 'ReqType', 'CLI', ('ActStamp', 'DESCENDING')], area='Sta_Index_3')
        t.index('CustNum', ['Brand', 'ReqType', 'CustNum', ('ActStamp', 'DESCENDING')], area='Sta_Index_3')
        t.index('MsActStamp', ['MsSeq', ('ActStamp', 'DESCENDING')], area='Sta_Index_3')
        t.index('MsRequest', ['MsRequest'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('MsSeq', ['MsSeq', 'ReqType', 'ReqStatus'], area='Sta_Index_3')
        t.index('OrigRequest', ['OrigRequest'], area='Sta_Index_3')
        t.index('ReqCParam1', ['MsSeq', 'ReqType', 'ReqCParam1', 'ReqStatus'], area='Sta_Index_3')
        t.index('ReqStatus', ['Brand', 'ReqStatus', ('ActStamp', 'DESCENDING')], area='Sta_Index_3')
        t.index('ReqType', ['Brand', 'ReqType', 'ReqStatus', ('ActStamp', 'DESCENDING')], area='Sta_Index_3')
        t.index('UpdateStamp', ['Brand', 'ReqStatus', ('UpdateStamp', 'DESCENDING')], area='Sta_Index_3')

    def down(self):
        self.drop_table('MSRequest')

