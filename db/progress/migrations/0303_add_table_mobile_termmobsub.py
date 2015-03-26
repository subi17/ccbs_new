from gearbox.migrations import Migration

class AddTermMobSub(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TermMobSub', area='Sta_Data_32',
                       label='Mobile Subscriber',
                       dump_name='Termms',
                       desc='Mobile Subscription')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('IMSI', 'character', format='x(18)', initial='',
                 label='IMSI Number',
                 column_label='IMSI Number')
        t.column('ICC', 'character', format='x(20)', initial='',
                 label='SIM Serial',
                 column_label='SIM Serial no. (ICC)',
                 help='Serial no. (ICC) of an individual SIM card')
        t.column('CreationDate', 'date', format='99-99-99',
                 label='Created',
                 help='Date when subscription was created (from order)')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('Reseller', 'character', initial='',
                 column_label='Reseller',
                 help='An unique code for a reseller')
        t.column('Contract', 'logical', initial='no',
                 label='ContSign',
                 column_label='ContSign',
                 help='Contract signed')
        t.column('ActivationDate', 'date', format='99-99-99',
                 label='Activation Date',
                 column_label='Activation',
                 help='Date when subscription is to be activated in HLR')
        t.column('MsStatus', 'integer', format='9', initial='1',
                 label='Status',
                 column_label='Status',
                 help='Status Code, 1 ... 9')
        t.column('RepCodes', 'character', format='x(30)', initial='',
                 label='ReportCode',
                 column_label='ReportCode',
                 help='Report Codes')
        t.column('CliType', 'character', initial='',
                 label='MType',
                 column_label='Mtype',
                 help='Type Of TermMobSub')
        t.column('ActivationTS', 'decimal', decimals=5, format='999999.99999', initial='0',
                 label='Activated',
                 help='Time when subscription was activated in TMS (HLR response OK)')
        t.column('SIMDelDate', 'date', format='99-99-99',
                 label='DeliDate',
                 column_label='DeliDate',
                 help='Date When Sim Card delivery request was sent')
        t.column('SimDelStatus', 'integer', format='9', initial='0',
                 label='DeliStat',
                 column_label='DeliStat',
                 help='Status of SIM Card Delivery')
        t.column('SIMActDate', 'date', format='99-99-99',
                 label='sadate',
                 column_label='sadate',
                 help='Date When Sim delevery request was saved')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='Bill.Target',
                 column_label='BT',
                 help='Customer\'s billing target')
        t.column('ServiceChanges', 'integer', format='>9', initial='0',
                 column_label='ServiceChanges',
                 help='Indicates if user is allowed to do changes (0,1,2,3)')
        t.column('MNPChannel', 'integer', format='>9', initial='0',
                 label='MNP Channel',
                 column_label='MNPChannel')
        t.column('InvCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Inv.Customer',
                 column_label='InvCust',
                 help='Invoicing customer\'s number')
        t.column('AgrCust', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Agr.Customer',
                 column_label='AgrCust',
                 help='Agreement customer\'s number')
        t.column('PayType', 'logical', format='PrePaid/PostPaid', initial='no',
                 column_label='PayType')
        t.column('IDCode', 'character', format='x(4)', initial='',
                 label='ID Code',
                 column_label='ID Code',
                 help='Identification Code',
                 description='4 digit code used to identify subscription')
        t.column('SegmentCode', 'character', initial='',
                 label='Segmentation Code',
                 column_label='SegmentCode',
                 help='Segmentation code')
        t.column('SegmentDate', 'date', format='99-99-99',
                 label='Segmentation Date',
                 column_label='SegmentDate',
                 help='Date when segmentation code was changed')
        t.index('AgrCust', ['Brand', 'AgrCust', 'CLI'], area='Sta_Index_1')
        t.index('CLI', ['Brand', 'CLI'], area='Sta_Index_1')
        t.index('CLIType', ['Brand', 'CliType', 'CLI'], area='Sta_Index_1')
        t.index('CLI_u', ['CLI'], area='Sta_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', 'BillTarget', 'CLI'], area='Sta_Index_1')
        t.index('CustNum_u', ['CustNum', 'BillTarget', 'CLI'], area='Sta_Index_1')
        t.index('IMSI', ['Brand', 'IMSI'], area='Sta_Index_1')
        t.index('InvCust', ['Brand', 'InvCust', 'CLI'], area='Sta_Index_1')
        t.index('MNPChannel', ['Brand', 'MNPChannel', 'MsStatus'], area='Sta_Index_1')
        t.index('MsSeq', ['MsSeq'], area='Sta_Index_1',
                primary=True)
        t.index('MsStatus', ['Brand', 'MsStatus', 'ActivationDate'], area='Sta_Index_1')
        t.index('MsStatus_u', ['MsStatus', 'ActivationDate'], area='Sta_Index_1')

    def down(self):
        self.drop_table('TermMobSub')

