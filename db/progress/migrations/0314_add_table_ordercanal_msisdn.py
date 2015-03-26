from gearbox.migrations import Migration

class AddMSISDN(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('MSISDN', area='Sta_Data_256',
                       dump_name='msisdn',
                       desc='A Single MSISDN number')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('StatusCode', 'integer', format='z9', initial='1',
                 label='Status',
                 column_label='Status',
                 help='Status Code')
        t.column('MSSeq', 'integer', format='zzzzzzzz9',
                 column_label='MSSEq',
                 help='Sequence for subscriptions')
        t.column('ValidFrom', 'decimal', decimals=5, format='99999999.99999', initial='99999999,99999',
                 label='Usage valid',
                 column_label='Usage valid',
                 help='Time Stamp, when usage begin')
        t.column('ValidTo', 'decimal', decimals=5, format='99999999.99999', initial='99999999,99999',
                 label='Usage valid',
                 column_label='Usage valid to',
                 help='Time Stamp, when usage end')
        t.column('MNP', 'logical', format='MNP/', initial='FALSE',
                 column_label='MNP',
                 help='MNP Number')
        t.column('PayType', 'logical', format='PrePaid/PostPaid', initial='FALSE',
                 column_label='PayType')
        t.column('ActionDate', 'date', format='99-99-99',
                 column_label='PortDate',
                 help='Date when action happens')
        t.column('POS', 'character', format='x(12)', initial='',
                 column_label='POS',
                 help='Point of Sales')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number IF this MSISDN is delivered to a customer')
        t.column('McCode', 'integer', format='zz9', initial='1',
                 label='Class',
                 column_label='Class',
                 help='Code of MSISDN Class')
        t.column('PrevMuSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='SeqNo',
                 column_label='SeqNo',
                 help='Internal, consecutive sequence no of PREVIOUS user')
        t.column('PortingDate', 'date', format='99-99-99',
                 label='PortDate',
                 column_label='PortDate',
                 help='Date when this MSISN no. was ported in or out')
        t.column('PortingTime', 'decimal', decimals=2, format='99.99', initial='0',
                 label='Ptime',
                 column_label='Ptime',
                 help='Time when this MSISDN can be activated (INported) earliest')
        t.column('OutOperator', 'character', format='x(12)', initial='',
                 label='OutOp',
                 column_label='OutOp',
                 help='Name of operator to whom this MSISDN was OUTported')
        t.column('OrderID', 'integer', format='>>>>>>>>9', initial='0',
                 label='OrderId',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('MsisdnType', 'integer', format='>9', initial='0',
                 label='MSISDNType',
                 column_label='MSISDNType',
                 help='MSISDN Type')
        t.column('LockedTo', 'decimal', decimals=2, format='999999.99999', initial='0')
        t.index('ActionDate', ['Brand', 'StatusCode', 'ActionDate'], area='Sta_Index_1')
        t.index('CLI', ['Brand', 'CLI', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CLI_s', ['CLI'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('ValidFrom', 'DESCENDING')], area='Sta_Index_1')
        t.index('MSSeq', ['MSSeq', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('OrderID', ['Brand', ('OrderID', 'DESCENDING')], area='Sta_Index_2')
        t.index('POS', ['POS', 'StatusCode', 'CLI', ('ValidFrom', 'DESCENDING')], area='Sta_Index_1')
        t.index('POSDCLI', ['POS', 'StatusCode', ('CLI', 'DESCENDING'), ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('StatusCode', ['Brand', 'StatusCode', 'CLI', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('MSISDN')

