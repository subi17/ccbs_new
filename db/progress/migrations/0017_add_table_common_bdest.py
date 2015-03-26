from gearbox.migrations import Migration

class AddBDest(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BDest', area='Sta_Data_128',
                       label='B-Destinations',
                       dump_name='bdest',
                       desc='B-Destinations')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-number    ')
        t.column('BDName', 'character', format='x(30)', initial='',
                 label='DestName',
                 column_label='DestName',
                 help='Destination\'s name ')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 label='Country',
                 column_label='Country',
                 help='Consecutive Country/Service Number')
        t.column('Class', 'integer', format='9', initial='1',
                 column_label='Cl ',
                 help='Class: 1: A-sub pays 2: Freephone, B-sub pays')
        t.column('bt-prate', 'decimal', decimals=5, format='zz9.99999', initial='0',
                 label='Rate/s',
                 column_label='Rate/s',
                 help='How much this service provider receives, 1/100 /Sec')
        t.column('bt-pros', 'integer', format='zz9', initial='0',
                 label='Proc',
                 column_label='Proc',
                 help='How many % shall be paid to SP monthly')
        t.column('CDestArea', 'character', format='x(4)', initial='',
                 label='Area Code',
                 column_label='Acode',
                 help='Area Code for c-number')
        t.column('CDest', 'character', format='x(15)', initial='',
                 label='C-subNo',
                 column_label='C-subNo',
                 help='C-number')
        t.column('PNPCustNum', 'integer', mandatory=True, format='>>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number, 1 ... 999999')
        t.column('Free', 'logical', initial='no',
                 column_label='Free',
                 help='Are calls to this B-number free of charge ?')
        t.column('DestType', 'integer', format='>9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='B-subscriber type, used with FTAM server.')
        t.column('FTAM', 'logical', initial='no',
                 column_label='FTAM',
                 help='Send this b-number to FTAM configuration file')
        t.column('DiscGroup', 'character', initial='',
                 label='Discount Group',
                 column_label='Discount Group',
                 help='Discount Group Code')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='Bill Target',
                 column_label='BT',
                 help='Customer\'s Billing Target')
        t.column('PNPBillTarget', 'integer', format='z9', initial='0',
                 label='PNP Bill Target',
                 column_label='PNP BT',
                 help='PNP Customer\'s billing target')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Last effective day')
        t.index('BDest', ['Brand', 'PNPCustNum', 'BDest'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('BDName', ['Brand', 'BDName', 'BDest'], area='Sta_Index_2')
        t.index('CCN', ['Brand', 'CCN', 'BDName', 'BDest'], area='Sta_Index_2')
        t.index('DestType', ['Brand', 'PNPCustNum', 'BDest', 'DestType'], area='Sta_Index_2')
        t.index('ws-bsub', ['Brand', 'BDest'], area='Sta_Index_2')

    def down(self):
        self.drop_table('BDest')

