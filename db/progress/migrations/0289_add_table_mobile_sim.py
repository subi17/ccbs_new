from gearbox.migrations import Migration

class AddSIM(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SIM', area='Sta_Data_64',
                       dump_name='sim',
                       desc='Sim cards')
        t.column('ICC', 'character', format='x(20)', initial='',
                 label='SIM Serial no.',
                 column_label='SIM Serial no. (ICC)',
                 help='Serial no. (ICC) of an individual SIM card')
        t.column('AcClass', 'character', initial='',
                 label='ACClass',
                 column_label='AcClass',
                 help='Access Control Class')
        t.column('SimType', 'character', format='x(12)', initial='',
                 label='Type',
                 column_label='Type')
        t.column('ManCode', 'character', initial='',
                 label='Manufacturer',
                 column_label='Manufacturer')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number IF this SIM is delivered to a customer')
        t.column('SimStat', 'integer', format='z9', initial='0',
                 label='Status',
                 column_label='Status',
                 help='SIM Status Code')
        t.column('SimArt', 'character', format='x(12)', initial='',
                 column_label='SimArt',
                 help='Article Code for a SIM type')
        t.column('BillLevel', 'character', format='x(10)', initial='',
                 label='Level',
                 column_label='Level',
                 help='Hierarchical level code of customer\'s billing structure')
        t.column('Stock', 'character', initial='',
                 column_label='Stock',
                 help='Code of Stock Where This Sim is (or was) located')
        t.column('ISC1', 'character', format='x(4)', initial='',
                 column_label='ISC1',
                 help='Issuer\'s Secret Code')
        t.column('SimBatch', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Batch Sequence',
                 column_label='Batch Seq.',
                 help='Sequence for a Simbatch')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('DealerStat', 'integer', format='z9', initial='0',
                 label='DealerStatus',
                 column_label='DealerStatus',
                 help='SIM Status Code for dealer')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.index('CustNum', ['Brand', 'CustNum', 'BillLevel', 'ICC'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'BillLevel', 'ICC'], area='Sta_Index_2',
                unique=True)
        t.index('ISC1', ['Brand', 'ISC1', 'ICC'], area='Sta_Index_2')
        t.index('SimArt', ['Brand', 'SimArt', 'ICC'], area='Sta_Index_2')
        t.index('SimBatch', ['Brand', 'SimBatch', 'ICC'], area='Sta_Index_2')
        t.index('SimSer', ['Brand', 'ICC'], area='Sta_Index_2',
                unique=True)
        t.index('simseSta_s', ['ICC'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('SimStat', ['Brand', 'Stock', 'SimStat', 'ICC'], area='Sta_Index_2')
        t.index('Stock', ['Brand', 'Stock', 'ICC'], area='Sta_Index_2')

    def down(self):
        self.drop_table('SIM')

