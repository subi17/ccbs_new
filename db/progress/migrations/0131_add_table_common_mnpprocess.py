from gearbox.migrations import Migration

class AddMNPProcess(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPProcess', area='Sta_Data_64',
                       dump_name='mnpproce')
        t.column('OrderId', 'integer', initial='0')
        t.column('FormRequest', 'character', initial='')
        t.column('PortRequest', 'character', initial='')
        t.column('StatusCode', 'integer', initial='0')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.column('UserCode', 'character', initial='')
        t.column('MNPSeq', 'integer', initial='0')
        t.column('UpdateTS', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.column('MNPType', 'integer', format='>9', initial='0',
                 column_label='MNPType',
                 help='MNP process type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('StateFlag', 'integer', format='>9', initial='0',
                 column_label='StateFlag',
                 help='Additional process state info')
        t.column('MNPUpdateTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='MNPUpdateTS')
        t.column('PortingTime', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='PortingTime')
        t.column('StatusReason', 'character', format='x(12)', initial='',
                 column_label='StatusReason')
        t.column('OperCode', 'character', format='x(3)', initial='',
                 column_label='OperCode',
                 help='Operator code (mnp in = donor, mnp out = receptor)')
        t.index('FormRequest', ['FormRequest'], area='Sta_Index_2')
        t.index('MNPSeq', ['MNPSeq'], area='Sta_Index_2',
                primary=True)
        t.index('MNPType', ['Brand', 'MNPType', 'StatusCode', ('CreatedTS', 'DESCENDING')], area='Sta_Index_2')
        t.index('OperCreated', ['Brand', 'MNPType', 'OperCode', ('CreatedTS', 'DESCENDING')], area='Sta_Index_2')
        t.index('OperPorting', ['Brand', 'MNPType', 'OperCode', ('PortingTime', 'DESCENDING')], area='Sta_Index_2')
        t.index('OperStatus', ['Brand', 'MNPType', 'OperCode', 'StatusCode'], area='Sta_Index_2')
        t.index('OrderId', ['OrderId'], area='Sta_Index_2')
        t.index('PortingTime', ['Brand', 'MNPType', 'StatusCode', ('PortingTime', 'DESCENDING')], area='Sta_Index_2')
        t.index('PortRequest', ['PortRequest'], area='Sta_Index_2')
        t.index('StateFlag', ['Brand', 'MNPType', 'StateFlag', 'StatusCode'], area='Sta_Index_2')
        t.index('StatusCode', ['StatusCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MNPProcess')

