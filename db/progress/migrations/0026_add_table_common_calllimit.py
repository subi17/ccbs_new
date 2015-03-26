from gearbox.migrations import Migration

class AddCallLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CallLimit', area='Sta_Data_128',
                       dump_name='calllimi')
        t.column('CustNo', 'integer', format='zzzzzzzzz', initial='0',
                 column_label='CustNo',
                 help='Customer Number')
        t.column('cli', 'character', format='X(16)', initial='',
                 label='Cli',
                 column_label='Cli',
                 description='The cli number (A-number)')
        t.column('DeliType', 'integer', format='>9', initial='0',
                 label='DelivereType',
                 column_label='DT')
        t.column('DeliPara', 'character', format='x(40)', initial='',
                 label='DeliverValue',
                 column_label='DV')
        t.column('limit', 'integer', format='>>9', initial='0',
                 column_label='limit',
                 description='limit')
        t.column('Dfrom', 'date', format='99-99-99', initial='today',
                 label='Valid From',
                 column_label='From',
                 help='Call Limit valid from')
        t.column('Dto', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Call limit valid to')
        t.column('ActStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Activated',
                 column_label='Activated',
                 help='Date and Time When Call Limit Record was last activated',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('CLSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Call Limit Sequence')
        t.column('CreditType', 'integer', format='>9', initial='0',
                 column_label='CreditType',
                 help='Credit Type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Cli', ['Brand', 'cli', ('Dto', 'DESCENDING')], area='Sta_Index_2')
        t.index('Cli_s', ['cli', ('Dto', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNo', ['Brand', 'CustNo', 'cli', ('Dto', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CallLimit')

