from gearbox.migrations import Migration

class AddSubSerPara(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SubSerPara', area='Sta_Data_128',
                       dump_name='subserpa')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Service Component',
                 help='Code of Service Component')
        t.column('ParaValue', 'character', format='x(25)', initial='',
                 label='Value',
                 column_label='Value',
                 help='Value of Subscription service parameter')
        t.column('ParaName', 'character', format='x(25)', initial='',
                 column_label='ParaName',
                 help='Name of the Subscription parameter name')
        t.column('SSDate', 'date', format='99-99-99',
                 label='Activation Date',
                 column_label='Date',
                 help='Date when activated')
        t.column('SologStat', 'integer', format='9', initial='0',
                 label='Solog Status',
                 column_label='HLR',
                 help='Solog status of attribute (sent to HLR)',
                 description='0=no need to send, 1=should be sent, 2=sent (solog created)')
        t.column('PDecValue', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Decimal Value',
                 column_label='DecValue',
                 help='Decimal parameter value')
        t.index('MSSeq', ['MsSeq', 'ServCom', 'ParaName', ('SSDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('SubSerPara')

