from gearbox.migrations import Migration

class AddSubSer(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SubSer', area='Sta_Data_128',
                       dump_name='subser',
                       desc='Services of mobile subscribers\
')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('ServPac', 'character', initial='',
                 label='ServPackage',
                 column_label='ServPack',
                 help='Code of ServPack')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Service Component',
                 help='Code of Service Component')
        t.column('SSAData', 'character', extent=5, format='x(40)', initial='',
                 label='Add\'l Data',
                 column_label='Additional Data',
                 help='Additional Data')
        t.column('SSDate', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date',
                 help='Date When Activated')
        t.column('SSParam', 'character', format='x(24)', initial='',
                 label='Parameter',
                 column_label='Parameter',
                 help='Service-oriented, subscriber-specific parameter')
        t.column('SSStat', 'integer', format='>>>9', initial='0',
                 label='Status',
                 column_label='Status',
                 help='Service Status')
        t.column('SologStat', 'integer', format='9', initial='0',
                 label='Solog Status',
                 column_label='HLR',
                 help='Solog status of service (sent to HLR)',
                 description='0=no need to send, 1=should be sent, 2=sent (solog created)')
        t.index('ServCom', ['MsSeq', 'ServCom', ('SSDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ServPac', ['MsSeq', 'ServPac', 'ServCom', ('SSDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('SubSer')

