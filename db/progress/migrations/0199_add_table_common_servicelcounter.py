from gearbox.migrations import Migration

class AddServiceLCounter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ServiceLCounter', area='Sta_Data_128',
                       dump_name='servcou1')
        t.column('MsSeq', 'integer', initial='0',
                 label='Mobsub',
                 column_label='Msub',
                 help='Link to mobsub-table')
        t.column('Period', 'integer', format='999999', initial='0',
                 help='Period what Servicecounter is to be')
        t.column('invseq', 'integer', format='>>>>>>>>>>9', initial='0')
        t.column('Amt', 'decimal', decimals=3, format='>>>>>>>>>>>>9.999', initial='0')
        t.column('SLSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='SLseq',
                 help='Sequence for Servicelimit')
        t.column('limit', 'integer', format='>>9', initial='0',
                 column_label='limit',
                 description='limit')
        t.column('BilledAmt', 'decimal', decimals=3, format='->>>>>>>>9.999', initial='0',
                 label='Billed Amount',
                 column_label='Billed',
                 help='Billed amount')
        t.index('MSSeq', ['MsSeq', ('Period', 'DESCENDING'), 'SLSeq'], area='Sta_Data_256',
                primary=True)

    def down(self):
        self.drop_table('ServiceLCounter')

