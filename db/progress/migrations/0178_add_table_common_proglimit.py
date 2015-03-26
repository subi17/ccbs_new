from gearbox.migrations import Migration

class AddProgLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ProgLimit', area='Sta_Data_128',
                       dump_name='proglimit')
        t.column('GroupCode', 'character', format='x(16)', initial='',
                 column_label='Group Code',
                 help='Group Code of Servicelimit')
        t.column('SLSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='SLseq',
                 help='Sequence for Servicelimit')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 help='Valid from this date on')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 help='Valid until this date')
        t.column('LimitFrom', 'decimal', decimals=6, format='>>>>>>>>9.999999', initial='0',
                 column_label='LimitFrom',
                 help='Limit value from')
        t.column('LimitTo', 'decimal', decimals=6, format='>>>>>>>>9.999999', initial='0',
                 column_label='LimitTo',
                 help='Limit value to')
        t.column('BDest', 'character', format='x(16)', initial='',
                 column_label='Bdest',
                 help='Billing Destination')
        t.index('LimitTo', ['GroupCode', 'SLSeq', 'LimitTo', 'ValidFrom'], area='Sta_Index_2')
        t.index('ValidTo', ['GroupCode', 'SLSeq', ('ValidTo', 'DESCENDING'), 'LimitTo'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('ProgLimit')

