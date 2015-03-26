from gearbox.migrations import Migration

class AddMServiceLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MServiceLimit', area='Sta_Data_128',
                       dump_name='mservice')
        t.column('SLSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='SLseq',
                 help='Sequence for Servicelimit')
        t.column('MsSeq', 'integer', initial='0',
                 label='Mobsub',
                 column_label='Msub',
                 help='Link to mobsub-table')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DT',
                 help='Dialling type code')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('InclAmt', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Included Amount',
                 column_label='Incl.Amt',
                 help='Amount of billable material that is included in this fee')
        t.column('FromTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ValidFromTS',
                 column_label='ValidFromTS',
                 help='Valid From TimeStamp')
        t.column('EndTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='EndTS',
                 help='End TimeStamp')
        t.index('Active', ['MsSeq', 'DialType', 'FromTS', 'EndTS'], area='Sta_Index_2')
        t.index('msseq', ['MsSeq', 'DialType', 'SLSeq', ('EndTS', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('slseq', ['SLSeq', 'DialType'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MServiceLimit')

