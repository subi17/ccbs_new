from gearbox.migrations import Migration

class AddServiceLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ServiceLimit', area='Sta_Data_64',
                       dump_name='service1')
        t.column('ToTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ToTimestamp',
                 column_label='ToTimestamp',
                 help='to timestamp')
        t.column('GroupCode', 'character', format='x(16)', initial='',
                 column_label='Group Code',
                 help='Group Code of Servicelimit')
        t.column('SLSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='SLseq',
                 help='Sequence for Servicelimit')
        t.column('WebDisp', 'integer', format='9', initial='1',
                 label='Web',
                 column_label='Web')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DT',
                 help='Dialling type code')
        t.column('SLCode', 'character', format='x(12)', initial='',
                 label='ServiceLimit',
                 column_label='ServiceLimit',
                 help='Code of Servicelimit')
        t.column('SLName', 'character', format='x(30)', initial='',
                 label='Servicelimit Name',
                 column_label='Servicelimit Name',
                 help='Name of Service limit')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('InclAmt', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Included Amount',
                 column_label='Incl.Amt',
                 help='Amount of billable material that is included in this fee')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 help='Valid from this date on')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 help='Valid until this date')
        t.index('GroupCode', ['GroupCode'], area='Sta_Index_2',
                primary=True)
        t.index('SLSeq', ['SLSeq'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('ServiceLimit')

