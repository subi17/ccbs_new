from gearbox.migrations import Migration

class AddMNPCancelProposal(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPCancelProposal', area='Sta_Data_128',
                       dump_name='mnpcancel')
        t.column('ReferenceCode', 'character', format='x(5)', initial='',
                 column_label='ReferenceCode')
        t.column('StatusCode', 'integer', format='>9', initial='0',
                 column_label='StatusCode')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='CreatedTS')
        t.column('MNPSeq', 'integer', initial='0',
                 column_label='MNPSeq')
        t.column('AttachmentFile', 'character', format='x(45)', initial='',
                 column_label='AttachmentFile')
        t.column('Pdf', 'blob', format='x(8)', area='Lob_Data', size='1M')
        t.index('MNPSeq', ['MNPSeq', ('CreatedTS', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('MNPCancelProposal')

