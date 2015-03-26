from gearbox.migrations import Migration

class AddPPBatch(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PPBatch', area='Sta_Data_256',
                       label='PP Batch',
                       dump_name='ppbatch',
                       desc='Payment plan\'s batches\
')
        t.column('PPlanID', 'integer', format='>>>>>>>9', initial='0',
                 label='Payment Plan ID',
                 column_label='PP ID',
                 help='Payment plan ID')
        t.column('PPBatch', 'integer', format='>9', initial='0',
                 label='Batch',
                 help='Batch number')
        t.column('DueDate', 'date', format='99-99-99',
                 label='Due Date',
                 column_label='DueDate',
                 help='Batche\'s due date')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Amount of batch')
        t.column('BatchFee', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Batch Fee',
                 column_label='Fee',
                 help='Batch fee')
        t.column('PBStatus', 'integer', format='9', initial='0',
                 label='Status',
                 help='Status of batch',
                 description='e.g. unpaid, partly paid, paid')
        t.column('RefNum', 'character', format='x(20)', initial='',
                 label='Reference Nbr',
                 column_label='RefNum',
                 help='Reference Number')
        t.column('PaidAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Paid Amount',
                 column_label='Paid',
                 help='Paid amount')
        t.index('PPlanID', ['PPlanID', 'PPBatch'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PPBatch')

