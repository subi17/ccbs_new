from gearbox.migrations import Migration

class AddHighUsage(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('HighUsage', area='Sta_Data_64',
                       dump_name='highusag')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='High usage Invoice sequence')
        t.column('CLI', 'character', format='x(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('Qty', 'integer', format='>>>>>>9', initial='0',
                 label='QTY',
                 column_label='QTY',
                 help='Amount of call to this  Invseq/cli')
        t.column('Duration', 'integer', format='>>>>>>>>9', initial='0.00',
                 column_label='Duration',
                 help='duration')
        t.column('Amount', 'decimal', decimals=2, format='z,zzz,zz9.99', initial='0',
                 label='Price',
                 column_label='Price',
                 help='Total Price')
        t.column('HiUsageStatus', 'integer', format='>9', initial='0',
                 label='Status',
                 column_label='Status')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='When was the order created',
                 description='Create timestamp')
        t.column('ChStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Change',
                 column_label='Change',
                 help='When was the order changed',
                 description='Change timestamp')
        t.column('Category', 'character', format='x(4)', initial='',
                 label='Cat',
                 column_label='Cat',
                 help='Category code')
        t.column('Date', 'date', format='99-99-99',
                 column_label='Date',
                 help='Date when last updated')
        t.column('date%', 'decimal', decimals=2, format='>>>>9.99-', initial='0',
                 label='Date%',
                 column_label='Date%')
        t.column('DateGrow', 'decimal', decimals=2, initial='0',
                 column_label='DateGrow')
        t.column('launch', 'character', initial='',
                 label='Launch',
                 column_label='Launch')
        t.index('Amount', [('Amount', 'DESCENDING')], area='Sta_Index_2')
        t.index('CLI', ['CLI', ('CrStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('HiUsageStatus', ['HiUsageStatus', ('CrStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('InvSeq', ['InvSeq', 'CLI'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('HighUsage')

