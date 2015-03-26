from gearbox.migrations import Migration

class AddDCCounter(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('DCCounter', area='Sta_Data_128',
                       dump_name='dccounte')
        t.column('DCEvent', 'character', format='x(12)', initial='',
                 label='Periodical Term',
                 column_label='Term',
                 help='ID of periodical term')
        t.column('DCDate', 'date', format='99-99-9999',
                 label='Date',
                 column_label='Date',
                 help='Day of the daily campaign')
        t.column('MSSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Sub.ID',
                 help='Sequence for a subscription')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('Amount', 'decimal', decimals=5, format='>>9.99999', initial='0',
                 column_label='Amount',
                 help='Used amount')
        t.column('MaxCharge', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Max. Charge',
                 column_label='MaxCharge',
                 help='Max. charge')
        t.column('DCTarget', 'character', format='x(12)', initial='',
                 label='Target',
                 column_label='Target',
                 help='Target (allowed billing item)')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillCode',
                 help='Billing item code')
        t.column('CCN', 'integer', format='>>>9', initial='0',
                 help='Report CCN that is marked to CDRs')
        t.column('DCType', 'character', initial='',
                 label='Campaign Type',
                 column_label='Type',
                 help='Campaign type')
        t.column('CalcMethod', 'integer', format='9', initial='0',
                 label='Calculation Method',
                 column_label='Method',
                 help='Calculation method')
        t.index('DCEvent', ['DCEvent', 'DCDate'], area='Sta_Index_2')
        t.index('MSSeq', ['MSSeq', 'DCDate', 'DCTarget'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('DCCounter')

