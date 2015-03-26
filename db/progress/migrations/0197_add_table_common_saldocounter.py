from gearbox.migrations import Migration

class AddSaldoCounter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('SaldoCounter', area='Sta_Data_128',
                       dump_name='callcoun')
        t.column('Period', 'integer', format='999999', initial='0',
                 help='Period what Callcounter is to be')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='MobSub Sequence',
                 column_label='MobSub',
                 help='Link to MobSub')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='Invoice sequence')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Sub',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('Amt', 'decimal', decimals=3, format='->>>,>>9.99', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Unbilled Balance')
        t.column('MobLimit', 'integer', format='>>9', initial='0',
                 label='limit',
                 column_label='limit',
                 description='limit')
        t.column('CustLimit', 'integer', format='>>9', initial='0',
                 label='limit',
                 column_label='limit',
                 description='limit')
        t.column('qty', 'integer', format='>>>,>>>,>>9', initial='0',
                 label='Qty',
                 help='Quantity of PNP Calls')
        t.index('MSSeq', ['MSSeq', ('Period', 'DESCENDING')], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('SaldoCounter')

