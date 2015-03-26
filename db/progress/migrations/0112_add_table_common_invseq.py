from gearbox.migrations import Migration

class AddInvSeq(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvSeq', area='Sta_Data_128',
                       label='Invoice Sequence',
                       dump_name='invseq',
                       desc='Customers invoice sequences')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer\'s number')
        t.column('FromDate', 'date', format='99-99-99',
                 label='DateFrom',
                 column_label='DateFrom',
                 help='FROM date for sequence')
        t.column('ToDate', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='TO date for sequence')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='Calls invoice sequence')
        t.column('Billed', 'logical', initial='no',
                 column_label='Billed',
                 help='Is this item billed (y/n)')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Consecutive Invoice Number, 1 ... 99999999')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.index('CustNum', ['CustNum', ('FromDate', 'DESCENDING'), ('ToDate', 'DESCENDING'), ('Billed', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('InvSeq', ['InvSeq'], area='Sta_Index_1')
        t.index('MsSeq', ['MsSeq', 'CustNum', 'Billed', ('ToDate', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('InvSeq')

