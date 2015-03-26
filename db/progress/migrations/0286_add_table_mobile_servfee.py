from gearbox.migrations import Migration

class AddServFee(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ServFee', area='Sta_Data_128',
                       label='ServFee',
                       dump_name='servfee',
                       desc='Fees from activating services\
')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('ServType', 'character', initial='',
                 label='Service Type',
                 column_label='Type',
                 help='Type of service',
                 description='e.g. Report')
        t.column('ServKey', 'character', format='x(12)', initial='',
                 label='Service Key',
                 column_label='Key',
                 help='Key value of service',
                 description='e.g. report number')
        t.column('EventType', 'integer', format='9', initial='0',
                 label='Event Type',
                 column_label='Event',
                 help='Event type')
        t.column('FeeModel', 'character', format='x(16)', initial='',
                 label='Fee Model',
                 column_label='FModel',
                 help='Fees that are created when this event occurs')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Valid to')
        t.column('InterAct', 'logical', initial='no',
                 column_label='InterAct',
                 help='Ask before create new billing event')
        t.column('InvInfo', 'character', format='x(60)', initial='',
                 label='Invoice Info',
                 column_label='Info',
                 help='Info to be written on invoice')
        t.index('ServType', ['Brand', 'ServType', 'ServKey', 'EventType', ('ToDate', 'DESCENDING')], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ServFee')

