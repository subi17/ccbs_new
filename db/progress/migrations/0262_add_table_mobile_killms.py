from gearbox.migrations import Migration

class AddKillMs(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('KillMs', area='Sta_Data_64',
                       label='Request for KILL of a mobsub',
                       dump_name='killms',
                       desc='requests for scheduled deactivations of mob subscribers')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('RequestTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ReqTimeSt',
                 column_label='ReqTimeSt',
                 help='Time stamp when request was saved')
        t.column('ExecuteTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ExecTs',
                 column_label='ExecTs',
                 help='Time Stamp when request was executed')
        t.column('UserCode', 'character', initial='',
                 label='UserId',
                 column_label='UserId',
                 help='User ID who made the KILL request')
        t.column('KillDate', 'date', format='99.99.99',
                 column_label='KillDate',
                 help='Date when subscription shall be killed')
        t.column('Stat', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='St',
                 help='Status 1: Pending 2: Failed 3: Done')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('ErrorMsg', 'character', format='x(78)', initial='',
                 label='Error msg',
                 column_label='Error msg',
                 help='Explanation why execution failed')
        t.column('KeepSIM', 'logical', initial='no',
                 label='Keep SIM',
                 column_label='Keep SIM',
                 help='Retain SIM card of de-activated mobile subscription (Y/N) ?')
        t.column('Stock', 'character', initial='',
                 column_label='Stock',
                 help='Return SIM into stock (enter stock code)')
        t.column('OutOper', 'character', format='x(12)', initial='',
                 label='Operator',
                 column_label='Operator',
                 help='Operator to whom this MSDN shall be outported')
        t.column('KillTime', 'decimal', decimals=2, format='99.99', initial='0',
                 label='KTime',
                 column_label='KTime',
                 help='Time when subscription should be killed')
        t.column('KillDateTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='KillTS',
                 column_label='KillTS',
                 help='Kill Date&Time in a Time Stamp Format')
        t.column('SoSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='OrdSeq',
                 column_label='OrdSeq',
                 help='Sequence for the corresponding "DELETE" Service Order')
        t.column('OutPort', 'logical', initial='no',
                 help='Shall this MSISDN be OUTported (MNP) when killed Y/N ?')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('CLI', ['Brand', 'CLI', 'MsSeq'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('CLI_s', ['CLI', 'MsSeq'], area='Sta_Index_3',
                unique=True)
        t.index('killdate', ['Brand', ('KillDate', 'DESCENDING'), 'CLI'], area='Sta_Index_3',
                unique=True)
        t.index('MsSeq', ['MsSeq', 'Stat'], area='Sta_Index_3',
                unique=True)
        t.index('Stat', ['Brand', 'Stat', 'KillDate', 'CLI'], area='Sta_Index_3')

    def down(self):
        self.drop_table('KillMs')

