from gearbox.migrations import Migration

class AddCallScanner(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CallScanner', area='Sta_Data_32',
                       dump_name='callscan')
        t.column('TMSTime', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='TMS Time',
                 column_label='TMS Time',
                 help='Timestamp')
        t.column('UserCode', 'character', initial='',
                 label='UserId',
                 column_label='UserId',
                 help='User ID who makes the CallScanner event')
        t.column('SystemID', 'character', initial='',
                 column_label='SystemID',
                 help='System ID')
        t.column('EventType', 'character', initial='',
                 column_label='EventType')
        t.column('ReasonCode', 'character', initial='',
                 label='ReasonC',
                 column_label='ReasonC',
                 help='Reason code')
        t.column('Level', 'character', initial='',
                 column_label='Level')
        t.column('Target', 'character', format='x(20)', initial='',
                 column_label='Target')
        t.column('StartTime', 'character', format='x(20)', initial='',
                 column_label='StartTime',
                 help='StartTime From')
        t.column('EndTime', 'character', format='x(20)', initial='',
                 column_label='EndTime',
                 help='EndTime to')
        t.column('SearchRule', 'character', format='x(40)', initial='',
                 column_label='SearchRule')
        t.index('ReasonCode', ['ReasonCode', ('TMSTime', 'DESCENDING')], area='Sta_Index_1')
        t.index('TMSTime', [('TMSTime', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('UserCode', ['UserCode'], area='Sta_Index_1')

    def down(self):
        self.drop_table('CallScanner')

