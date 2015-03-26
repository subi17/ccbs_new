from gearbox.migrations import Migration

class AddSOLog(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SOLog', area='Sta_Data_32',
                       label='Service Order Log',
                       dump_name='solog',
                       desc='\
')
        t.column('SoLog', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Order Seq',
                 column_label='Order Seq',
                 help='Sequence for a Service Order')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Time Stamp: When Created')
        t.column('ActivationTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Activated',
                 column_label='Activated',
                 help='Time Stamp: Intended Activation')
        t.column('CompletedTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Completed',
                 column_label='Completed',
                 help='Time Stamp: Service order Succesfully Completed')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('Stat', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='St',
                 help='Status Code 0 ... 9')
        t.column('CommLine', 'character', format='x(255)', initial='',
                 label='Command Line',
                 column_label='Command Line',
                 help='Service Order Command Line as sent via SOG GWY')
        t.column('Response', 'character', format='x(70)', initial='',
                 column_label='Response',
                 help='Response/error message')
        t.column('TimeSlotTMS', 'decimal', decimals=6, format='99999999.99999', initial='0',
                 label='ActTime',
                 column_label='ActTime',
                 help='Time Slot for activation')
        t.column('MsALog', 'integer', format='>>>>>>9', initial='0',
                 label='Activation Seq',
                 column_label='Act. Seq',
                 help='Sequence of Send Activation LOG')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('users', 'character', format='x(12)', initial='',
                 label='Users',
                 column_label='Users',
                 help='Who made this Service order log req')
        t.column('MsRequest', 'integer', initial='0',
                 label='Request ID',
                 column_label='Request',
                 help='Unique ID for request')
        t.index('CLI', ['Brand', 'CLI', 'Stat', 'ActivationTS'], area='Sta_Index_2')
        t.index('CLI_s', ['CLI', 'Stat', 'ActivationTS'], area='Sta_Index_2')
        t.index('MsSeq', ['MsSeq', 'Stat', 'ActivationTS'], area='Sta_Index_2',
                primary=True)
        t.index('SoLog', ['Brand', ('SoLog', 'DESCENDING')], area='Sta_Index_2')
        t.index('Solog_s', ['SoLog'], area='Sta_Index_2')
        t.index('Stat', ['Brand', 'Stat', 'ActivationTS', 'SoLog'], area='Sta_Index_2')
        t.index('TimeSlotTMS', ['Brand', 'Stat', 'TimeSlotTMS', 'SoLog'], area='Sta_Index_2')

    def down(self):
        self.drop_table('SOLog')

