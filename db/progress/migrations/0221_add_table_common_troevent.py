from gearbox.migrations import Migration

class AddTroEvent(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroEvent', area='Sta_Data_256',
                       label='Trouble Ticket Events',
                       dump_name='troevent',
                       desc='Trouble Ticket events')
        t.column('TTNum', 'integer', format='>,>>>,>>9', initial='0',
                 label='Reference',
                 column_label='Reference',
                 help='Reference no. for Customer',
                 description='Auto-generated sequence number for Trouble Ticket')
        t.column('PlanStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Planned',
                 column_label='Planned',
                 help='Planned Date and time of Activity',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('StartStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Started',
                 column_label='Started',
                 help='Actual Start Date and time of Activity',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Handler', 'character', initial='',
                 label='Responsible Person',
                 column_label='Responsible Person',
                 help='Code of the responsible person',
                 description='Responsible Person')
        t.column('Active', 'character', format='x(30)', initial='',
                 label='Activity',
                 column_label='Activity',
                 help='Action to be performed')
        t.column('ActType', 'character', format='x(1)', initial='',
                 label='Activity Type',
                 column_label='Activity Type',
                 help='Type of Activity to be performed')
        t.column('EndStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Ended',
                 column_label='Ended',
                 help='Actual End Date and time of Activity',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Duration', 'decimal', format='>>9.99', initial='0.00',
                 column_label='Duration',
                 help='Time Taken to complete the activity')
        t.column('Priority', 'character', initial='',
                 column_label='Priority',
                 help='Priority of activity - High,Medium or Low')
        t.column('Remarks', 'character', format='x(60)', initial='',
                 column_label='Remarks',
                 help='Activity Remarks - if any')
        t.index('handend', ['Handler', 'EndStamp'], area='Sta_Index_2')
        t.index('handler', ['Handler', 'PlanStamp'], area='Sta_Index_2')
        t.index('seqend', ['Handler', 'EndStamp'], area='Sta_Index_2')
        t.index('TTNum', ['TTNum', 'PlanStamp'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TroEvent')

