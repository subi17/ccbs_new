from gearbox.migrations import Migration

class AddJobQueue(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('JobQueue', area='Sta_Data_256',
                       label='Job Queue',
                       dump_name='jobqueue',
                       desc='Job Queue')
        t.column('JobNum', 'integer', format='>>>,>>>,>>9', initial='0',
                 column_label='Job Id',
                 help='Job Id. Number which identify job')
        t.column('JobState', 'integer', format='9', initial='0',
                 label='Status',
                 help='Job status',
                 description='0 = queue\
5 = running\
6 = aborted\
7 = error\
9 = done')
        t.column('Priority', 'integer', format='>9', initial='90',
                 help='Queue priority 0 = faster.. 99=slower.. 90=default')
        t.column('RunDate', 'date', format='99.99.99',
                 label='Run Date',
                 help='Date when launch')
        t.column('RunTime', 'character', initial='',
                 label='Run Time',
                 help='Time when launch')
        t.column('QueueDate', 'date', format='99.99.99',
                 label='Queued Date',
                 help='Date when put to queue')
        t.column('QueueTime', 'character', initial='',
                 label='Queued Time',
                 help='Time when put to queue')
        t.column('ProgramCode', 'character', format='X(12)', initial='')
        t.column('UserCode', 'character', initial='',
                 help='Job owner')
        t.column('UserName', 'character', format='X(50)', initial='',
                 label='User name',
                 help='Job owner real name')
        t.column('InputData', 'character', format='X(256)', initial='',
                 label='Input Data',
                 help='starEntry list')
        t.column('CommitDate', 'date', format='99.99.99',
                 label='Commit Date',
                 help='Date when job is commited')
        t.column('CommitTime', 'character', initial='',
                 label='Commit time',
                 help='Time when job is commited')
        t.column('TotalTime', 'character', format='X(10)', initial='',
                 label='Total time',
                 help='Total time. Format "HHHH:MM:SS"')
        t.column('queueNumber', 'integer', format='>>9', initial='0',
                 label='Queue number',
                 help='0=workstation,1...999 queues')
        t.column('jobgroup', 'character', format='X(10)', initial='',
                 label='Job group',
                 help='Job group code')
        t.column('number', 'integer', format='>9', initial='0',
                 label='Number',
                 help='Job groups sequence number')
        t.column('archive', 'character', format='X(10)', initial='Default',
                 label='Archive',
                 help='Archive time',
                 description='Default\
Year\
Month\
Week\
Permanent')
        t.column('logfile', 'character', format='X(78)', initial='',
                 label='Log File',
                 help='Log File information',
                 description='max 16384 characters')
        t.index('archive', ['archive', 'CommitDate'], area='Sta_Index_2')
        t.index('JobGroup', ['jobgroup', 'number', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('JobNum', ['JobNum'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('JobState', ['JobState', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('Priority', ['JobState', 'Priority', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('programCode', ['ProgramCode', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('Queue', ['queueNumber', 'JobState', 'Priority', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('UserCode', ['UserCode', 'JobState', 'QueueDate', 'QueueTime'], area='Sta_Index_2')
        t.index('UserCode_ProgramCode', ['UserCode', 'ProgramCode', ('QueueDate', 'DESCENDING'), ('QueueTime', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('JobQueue')

