from gearbox.migrations import Migration

class Addjobgroup(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('jobgroup', area='Sta_Data_256')
        t.column('jobgroup', 'character', format='X(10)', initial='',
                 label='Job group',
                 help='Job group code')
        t.column('description', 'character', format='X(50)', initial='',
                 label='Description',
                 help='Description of group')
        t.column('schedulegroup', 'character', format='x(10)', initial='',
                 label='Schedule group',
                 help='Schedule group code',
                 description='manually,daily,weekly,monthly')
        t.column('scheduleDay', 'character', initial='',
                 label='Schedule day',
                 help='Schedule day code')
        t.column('launchTime', 'character', format='X(5)', initial='',
                 label='Launch',
                 help='Time to launch')
        t.column('lastTime', 'character', format='X(5)', initial='',
                 label='Last',
                 help='Last time to lauch')
        t.column('interval', 'integer', format='>>>9', initial='0',
                 label='Interval',
                 help='Interval in minutes')
        t.column('queueNumber', 'integer', format='>>9', initial='0',
                 label='Queue number',
                 help='0=any queue,1...999 queues')
        t.column('onlyoneQueued', 'logical', initial='no',
                 label='Only one queued',
                 help='Only one queued job')
        t.column('runType', 'character', initial='',
                 label='Run type',
                 description='Periphal / sequental run')
        t.index('jobgroup', ['jobgroup'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('schedulegroup', ['schedulegroup', 'scheduleDay'], area='Sta_Index_2')

    def down(self):
        self.drop_table('jobgroup')

