from gearbox.migrations import Migration

class AddjobQConfig(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('jobQConfig', area='Sta_Data_256',
                       dump_name='jobqconf')
        t.column('queueNumber', 'integer', format='>>9', initial='0',
                 label='Queue number',
                 help='1...999 queues')
        t.column('queuestatus', 'logical', format='Active/Stopped', initial='Active',
                 label='Active')
        t.column('queueType', 'character', initial='',
                 label='Type',
                 help='Queue type')
        t.index('Queue', ['queueNumber'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('jobQConfig')

