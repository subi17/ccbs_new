from gearbox.migrations import Migration

class Addprogjob(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('progjob', area='Sta_Data_256',
                       label='Program jobs')
        t.column('job', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Job',
                 help='Number of job')
        t.column('applcode', 'character', mandatory=True, initial='',
                 label='Application',
                 help='Application code. Internal use only')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('statuscode', 'integer', format='>9', initial='0',
                 label='Statuscode',
                 help='0=wait,10=work on,20=waiting infromation,99=complete')
        t.column('action', 'character', format='X(20)', initial='',
                 label='Action',
                 help='Action: Planing,Develoment,Testing')
        t.column('description', 'character', format='X(78)', initial='',
                 label='Description')
        t.column('startdate', 'date', format='99.99.99',
                 label='Start',
                 help='Start date')
        t.column('enddate', 'date', format='99.99.99',
                 label='End',
                 help='End date')
        t.column('lastchanged', 'date', format='99.99.99',
                 label='Last changed')
        t.column('lastuser', 'character', initial='',
                 label='Last user')
        t.index('application', ['applcode', 'job'], area='Sta_Index_2',
                unique=True)
        t.index('job', ['job'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('programcode', ['programcode', 'job'], area='Sta_Index_2',
                unique=True)
        t.index('statuscode', ['statuscode', 'programcode'], area='Sta_Index_2')
        t.index('userstatus', ['usercode', 'statuscode', 'programcode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('progjob')

