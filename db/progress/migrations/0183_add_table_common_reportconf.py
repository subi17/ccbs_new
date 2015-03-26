from gearbox.migrations import Migration

class AddReportConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ReportConf', area='Sta_Data_256',
                       label='Report Configuration',
                       dump_name='reportconf',
                       desc='Report configuration')
        t.column('Brand', 'character', initial='',
                 help='Code of Brand')
        t.column('ReportID', 'character', format='x(12)', initial='',
                 label='Report ID',
                 column_label='ID')
        t.column('ReportName', 'character', format='x(30)', initial='',
                 label='Report Name',
                 column_label='Name',
                 help='Report name')
        t.index('ReportID', ['Brand', 'ReportID'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ReportConf')

