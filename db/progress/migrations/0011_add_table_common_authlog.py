from gearbox.migrations import Migration

class AddAuthLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AuthLog', area='Sta_Data_128',
                       label='AuthLog',
                       dump_name='authlog',
                       desc='3rd party user authentication log')
        t.column('Username', 'character', format='x(20)', initial='',
                 column_label='Username')
        t.column('IP', 'character', format='x(16)', initial='',
                 label='IP Address',
                 column_label='IP Address')
        t.column('EndUserID', 'character', format='x(40)', initial='',
                 column_label='EndUserID')
        t.column('UserAgent', 'character', format='x(40)', initial='',
                 column_label='UserAgent')
        t.column('MethodName', 'character', format='x(40)', initial='',
                 column_label='MethodName')
        t.column('ErrorMsg', 'character', format='x(60)', initial='',
                 column_label='ErrorMsg')
        t.column('TimeStamp', 'datetime', format='99-99-9999 HH:MM:SS',
                 column_label='TimeStamp')
        t.index('TimeStamp', [('TimeStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('Username', ['Username'], area='Sta_Index_2')

    def down(self):
        self.drop_table('AuthLog')

