from gearbox.migrations import Migration

class AddPersLeave(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PersLeave', area='Sta_Data_256',
                       label='Leaves',
                       dump_name='persleav',
                       desc='Personnel Leave Details')
        t.column('PersCode', 'character', initial='',
                 label='Person',
                 column_label='Person',
                 help='Person code of the person for whom leave will be applicable')
        t.column('FromStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='From',
                 column_label='From',
                 help='Date and time of Leave From',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('ToStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='To',
                 column_label='To',
                 help='Date and time of Leave To',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('CreUser', 'character', initial='',
                 label='Created By',
                 help='User code')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time of holiday creation',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.index('FromStamp', ['FromStamp'], area='Sta_Index_2')
        t.index('perscode', ['PersCode', 'FromStamp'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PersLeave')

