from gearbox.migrations import Migration

class AddHoliday(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Holiday', area='Sta_Data_256',
                       label='Holiday Master Table',
                       dump_name='holiday',
                       desc='Holiday Details')
        t.column('Holiday', 'date', format='99-99-9999',
                 label='Holiday Date',
                 column_label='Date',
                 help='Holiday\'s date')
        t.column('Caption', 'character', format='x(40)', initial='',
                 label='Holiday Caption',
                 column_label='Caption')
        t.column('CreUser', 'character', initial='',
                 label='Created By',
                 help='User code')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time of holiday creation',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Holiday', ['Brand', 'Holiday'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('Holiday')

