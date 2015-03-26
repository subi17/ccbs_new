from gearbox.migrations import Migration

class AddDFTimeTable(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DFTimeTable', area='Sta_Data_128',
                       label='Dump Time Table',
                       dump_name='dftimetable',
                       desc='Time table for dump files')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('DumpID', 'integer', format='>>>>>>>9', initial='0',
                 label='Dump ID',
                 column_label='ID',
                 help='Unique ID')
        t.column('DumpDay', 'character', format='x(30)', initial='',
                 label='Days',
                 help='Days of the month when file is created')
        t.column('DumpTime', 'character', format='x(20)', initial='',
                 label='Time',
                 help='Time when dump files is created')
        t.column('DumpMode', 'character', initial='',
                 label='Dump Mode',
                 column_label='Mode',
                 help='Dump mode')
        t.column('LastRun', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Last Run',
                 column_label='Last',
                 help='Latest creation of dump file')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('DumpWeekday', 'character', format='x(30)', initial='',
                 label='Weekdays',
                 help='Weekdays when dump file is created')
        t.column('FileNameTag', 'character', format='x(20)', initial='',
                 label='File Name Tag',
                 column_label='Tag',
                 help='Value for tag used in file name')
        t.column('Ongoing', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Ongoing Run Started',
                 column_label='Ongoing',
                 help='Starting time of an ongoing run')
        t.index('DumpID', ['Brand', 'DumpID', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('DFTimeTable')

