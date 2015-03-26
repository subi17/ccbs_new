from gearbox.migrations import Migration

class AddFileExpLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FileExpLog', area='Sta_Data_256',
                       label='Transfer file',
                       dump_name='fileexpl',
                       desc='Transfer file for bookkeeping')
        t.column('TransType', 'character', initial='',
                 label='Type',
                 column_label='Type',
                 help='Type of transfer file')
        t.column('TransNum', 'integer', format='>>>>>>9', initial='0',
                 label='Xfer file',
                 column_label='Xfer file',
                 help='Consecutive number of transfer file (for bookkeeping)')
        t.column('TransDate', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date',
                 help='Date when file was created')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('TransDate', ['Brand', ('TransDate', 'DESCENDING'), ('TransType', 'DESCENDING')], area='Sta_Index_2')
        t.index('TransType', ['Brand', 'TransType', ('TransNum', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('FileExpLog')

