from gearbox.migrations import Migration

class AddForeignRequest(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ForeignRequest', area='Sta_Data_256',
                       dump_name='foreignr')
        t.column('MessageId', 'decimal', decimals=2, format='>>>>>>>>>>>>>>>9', initial='0',
                 label='MessageID',
                 column_label='MessageID',
                 help='Message identification number')
        t.column('TimeStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='TimeStamp',
                 help='Record creation timestamp')
        t.column('ForeignRequest', 'character', initial='',
                 label='Request',
                 column_label='Request',
                 help='Complete request')
        t.column('StatusFlag', 'integer', format='>9', initial='0',
                 label='Status',
                 column_label='Status',
                 help='Status information flag')
        t.column('InTopic', 'character', initial='',
                 column_label='InTopic')
        t.column('OutTopic', 'character', initial='',
                 column_label='OutTopic')
        t.column('Request', 'character', initial='',
                 column_label='Request',
                 help='What was requested ?')
        t.column('ErrClass', 'integer', format='>>9', initial='0',
                 label='Error Class',
                 column_label='Error Class')
        t.column('Severity', 'integer', format='>>9', initial='0',
                 column_label='Severity')
        t.column('ErrNum', 'integer', format='>>9', initial='0',
                 label='Error Number',
                 column_label='Error Number')
        t.index('MessaggeId', ['MessageId'], area='Sta_Index_2',
                primary=True)
        t.index('StatusFlag', ['StatusFlag', 'Request'], area='Sta_Index_2')

    def down(self):
        self.drop_table('ForeignRequest')

