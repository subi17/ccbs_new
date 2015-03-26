from gearbox.migrations import Migration

class AddContact(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Contact', area='Sta_Data_256',
                       label='Contact',
                       dump_name='contact',
                       desc='Customer contact calendar and history\
\
 \
')
        t.column('CustNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.column('UserCode', 'character', initial='',
                 label='User Code',
                 column_label='User',
                 help='User who handled this contact')
        t.column('ConDate', 'date', format='99-99-99',
                 label='Contact Date',
                 column_label='Date',
                 help='Date when contact was taken')
        t.column('ConStamp', 'decimal', decimals=2, format='99999999.99999', initial='0',
                 label='Contact Taken',
                 column_label='Taken',
                 help='Time stamp when contact was taken')
        t.column('ConState', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='Status',
                 help='Status of contact event')
        t.column('CustBal', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Customer\'s Balance',
                 column_label='Balance',
                 help='Customer\'s open balance')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('PlanDate', 'date', format='99-99-99',
                 label='Contact Planned',
                 column_label='Planned',
                 help='Date when contact was planned to be taken')
        t.column('ConType', 'integer', format='9', initial='0',
                 label='Contact Type',
                 column_label='Type',
                 help='Type of contact')
        t.column('ConID', 'integer', format='>>>>>>>>9', initial='0',
                 label='Contact ID',
                 column_label='ID',
                 help='Unique contact ID',
                 description='Sequence ConID')
        t.index('ConID', ['ConID'], area='Sta_Index_2',
                unique=True)
        t.index('ConStamp', ['Brand', ('ConStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('ConType', ['Brand', 'ConType', 'ConState'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('ConStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('UserCode', ['Brand', 'UserCode', 'ConState', ('ConDate', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')
        t.index('UserStamp', ['Brand', 'UserCode', ('ConStamp', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('Contact')

