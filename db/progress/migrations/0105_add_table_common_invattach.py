from gearbox.migrations import Migration

class AddInvAttach(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvAttach', area='Sta_Data_256',
                       label='Invoice Attachments',
                       dump_name='invattac',
                       desc='Attachment definitions for EPL printing of invoices')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('Attach1', 'integer', format='>', initial='0',
                 label='Attachment 1',
                 column_label='Att1')
        t.column('Attach2', 'integer', format='>', initial='0',
                 label='Attachment 2',
                 column_label='Att2')
        t.column('Attach3', 'integer', format='>', initial='0',
                 label='Attachment 3',
                 column_label='Att3')
        t.column('Attach4', 'integer', format='>', initial='0',
                 label='Attachment 4',
                 column_label='Att4')
        t.column('PrintInv', 'integer', format='>>>>>>>9', initial='0',
                 label='Printed to Invoice',
                 column_label='Invoice',
                 help='Invoice to which attachments were printed')
        t.column('PrintStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Print Time',
                 column_label='Time',
                 help='Time when invoice was printed (with attachment data)')
        t.index('CustNum', ['CustNum', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('PrintStamp', [('PrintStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('ToDate', [('ToDate', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')

    def down(self):
        self.drop_table('InvAttach')

